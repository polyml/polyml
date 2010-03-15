/*
    Title:      Share common immutable data

    Copyright (c) 2000
        Cambridge University Technical Services Limited
    and David C. J. Matthews 2006

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x)   assert(x)
#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "globals.h"
#include "save_vec.h"
#include "machine_dep.h"
#include "scanaddrs.h"
#include "run_time.h"
#include "sys.h"
#include "gc.h"
#include "rts_module.h"
#include "memmgr.h"
#include "processes.h"

/*
This code was largely written by Simon Finn as a database improver for the the
memory-mapped persistent store version.  The aim is that where two immutable
objects (cells) contain the same data (i.e. where ML equality would say they
were equal) they should be merged so that only a single object is retained.

The basic algorithm works like this:
1. From the root, recursively process all objects and calculate a "depth"
   for each object.  Mutable data, stacks and code segments have depth 0 and
   cannot be merged.  Byte segments (e.g. strings and long-format arbitrary
   precision values) have depth 1.  Other cells have depths of 1 or greater,
   the depth being the maximum recursion depth until a byte segment or an
   object with depth 0 is reached.  Cycles of immutable data don't arise
   normally in ML but could be produced as a result of locking mutable objects.
   To avoid infinite recursion cycles are broken by setting the depth of an
   object to zero before processing it.  The depth of each object is stored
   in the length word of the object.  This ensures each object is processed
   once only.
2. Vectors are created containing objects of the same depth, from 1 to the
   maximum depth found.
3. We begin a loop starting at depth 1.
4. The objects are sorted by their contents so bringing together objects
   with the same contents.  The contents are considered simply as
   uninterpreted bits.  
5. The sorted vector is processed to find those objects that are actually
   bitwise equal.  One object is selected to be retained and its length
   word is restored to be a normal length (phase 1 had set it to be a depth).
   The other objects have their length words turned into tombstones pointing
   at the retained object.
6. Objects at the next depth are first processed to find pointers to objects
   that moved in the previous step (or that step with a lower depth).  The
   addresses are updated to point to the retained object.  The effect of this
   step is to ensure that now two objects that are equal in ML terms have
   identical contents.
   e.g. If we have
      val a = ("abc", "def") and b = ("abc", "def")
   then we will have merged the two occurrences of "abc" and "def" in the
   previous pass of level 1 objects.  This step ensures that the two cells
   containing the pairs both hold pointers to the same objects and so are
   bitwise equal.
7. Repeat with 4, 5 and 6 until all the levels have been processed.

Each object is processed once and at the end most of the objects have been
updated with the shared addresses.  We have to scan all the mutable and
code objects to update the addresses but also have to scan the immutables
because of the possibility of missing an update as a result of breaking a
loop (see SPF's comment below).
DCJM 3/8/06
*/

/******************************************************************************/
/*                                                                            */
/*      Data objects                                                          */
/*                                                                            */
/******************************************************************************/
/* Meaning of length word:
  (1) bit 31 (msbit) = 0, bits 0-23 = length, bits 24-30 = flags
  (2) bit 31 = 1, bit 30 = 1, bits 0-30 = depth
  (2) bit 31 = 1, bit 30 = 0, bits 0-30 = forwarding pointer>>2
*/

typedef struct
{
    POLYUNSIGNED    L;
    PolyObject *pt;
} Item;

// The DepthVector type contains all the items of a particular depth.
typedef struct
{
    POLYUNSIGNED    depth;
    POLYUNSIGNED    nitems;
    POLYUNSIGNED    vsize;
    Item            *vector;
} DepthVector;
/*
class Vector
{
public:
    static Vector *AddDepth(POLYUNSIGNED depth);

    Vector(POLYUNSIGNED d): depth(d), nitems(0), vsize(0), vector(0), next(0) {}
    ~Vector() { free(vector); }

    POLYUNSIGNED    depth;
    POLYUNSIGNED    nitems;
    POLYUNSIGNED    vsize;
    Item            *vector;
    Vector          *next;
};*/

static DepthVector *depthVectors = 0;
static POLYUNSIGNED depthVectorSize = 0;

// Returns true if this is an address into either the permanent or temporary areas.
inline bool IsDataAddress(PolyWord p)
{
    MemSpace *space = gMem.SpaceForAddress(p.AsAddress());
    return space != 0 && space->spaceType != ST_IO;
}


DepthVector *AddDepth(POLYUNSIGNED depth)
{
    if (depth >= depthVectorSize) {
        POLYUNSIGNED newDepth = depth+1;
        DepthVector *newVec = (DepthVector *)realloc(depthVectors, sizeof(DepthVector)*newDepth);
        if (newVec == 0) throw MemoryException();
        depthVectors = newVec;
        for (POLYUNSIGNED d = depthVectorSize; d < newDepth; d++) {
            DepthVector *dv = &depthVectors[d];
            dv->depth = d;
            dv->nitems = dv->vsize = 0;
            dv->vector = 0;
        }
        depthVectorSize = newDepth;
    }
    return &depthVectors[depth];
}

/******************************************************************************/
/*                                                                            */
/*      AddToVector                                                           */
/*                                                                            */
/******************************************************************************/
static void AddToVector(POLYUNSIGNED depth, POLYUNSIGNED L, PolyObject *pt)
{
    DepthVector *v = AddDepth (depth);
    
    ASSERT (v->nitems <= v->vsize);
    
    if (v->nitems == v->vsize)
    {
        unsigned long new_vsize  = 2 * v->vsize + 1;
        if (new_vsize < 15)
            new_vsize = 15;

        Item *new_vector = (Item *)realloc (v->vector, new_vsize*sizeof(Item));

        if (new_vector == 0)
            throw MemoryException();
        
        v->vector = new_vector;
        v->vsize  = new_vsize;
    }
    
    ASSERT (v->nitems < v->vsize);
    
    v->vector[v->nitems].L  = L;
    v->vector[v->nitems].pt = pt;
    
    v->nitems++;
    
    ASSERT (v->nitems <= v->vsize);
}

/******************************************************************************/
/*                                                                            */
/*      CompareItems                                                          */
/*                                                                            */
/******************************************************************************/
static int CompareItems(const void *arg_a, const void *arg_b)
{
    Item *a = (Item *)arg_a;
    Item *b = (Item *)arg_b;
 
    PolyObject *x = a->pt;
    PolyObject *y = b->pt;
    POLYUNSIGNED  A = x->LengthWord();
    POLYUNSIGNED  B = y->LengthWord();
    
    ASSERT (OBJ_IS_DEPTH(A));
    ASSERT (OBJ_IS_DEPTH(B));
    ASSERT (A == B); // Should be the same depth.
    
    ASSERT (OBJ_IS_LENGTH(a->L));
    ASSERT (OBJ_IS_LENGTH(b->L));
    
    if (a->L > b->L) return  1;
    if (a->L < b->L) return -1;
    
    // Return simple bitwise equality.
    return memcmp(x, y, OBJ_OBJECT_LENGTH(a->L)*sizeof(PolyWord));
}

/******************************************************************************/
/*                                                                            */
/*      MergeSameItems                                                        */
/*                                                                            */
/******************************************************************************/
static POLYUNSIGNED MergeSameItems(DepthVector *v)
{
    POLYUNSIGNED  N = v->nitems;
    Item *itemVec = v->vector;
    POLYUNSIGNED  n = 0;
    POLYUNSIGNED  i = 0;
    
    while (i < N)
    {
        ASSERT (OBJ_IS_DEPTH(itemVec[i].pt->LengthWord()));
        PolyObject *toShare = NULL; // Candidate to share.
        unsigned minHierarchy = 0;

        POLYUNSIGNED j;
        for (j = i; j < N; j++)
        {
            // Search for identical objects.  Don't bother to compare it with itself.
            if (i != j && CompareItems (& itemVec[i], & itemVec[j]) != 0) break;
            // Choose an object in the permanent memory if that is available.
            // This is necessary to retain the invariant that no object in
            // the permanent memory points to an object in the temporary heap.
            // (There may well be pointers to this object elsewhere in the permanent
            // heap).
            // Choose the lowest hierarchy value for preference since that
            // may reduce the size of saved state when resaving already saved
            // data.
            MemSpace *space = gMem.SpaceForAddress(itemVec[j].pt);
            if (space->spaceType == ST_PERMANENT)
            {
                PermanentMemSpace *pSpace = (PermanentMemSpace *)space;
                if (toShare == NULL || pSpace->hierarchy < minHierarchy)
                {
                    toShare = itemVec[j].pt;
                    minHierarchy = pSpace->hierarchy;
                }
            }
        }
        // If there isn't a permanent object choose the first in the table.
        if (toShare == NULL) toShare = itemVec[i].pt;
        POLYUNSIGNED k = j; // Remember the first object that didn't match.
        //.For each identical object set all but the one we want to point to
        // the shared object.
        for (j = i; j < k; j++)
        {
            ASSERT (OBJ_IS_DEPTH(itemVec[j].pt->LengthWord()));
            if (itemVec[j].pt == toShare)
            {
                // This is the common object.
                toShare->SetLengthWord(itemVec[j].L); // restore genuine length word
                ASSERT (OBJ_IS_LENGTH(toShare->LengthWord()));
            }
            else
            {
                itemVec[j].pt->SetForwardingPtr(toShare); /* an indirection */
                ASSERT (itemVec[j].pt->ContainsForwardingPtr());
            }
            n++;
        }
        ASSERT(! OBJ_IS_DEPTH(itemVec[i].pt->LengthWord()));
        i = k;
    }
    
    return n;
}

class ProcessFixupAddress: public ScanAddress
{
public:
    void FixupItems (DepthVector *v);
protected:
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt);
    virtual PolyObject *ScanObjectAddress(PolyObject *base)
        { return GetNewAddress(base).AsObjPtr(); }
    PolyWord GetNewAddress(PolyWord old);
};

POLYUNSIGNED ProcessFixupAddress::ScanAddressAt(PolyWord *pt)
{
    *pt = GetNewAddress(*pt);
    // If we are updating an immutable object in the permanent memory we mustn't
    // have it contain an address in the temporary heap.  Mutables are ok since the
    // garbage collector always scans that area.
//    ASSERT(!(H->i_space.bottom <= pt && pt < H->i_space.pointer) || (*pt).IsTagged() ||
//            gMem.IsPermanentMemoryPointer((*pt).AsAddress()) || gMem.IsIOPointer((*pt).AsAddress()));
    return 0;
}

// Returns the new address if the argument is the address of an object that
// has moved, otherwise returns the original.
PolyWord ProcessFixupAddress::GetNewAddress(PolyWord old)
{
    if (old.IsTagged() || old == PolyWord::FromUnsigned(0) || gMem.IsIOPointer(old.AsAddress()))
        return old; //  Nothing to do.

    // When we are updating addresses in the stack or in code segments we may have
    // code pointers.
    if (old.IsCodePtr())
    {
        // Find the start of the code segment
        PolyObject *oldObject = ObjCodePtrToPtr(old.AsCodePtr());
        // Calculate the byte offset of this value within the code object.
        POLYUNSIGNED offset = old.AsCodePtr() - (byte*)oldObject;
        PolyWord newObject = GetNewAddress(oldObject);
        return PolyWord::FromCodePtr(newObject.AsCodePtr() + offset);
    }

    ASSERT(old.IsDataPtr());
         
    ASSERT(IsDataAddress(old));

    PolyObject *obj = old.AsObjPtr();
    POLYUNSIGNED L = obj->LengthWord();

    // Generally each address will point to an object processed at a lower depth.
    // The exception is if we have a cycle and have assigned the rest of the
    // structure to a higher depth.
    // N.B. We return the original address here but this could actually share
    // with something else and not be retained.
    if (OBJ_IS_DEPTH(L))
        return old; 
    
    if (obj->ContainsForwardingPtr()) // tombstone is a pointer to a shared object
    {
        PolyObject *newp = obj->GetForwardingPtr();
        ASSERT (newp->ContainsNormalLengthWord());
        return newp;
    }
    
    ASSERT (obj->ContainsNormalLengthWord()); // object is not shared
    return old;
}

void ProcessFixupAddress::FixupItems (DepthVector *v)
{
    POLYUNSIGNED  N = v->nitems;
    Item *V = v->vector;
    for (POLYUNSIGNED i = 0; i < N; i++)
    {
        ScanAddressesInObject(V[i].pt, V[i].L);
    }
}

class ProcessAddToVector: public ScanAddress
{
public:
    ProcessAddToVector() {}
    virtual POLYUNSIGNED ScanAddressAt(PolyWord *pt)
        { (void)AddObjectsToDepthVectors(*pt); return 0; }
    virtual PolyObject *ScanObjectAddress(PolyObject *base)
        { (void)AddObjectsToDepthVectors(base); return base; }
protected:
    POLYUNSIGNED AddObjectsToDepthVectors(PolyWord old);
};

POLYUNSIGNED ProcessAddToVector::AddObjectsToDepthVectors(PolyWord old)
{
    // If this is a tagged integer or an IO pointer that's simply a constant.
    if (old.IsTagged() || old == PolyWord::FromUnsigned(0) || gMem.IsIOPointer(old.AsAddress()))
        return 0;

    ASSERT(IsDataAddress(old));
    PolyObject *obj = old.AsObjPtr();
    POLYUNSIGNED L = obj->LengthWord();

    if (OBJ_IS_DEPTH(L)) // tombstone contains genuine depth or 0.
        return OBJ_GET_DEPTH(L);

    ASSERT (OBJ_IS_LENGTH(L));

    // Byte objects always have depth 1 and can't contain addresses.
    if (obj->IsByteObject())
    {
        obj->SetLengthWord(OBJ_SET_DEPTH(1));
        AddToVector (1, L, old.AsObjPtr());// add to vector at correct depth
        return 1;
    }

    // set initial depth to 0 (to cope with loops)
    obj->SetLengthWord(OBJ_SET_DEPTH(0));

    /* There's a problem sharing code objects if they have relative calls/jumps
       in them to other code.  The code of two functions may be identical (e.g.
       they both call functions 100 bytes ahead) and so they will appear the
       same but if the functions they jump to are different they are actually
       different.  For that reason we don't share code segments.  DCJM 4/1/01 */
    if (OBJ_IS_MUTABLE_OBJECT(L) || OBJ_IS_STACK_OBJECT(L) || OBJ_IS_CODE_OBJECT(L))
    {
        // These always have depth 0.  We still have to process any addresses within them.
        ScanAddressesInObject(obj, L);
        // Add these at depth zero.  This allows us to restore the length words.
        AddToVector (0, L, old.AsObjPtr());
        return 0;
    }

    ASSERT(OBJ_IS_WORD_OBJECT(L)); // That leaves immutable data objects.
    POLYUNSIGNED depth = 0;
    POLYUNSIGNED n  = OBJ_OBJECT_LENGTH(L);
    PolyWord     *pt = (PolyWord*)obj;
    // Process all the values in this object and calculate the maximum depth.
    for(POLYUNSIGNED i = 0; i < n; i++)
    {
        POLYUNSIGNED d = AddObjectsToDepthVectors(*pt);
        if (d > depth) depth = d;
        pt++;
    }

    depth++; // Plus one for this object.
    obj->SetLengthWord(OBJ_SET_DEPTH(depth));// set genuine depth.
    AddToVector (depth, L, old.AsObjPtr());// add to vector at correct depth
    return depth;
}

static void RestoreLengthWords(DepthVector *vec)
{
   // Restore the length words.
    Item *itemVec = vec->vector;
    for (POLYUNSIGNED  i = 0; i < vec->nitems; i++)
    {
        itemVec[i].pt->SetLengthWord(itemVec[i].L); // restore genuine length word
        ASSERT (OBJ_IS_LENGTH(itemVec[i].pt->LengthWord()));
    }
}


static unsigned verbose = 0;


// This is called by the root thread to do the work.
bool RunShareData(PolyObject *root)
{
    POLYUNSIGNED totalObjects = 0;
    POLYUNSIGNED totalShared  = 0;
    
    depthVectors = 0;

    // Build the vectors from the immutable objects.
    ProcessAddToVector addToVector;
    try {
        (void)addToVector.ScanObjectAddress(root);
    }
    catch (MemoryException)
    {
        // If we run out of memory and raise an exception we have to clean up.
        while (depthVectorSize > 0)
        {
            depthVectorSize--;
            free(depthVectors[depthVectorSize].vector);
        }
        free(depthVectors);
        depthVectors = 0;
        return false; // Run out of memory.
    }

    ProcessFixupAddress fixup;

    for (POLYUNSIGNED depth = 1; depth < depthVectorSize; depth++)
    {
        DepthVector *vec = &depthVectors[depth];
        fixup.FixupItems(vec);
        qsort (vec->vector, vec->nitems, sizeof(Item), CompareItems);
    
        POLYUNSIGNED n = MergeSameItems (vec);
    
        if (n && verbose)
        {
            printf("Level %4lu, Objects %6lu, Shared %6lu\n", vec->depth, vec->nitems, n);
        }
    
        totalObjects += vec->nitems;
        totalShared  += n;
    }  

      /* 
       At this stage, we have fixed up most but not all of the forwarding
       pointers. The ones that we haven't fixed up arise from situations
       such as the following:
   
               X -> Y <-> Z
           
       i.e. Y and Z form a loop, and X is isomorphic to Z. When we assigned
       the depths, we have to arbitrarily break the loop between Y and Z.
       Suppose Y is assigned to level 1, and Z is assigned to level 2.
       When we process level 1 and fixup Y, there's nothing to do, since
       Z is still an ordinary object. However when we process level 2, 
       we find that X and Z are isomorphic so we arbitrarily choose one
       of them and turn it into a "tombstone" pointing at the other. If
       we change Z into the tombstone, then Y now contains a pointer
       that needs fixing up. That's why we need the second fixup pass.
   
       Note also that if we had broken the loop the other way, we would have
       assigned Z to level 1, Y to level 2 and X to level 3, so we would
       have missed the chance to share Z and X. Perhaps that's why running
       the program repeatedly sometimes finds extra things to share? 

      SPF 26/1/95
    */

    /* We have updated the addresses in objects with non-zero level so they point to
       the single occurrence but we need to do the same with level 0 objects
       (mutables, stacks and code). */
    if (depthVectorSize > 0)
    {
        DepthVector *v = &depthVectors[0];
        RestoreLengthWords(v);
        fixup.FixupItems(v);
        free(v->vector);
    }

    /* Previously we made a complete scan over the memory updating any addresses so
       that if we have shared two substructures within our root we would also
       share any external pointers.  This has been removed but we have to
       reinstate the length words we've overwritten with forwarding pointers because
       there may be references to unshared objects from outside. */
    for (POLYUNSIGNED d = 1; d < depthVectorSize; d++)
    {
        DepthVector *v = &depthVectors[d];
        RestoreLengthWords(v);
        free(v->vector);
    }

    if (verbose)
    {
        printf ("Total Objects %6lu, Total Shared %6lu\n", totalObjects, totalShared);
        fflush(stdout); /* We need this for Windows at least. */
    }
    return true; // Succeeded.
}

class ShareRequest: public MainThreadRequest
{
public:
    ShareRequest(Handle root): MainThreadRequest(MTP_SHARING), shareRoot(root), result(false) {}

    virtual void Perform() { result = RunShareData(shareRoot->WordP()); }
    Handle shareRoot;
    bool result;
};


// ShareData.  This is the main entry point.
// Because this can recurse deeply it needs to be run by the main thread.
// Also it manipulates the heap in ways that could mess up other threads
// so we need to stop them before executing this.
void ShareData(TaskData *taskData, Handle root)
{
    if (! root->Word().IsDataPtr())
        return; // Nothing to do.  We could do handle a code pointer but it shouldn't occur.

    // Request a full GC  to reduce the size of fix-ups.
    FullGC(taskData);

    // Request the main thread to do the sharing.
    ShareRequest request(root);
    processes->MakeRootRequest(taskData, &request);

    // Raise an exception if it failed.
    if (! request.result)
        raise_exception_string(taskData, EXC_Fail, "Insufficient memory");
}
