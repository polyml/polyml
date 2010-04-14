/*
    Title:      Address scanner

    Copyright (c) 2006-8 David C.J. Matthews

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

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#include "globals.h"
#include "scanaddrs.h"
#include "machine_dep.h"
#include "check_objects.h"
#include "diagnostics.h"

// Process the value at a given location and update it as necessary.
POLYUNSIGNED ScanAddress::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    if (IS_INT(val) || val == PolyWord::FromUnsigned(0))
    {
        // We can get zeros in the constant area if we garbage collect
        //  while compiling some code. */
    }
    else if (val.IsCodePtr())
    {
        // We can get code pointers either in the stack as return addresses or
        // handler pointers or in constants in code segments as the addresses of
        // exception handlers.

        // Find the start of the code segment
        PolyObject *oldObject = ObjCodePtrToPtr(val.AsCodePtr());
        // Calculate the byte offset of this value within the code object.
        POLYUNSIGNED offset = val.AsCodePtr() - (byte*)oldObject;
        // Mustn't use ScanAddressAt here.  That's only valid if the value points
        // into the area being updated.
        PolyObject *newObject = ScanObjectAddress(oldObject);
        *pt = PolyWord::FromCodePtr((byte*)newObject + offset);
    }
    else
    {
        ASSERT(OBJ_IS_DATAPTR(val));
        // Database pointer, local pointer or IO pointer.
        // We need to include IO area pointers when we produce an object module.
        *pt = ScanObjectAddress(val.AsObjPtr());
    }
    return 0;
}

// Process a value within the stack.
PolyWord ScanAddress::ScanStackAddress(PolyWord val, StackObject *base, bool isCode)
{
    PolyWord *end = (PolyWord*)base + base->Length();

    ASSERT(base->ContainsNormalLengthWord());
    ASSERT(base->IsStackObject());

    // If isCode is set we definitely have a code address.  It may have the
    // bottom bit set or it may be word aligned.
    if (isCode || val.IsCodePtr())
    {
        /* Find the start of the code segment */
        PolyObject *oldObject = ObjCodePtrToPtr(val.AsCodePtr());
        // Calculate the byte offset of this value within the code object.
        POLYUNSIGNED offset = val.AsCodePtr() - (byte*)oldObject;
        PolyObject *newObject = ScanObjectAddress(oldObject);
        return PolyWord::FromCodePtr((byte*)newObject + offset);
    }

    else if (val.IsTagged() || (val.AsAddress() > base && val.AsAddress() <= end))
            /* We don't need to process tagged integers (now we've checked it isn't
               a code address) and we don't need to process addresses within the
               current stack.  If the containing stack has moved the stack addresses
               will have been updated by CopyStack. */
            /* N.B. We have "<= end" rather than "< end" because it is possible for
               the stack to be completely empty on a terminated thread. */
           return val;

    else
    {
        ASSERT(val.IsDataPtr());
        return ScanObjectAddress(val.AsObjPtr());
    }
}

// General purpose object processor,  Processes all the addresses in an object.
// Handles the various kinds of object that may contain addresses.
void ScanAddress::ScanAddressesInObject(PolyObject *obj, POLYUNSIGNED lengthWord)
{
    do
    {
        ASSERT (OBJ_IS_LENGTH(lengthWord));
    
        CheckObjectL (obj, lengthWord);
    
        if (OBJ_IS_BYTE_OBJECT(lengthWord))
            return; /* Nothing more to do */
    
        POLYUNSIGNED length = OBJ_OBJECT_LENGTH(lengthWord);
        PolyWord *baseAddr = (PolyWord*)obj;
    
        if (OBJ_IS_STACK_OBJECT(lengthWord))
        {
            StackObject *stack = (StackObject *) obj;
            PolyWord *stackPtr = stack->p_sp; // Save this BEFORE we update
            PolyWord *stackEnd = (PolyWord*)obj + length;
        
            // Either this is TAGGED(0) indicating a retry or it's a code pointer.
            if (stack->p_pc != TAGGED(0).AsCodePtr())
                stack->p_pc = ScanStackAddress (PolyWord::FromCodePtr(stack->p_pc), stack, true).AsCodePtr();

            // Stack pointer and handler pointers
            stack->p_sp =
                ScanStackAddress (PolyWord::FromStackAddr(stack->p_sp), stack, false).AsStackAddr();
            stack->p_hr =
                ScanStackAddress (PolyWord::FromStackAddr(stack->p_hr), stack, false).AsStackAddr();

            // The checked registers.
            for (POLYUNSIGNED i = 0; i < stack->p_nreg; i++)
                stack->p_reg[i] = ScanStackAddress(stack->p_reg[i], stack, false);

            // Now the values on the stack.
            for (PolyWord *q = stackPtr; q < stackEnd; q++)
                *q = ScanStackAddress(*q,stack, false);
            return; // We're done.
        }
        else if (OBJ_IS_CODE_OBJECT(lengthWord))
        {
            // Scan constants within the code.
            machineDependent->ScanConstantsWithinCode(obj, obj, length, this);
        
            // Skip to the constants and get ready to scan them.
            obj->GetConstSegmentForCode(length, baseAddr, length);

        } // else it's a normal object,

        PolyWord *endWord = baseAddr + length;

        // We want to minimise the actual recursion we preform so we try to
        // use tail recursion if we can.  We first scan from the end and
        // remove any words that don't need recursion.
        POLYUNSIGNED lastLengthWord = 0;
        while (endWord != baseAddr)
        {
            PolyWord wordAt = endWord[-1];
            if (IS_INT(wordAt) || wordAt == PolyWord::FromUnsigned(0))
                endWord--; // Don't need to look at this.
            else if ((lastLengthWord = ScanAddressAt(endWord-1)) != 0)
                // We need to process this one
                break;
            else endWord--; // We're not interested in this.
        }

        if (endWord == baseAddr)
            return; // We've done everything.

        // There is at least one word that needs to be processed, the
        // one at endWord-1.
        // Now process from the beginning forward to see if there are
        // any words before this that need to be handled.  This way we are more
        // likely to handle the head of a list by recursion and the
        // tail by looping (tail recursion).
        while (baseAddr < endWord-1)
        {
            PolyWord wordAt = *baseAddr;
            if (IS_INT(wordAt) || wordAt == PolyWord::FromUnsigned(0))
                baseAddr++; // Don't need to look at this.
            else
            {
                POLYUNSIGNED lengthWord = ScanAddressAt(baseAddr);
                if (lengthWord != 0)
                {
                    wordAt = *baseAddr; // Reload because it may have been side-effected
                     // We really have to process this recursively.
                    if (wordAt.IsCodePtr())
                        ScanAddressesInObject(ObjCodePtrToPtr(wordAt.AsCodePtr()), lengthWord);
                    else
                        ScanAddressesInObject(wordAt.AsObjPtr(), lengthWord);
                    baseAddr++;
                }
                else baseAddr++;
            }
        }

        // Finally process the last word we found that has to be processed.
        // Do this by looping rather than recursion.
        PolyWord wordAt = *baseAddr; // Last word to do.
        // This must be an address 
        if (wordAt.IsCodePtr())
            obj = ObjCodePtrToPtr(wordAt.AsCodePtr());
        else
            obj = wordAt.AsObjPtr();

        lengthWord = lastLengthWord;

    } while(1);
}

void ScanAddress::ScanAddressesInRegion(PolyWord *region, PolyWord *end)
{
    PolyWord *pt = region;
    while (pt < end)
    {
        pt++; // Skip length word.
        // pt actually points AT the object here.
        PolyObject *obj = (PolyObject*)pt;
        if (obj->ContainsForwardingPtr())    /* skip over moved object */
        {
            // We can now get multiple forwarding pointers as a result
            // of applying ShareData repeatedly.  Perhaps we should
            // turn the forwarding pointers back into normal words in
            // an extra pass.
            while (obj->ContainsForwardingPtr())
                obj = obj->GetForwardingPtr();
            ASSERT(obj->ContainsNormalLengthWord());
            CheckObject(obj);
            pt += obj->Length();
        }
        else
        {
            ASSERT(obj->ContainsNormalLengthWord());
            POLYUNSIGNED length = obj->Length();
            if (pt+length > end)
                Crash("Malformed object at %p - length %lu\n", pt, length);
            if (length != 0)
                ScanAddressesInObject(obj);
            pt += length;
        }
    }
}

// Extract a constant from the code.
PolyWord ScanAddress::GetConstantValue(byte *addressOfConstant, ScanRelocationKind code)
{
    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 or 64 bit address of target
        {
            POLYUNSIGNED valu;
            unsigned i;
            byte *pt = addressOfConstant;
            if (pt[3] & 0x80) valu = 0-1; else valu = 0;
            for (i = sizeof(PolyWord); i > 0; i--)
                valu = (valu << 8) | pt[i-1];

            /* The old code generator generated reverse subtraction
               of words using a move immediate which loaded a register
               with a the tagged value plus one.  In practice the only
               reverse subtraction of a constant is 0-x so for backwards
               compatibility we need to treat 2 specially. */
            ASSERT(valu != 2);

            return PolyWord::FromUnsigned(valu);
        }
    case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            POLYSIGNED disp;
            byte *pt = addressOfConstant;
            // Get the displacement. This is signed.
            if (pt[3] & 0x80) disp = -1; else disp = 0; // Set the sign just in case.
            for(unsigned i = 4; i > 0; i--) disp = (disp << 8) | pt[i-1];

            byte *absAddr = pt + disp + 4; // The address is relative to AFTER the constant

            return PolyWord::FromCodePtr(absAddr);
        }
    case PROCESS_RELOC_PPCDUAL16SIGNED:       // Power PC - two consecutive words
    case PROCESS_RELOC_PPCDUAL16UNSIGNED:
        {
            // The second word may be a sign-extending "add" instruction or
            // a non-signing extending "or" instruction.
            bool isSigned = code == PROCESS_RELOC_PPCDUAL16SIGNED;
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addressOfConstant;
            // Put together the two halves.
            POLYUNSIGNED hi = pt[0] & 0xffff;
            POLYUNSIGNED lo = pt[1] & 0xffff;
            if (lo >= 32768 && isSigned) hi--; // Correct for sign extension.

            return PolyWord::FromUnsigned((hi << 16) + lo);
        }
    case PROCESS_RELOC_SPARCDUAL:  // Sparc - Sethi and Add instruction pair.
        {
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addressOfConstant;
            // Put together the two halves.
            POLYUNSIGNED valu = (pt[0] << 10) | (pt[1] & 0x3ff);
            return PolyWord::FromUnsigned(valu);
        }
    case PROCESS_RELOC_SPARCRELATIVE: // Call instruction with 30-bit word displacement
        {
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addressOfConstant;
            POLYSIGNED disp = (*pt) & 0x3fffffff;
            // This will work on a 32-bit machine because shifting the 30 bits will set the
            // sign bit.
            return PolyWord::FromStackAddr((PolyWord*)pt + disp);
        }
    default:
        ASSERT(false);
        return TAGGED(0);
    }
}

// Store a constant value.  Also used with a patch table when importing a saved heap which has
// been exported using the C exporter.
void ScanAddress::SetConstantValue(byte *addressOfConstant, PolyWord p, ScanRelocationKind code)
{

    switch (code)
    {
    case PROCESS_RELOC_DIRECT: // 32 or 64 bit address of target
        {
            POLYUNSIGNED valu = p.AsUnsigned();
            for (unsigned i = 0; i < sizeof(PolyWord); i++)
            {
                addressOfConstant[i] = (byte)(valu & 255); 
                valu >>= 8;
            }
        }
        break;
    case PROCESS_RELOC_I386RELATIVE:         // 32 bit relative address
        {
            POLYSIGNED newDisp = p.AsCodePtr() - addressOfConstant - 4;
            for (unsigned i = 0; i < 4; i++) {
                addressOfConstant[i] = (byte)(newDisp & 0xff);
                newDisp >>= 8;
            }
        }
        break;
    case PROCESS_RELOC_PPCDUAL16SIGNED:       // Power PC - two consecutive words
    case PROCESS_RELOC_PPCDUAL16UNSIGNED:
        {
            // The second word may be a sign-extending "add" instruction or
            // a non-signing extending "or" instruction.
            bool isSigned = code == PROCESS_RELOC_PPCDUAL16SIGNED;
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addressOfConstant;
            POLYUNSIGNED hi = p.AsUnsigned() >> 16;
            POLYUNSIGNED lo = p.AsUnsigned() & 0xffff;
            if ((lo & 0x8000) && isSigned) hi++; // Adjust the for sign extension.
            pt[0] = (pt[0] & 0xffff0000) | hi;
            pt[1] = (pt[1] & 0xffff0000) | lo;
        }
        break;
    case PROCESS_RELOC_SPARCDUAL:           // Sparc - SETHI has top 22 bits, ADD has low 10 bits.
        {
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addressOfConstant;
            POLYUNSIGNED valu = p.AsUnsigned();
            pt[0] = (pt[0] & 0xffc00000) | (valu >> 10);
            pt[1] = (pt[1] & 0xfffff000) | (valu & 0x3ff);
        }
        break;
    case PROCESS_RELOC_SPARCRELATIVE: // Call instruction with 30-bit word displacement
        {
            POLYUNSIGNED *pt = (POLYUNSIGNED *)addressOfConstant;
            POLYSIGNED newDisp = p.AsStackAddr() - (PolyWord*)addressOfConstant;
            *pt = (newDisp & 0x3fffffff) | 0x40000000;
        }
        break;
    }
}

// The default action is to call the DEFAULT ScanAddressAt NOT the virtual which means that it calls
// ScanObjectAddress for the base address of the object referred to.
void ScanAddress::ScanConstant(byte *addressOfConstant, ScanRelocationKind code)
{
    PolyWord p = GetConstantValue(addressOfConstant, code);

    if (! IS_INT(p))
    {
        PolyWord oldValue = p;
        ScanAddress::ScanAddressAt(&p);
        if (p != oldValue) // Update it if it has changed.
            SetConstantValue(addressOfConstant, p, code);
    }
}

