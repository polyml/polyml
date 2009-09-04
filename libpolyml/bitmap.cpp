/*
    Title:  Bitmap.  Generally used by the garbage collector to indicate allocated words

    Copyright (c) 2006  David C.J. Matthews
       Based on original code in garbage_collect.c.

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

/*
   Bitmaps are used particularly in the garbage collector to indicate allocated
   words.  The efficiency of this code is crucial for the speed of the garbage
   collector.
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
#define ASSERT(h) assert(h)
#else
#define ASSERT(h)
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif


#include "bitmap.h"
#include "globals.h"

Bitmap::Bitmap(POLYUNSIGNED bits)
{
    POLYUNSIGNED words = (bits+BITS_PER_WORD-1) / BITS_PER_WORD;
    m_bits = (POLYUNSIGNED *)calloc(words, sizeof(POLYUNSIGNED));
}

bool Bitmap::Create(POLYUNSIGNED bits)
{
    free(m_bits);
    POLYUNSIGNED words = (bits+BITS_PER_WORD-1) / BITS_PER_WORD;
    m_bits = (POLYUNSIGNED *)calloc(words, sizeof(POLYUNSIGNED));
    return m_bits != 0;
}

Bitmap::~Bitmap()
{
    free(m_bits);
}

// Set a range of bits in a bitmap.  This checks that the bits are not already set.
void Bitmap::SetBits(POLYUNSIGNED bitno, POLYUNSIGNED length)
{
    POLYUNSIGNED word_index = bitno / BITS_PER_WORD;
    
    ASSERT (0 < length);
    
    /* Set the first part word */
    POLYUNSIGNED start_bit_index = bitno % BITS_PER_WORD;
    POLYUNSIGNED stop_bit_index  = start_bit_index + length;
    /* Do we need to change more than one word? */
    if (stop_bit_index < BITS_PER_WORD)
    {
        /* N.B.  On some machines shifts of the number of bits per word can mean
           no shift at all. */
        const POLYUNSIGNED mask1 = ((~ (POLYUNSIGNED)0)) << start_bit_index;
        const POLYUNSIGNED mask2 = ((~ (POLYUNSIGNED)0)) << stop_bit_index;
        const POLYUNSIGNED mask  = mask1 & ~mask2;
        
        ASSERT((m_bits[word_index] & mask) == 0);
        m_bits[word_index] |= mask;
        return;
    }
    else /* Set all the bits we can in the first word */
    {
        const POLYUNSIGNED mask  = ((POLYUNSIGNED)(~ 0)) << start_bit_index;
        
        ASSERT((m_bits[word_index] & mask) == 0);
        m_bits[word_index] |= mask;
        
        /* length = length - (BITS_PER_WORD - start_bit_index); */
        length = stop_bit_index - BITS_PER_WORD;
    }
    
    /* Invariant: 0 <= length */
    
    /* Set as many full words as possible */
    while (BITS_PER_WORD <= length)
    {
        /* Invariant: BITS_PER_WORD <= length */
        word_index ++;
        ASSERT(m_bits[word_index] == 0);
        m_bits[word_index] = ~ (POLYUNSIGNED)0;
        length -= BITS_PER_WORD;
        /* Invariant: 0 <= length */
    }
    
    /* Invariant: 0 <= length < BITS_PER_WORD */
    if (length == 0) return;
    
    /* Invariant: 0 < length < BITS_PER_WORD */
    
    /* Set the final part word */
    word_index ++;

    const POLYUNSIGNED mask1 = (~ (POLYUNSIGNED)0) << 0;
    const POLYUNSIGNED mask2 = (~ (POLYUNSIGNED)0) << length;
    const POLYUNSIGNED mask  = mask1 & ~mask2;
        
    ASSERT((m_bits[word_index] & mask) == 0);
    m_bits[word_index] |= mask;
}

void Bitmap::ClearBits(POLYUNSIGNED bitno, POLYUNSIGNED length)
{
    POLYUNSIGNED word_index = bitno / BITS_PER_WORD;
    
    if (length == 0)
        return;
    
    // Clear the first part word
    POLYUNSIGNED start_bit_index = bitno % BITS_PER_WORD;
    POLYUNSIGNED stop_bit_index  = start_bit_index + length;
    /* Do we need to change more than one word? */
    if (stop_bit_index < BITS_PER_WORD)
    {
        const POLYUNSIGNED mask1 = ((~ (POLYUNSIGNED)0)) << start_bit_index;
        const POLYUNSIGNED mask2 = ((~ (POLYUNSIGNED)0)) << stop_bit_index;
        const POLYUNSIGNED mask  = mask1 & ~mask2;
        m_bits[word_index] &= ~mask;
        return;
    }
    else /* Clear all the bits we can in the first word */
    {
        const POLYUNSIGNED mask  = ((POLYUNSIGNED)(~ 0)) << start_bit_index; 
        m_bits[word_index] &= ~mask;
        length = stop_bit_index - BITS_PER_WORD;
    }
    
    /* Invariant: 0 <= length */
    
    // Clear as many full words as possible.
    while (BITS_PER_WORD <= length)
    {
        /* Invariant: BITS_PER_WORD <= length */
        word_index ++;
        m_bits[word_index] = 0;
        length -= BITS_PER_WORD;
        /* Invariant: 0 <= length */
    }
    
    /* Invariant: 0 <= length < BITS_PER_WORD */
    if (length == 0)
        return;
    
    /* Invariant: 0 < length < BITS_PER_WORD */
    
    // Clear the final part word
    word_index ++;

    const POLYUNSIGNED mask1 = (~ (POLYUNSIGNED)0) << 0;
    const POLYUNSIGNED mask2 = (~ (POLYUNSIGNED)0) << length;
    const POLYUNSIGNED mask  = mask1 & ~mask2;
    m_bits[word_index] &= ~mask;
}


// How many zero bits (maximum n) are there in the bitmap, starting at location start? */
POLYUNSIGNED Bitmap::CountZeroBits(POLYUNSIGNED bitno, POLYUNSIGNED n)
{
    POLYUNSIGNED *bitmap = m_bits;
    /* Less naive version */
    POLYUNSIGNED word_index = bitno / BITS_PER_WORD;
    POLYUNSIGNED bit_index  = bitno % BITS_PER_WORD;
    POLYUNSIGNED bits  = bitmap[word_index];
    POLYUNSIGNED mask  = (POLYUNSIGNED)1 << bit_index;
    POLYUNSIGNED zero_bits  = 0;
    ASSERT (0 < n);
    
    /* Check the first part word */
    while (mask != 0)
    {
        /* zero_bits < n */
        if ((bits & mask) != 0) return zero_bits;
        zero_bits ++;
        if (zero_bits == n) return zero_bits;
        mask <<= 1;
        /* zero_bits < n */
    }
    
    /* zero_bits < n */
    
    /* Check as many full words as possible */
    word_index ++;
    bits = bitmap[word_index];
    while (zero_bits < n && bits == 0)
    {
        zero_bits += BITS_PER_WORD;
        word_index ++;
        if (zero_bits < n) // Be careful not to go over the end
            bits = bitmap[word_index];
    }
    
    /* Check the final part word */
    mask = 1;
    while (zero_bits < n && ((bits & mask) == 0))
    {
        zero_bits ++;
        mask <<= 1;
    }
    
    return zero_bits;
}


/* search the bitmap from the high end down looking for n contiguous zeros */
POLYUNSIGNED Bitmap::FindFree
(
  POLYUNSIGNED   limit,  /* The highest numbered bit that's too small to use */
  POLYUNSIGNED   bitno,  /* The lowest numbered bit that's too large to use */
  POLYUNSIGNED   n       /* The number of consecutive zero bits required */
)
{
    if (limit + n >= bitno)
        return 0;

    POLYUNSIGNED candidate = bitno - n;
    ASSERT (bitno > limit);
    
    while (1)
    {
        POLYUNSIGNED bits_free = CountZeroBits(candidate, n);
        
        if (n <= bits_free)
            return candidate;

        if (candidate < n - bits_free + limit)
            return 0;
        
        candidate -= (n - bits_free);
    }
}
