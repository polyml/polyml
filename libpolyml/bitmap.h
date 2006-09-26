/*
    Title:  Bitmap.  Generally used by the garbage collector to indicate allocated words

    Copyright (c) 2006  David C.J. Matthews

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

#ifndef BITMAP_H_DEFINED
#define BITMAP_H_DEFINED

#include "globals.h" // For POLYUNSIGNED

/* bitmaps using arrays of word */

#define BITS_PER_BYTE 8
#define BITS_PER_WORD (BITS_PER_BYTE * sizeof(POLYUNSIGNED))

class Bitmap
{
public:
    Bitmap(): m_bits(0) {}
    Bitmap(POLYUNSIGNED bits);
    ~Bitmap();

    bool Create(POLYUNSIGNED bits);

private:
    static POLYUNSIGNED BitN(POLYUNSIGNED n) { return (POLYUNSIGNED)1 << (n & (BITS_PER_WORD-1)); }
public:
    // Set a single bit
    void SetBit(POLYUNSIGNED n) { m_bits[n/BITS_PER_WORD] |=  BitN(n); }
    // Set a range of bits
    void SetBits(POLYUNSIGNED bitno, POLYUNSIGNED length);
    // Clear a bit
    void ClearBit(POLYUNSIGNED n) { m_bits[n/BITS_PER_WORD] &= ~BitN(n); }
    // Clear a range of bits.  May already be partly clear
    void ClearBits(POLYUNSIGNED bitno, POLYUNSIGNED length);
    // Test a bit
    bool TestBit(POLYUNSIGNED n) const { return (m_bits[n/BITS_PER_WORD] & BitN(n)) != 0; }
    // How many zero bits (maximum n) are there in the bitmap, starting at location start?
    POLYUNSIGNED CountZeroBits(POLYUNSIGNED bitno, POLYUNSIGNED n);
    //* search the bitmap from the high end down looking for n contiguous zeros
    POLYUNSIGNED FindFree(POLYUNSIGNED limit, POLYUNSIGNED bitno, POLYUNSIGNED n);
private:

    POLYUNSIGNED *m_bits;
};

#endif


