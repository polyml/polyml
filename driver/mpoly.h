/*
    Title: 	exports signature for mpoly.c

	Copyright (c) 2000
		Cambridge University Technical Services Limited

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

/* created 26/10/93 SPF */

#ifndef _MPOLY_H_DEFINED
#define _MPOLY_H_DEFINED

#include "memory.h"

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(attribute) /* attribute */
#endif
#endif

#if	(defined(_MSC_EXTENSIONS) && (_MSC_VER >= 1200))
#define NORETURN	__declspec(noreturn)
#else
#define NORETURN
#endif

extern int user_arg_count;
extern char **user_arg_strings;
extern char *programName;

extern int main();

extern int commitc(void);

extern void stext(void);
extern word *nil_value;

/* Store areas */
extern LocalMemory A;
extern Header      H;

extern word *interface_map[]; 

extern void add_word_to_io_area(int SYS_op, word val);

extern word createfc(
  Handle db_specific_list, 
  Handle root_function,
  Handle child
);


NORETURN extern void finish(int n) __attribute__((noreturn));

/* called from assembly code */
extern word assign_byte_long_c(
  Handle value_handle,
  Handle byte_no,
  Handle vector
);

/* called from assembly code */
extern word assign_word_long_c(
  Handle value_handle,
  Handle word_no,
  Handle vector
);


extern void runtime_assign_word(
  word *pointer,
  word  value
);

extern word move_bytes_long_c(
	Handle len, Handle dest_offset_handle, Handle dest_handle,
	Handle src_offset_handle, Handle src_handle);

extern word move_words_long_c(
	Handle len, Handle dest_offset_handle, Handle dest_handle,
	Handle src_offset_handle, Handle src_handle);

extern StackObject *copy_mapped_stack(StackObject *old);

extern int main (int argc, char **argv);

#endif /* _MPOLY_H_DEFINED */
