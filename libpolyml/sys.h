/*
    Title:  sys.h

    Copyright (c) 2000-7
        Cambridge University Technical Services Limited
    Further development Copyright David C.J. Matthews 2007-12

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

/* This file is included in some of the assembly code files so it must only
   contain preprocessor directives and not C code. */

#ifndef _SYS_H
#define _SYS_H

/*********************************************************************
 * 
 * Entries in the IO area. 
 *
 *********************************************************************/

#define POLY_SYS_exit                1
#define POLY_SYS_chdir               9

#define POLY_SYS_alloc_store         11
#define POLY_SYS_alloc_uninit        12
#define POLY_SYS_raisex              14 /* This is now used only by SetException. */
#define POLY_SYS_get_length          15
#define POLY_SYS_get_flags           17
#define POLY_SYS_str_compare         23 /* DCJM 21/3/06 */
#define POLY_SYS_teststrgtr          26
#define POLY_SYS_teststrlss          27
#define POLY_SYS_teststrgeq          28
#define POLY_SYS_teststrleq          29
#define POLY_SYS_exception_trace     30
#define POLY_SYS_give_ex_trace       31 /* Called from exception unwind code. */
#define POLY_SYS_exception_trace_fn  32
#define POLY_SYS_give_ex_trace_fn    33 // Called to produce the exception trace
#define POLY_SYS_lockseg             47
#define POLY_SYS_emptystring         48 // A value not a function
#define POLY_SYS_nullvector          49 // A value not a function
#define POLY_SYS_network             51 /* DCJM 22/5/00 */
#define POLY_SYS_os_specific         52 /* DCJM 22/5/00 */
#define POLY_SYS_eq_longword                    53
#define POLY_SYS_neq_longword                   54
#define POLY_SYS_geq_longword                   55
#define POLY_SYS_leq_longword                   56
#define POLY_SYS_gt_longword                    57
#define POLY_SYS_lt_longword                    58
#define POLY_SYS_io_dispatch         61 /* DCJM 8/5/00 */
#define POLY_SYS_signal_handler      62 /* DCJM 18/7/00 */
#define POLY_SYS_atomic_reset        69 /* DCJM 18/9/12 */
#define POLY_SYS_atomic_incr         70 /* DCJM 12/3/07 */
#define POLY_SYS_atomic_decr         71 /* DCJM 12/3/07 */
#define POLY_SYS_thread_self         72 /* DCJM 12/3/07 */
#define POLY_SYS_thread_dispatch     73 /* DCJM 12/3/07 */
#define POLY_SYS_plus_longword                  74
#define POLY_SYS_minus_longword                 75
#define POLY_SYS_mul_longword                   76
#define POLY_SYS_div_longword                   77
#define POLY_SYS_mod_longword                   78
#define POLY_SYS_andb_longword                  79
#define POLY_SYS_orb_longword                   80
#define POLY_SYS_xorb_longword                  81
#define POLY_SYS_kill_self           84
#define POLY_SYS_shift_left_longword            85
#define POLY_SYS_shift_right_longword           86
#define POLY_SYS_shift_right_arith_longword     87
#define POLY_SYS_profiler            88
#define POLY_SYS_longword_to_tagged             89
#define POLY_SYS_signed_to_longword             90
#define POLY_SYS_unsigned_to_longword           91
#define POLY_SYS_full_gc             92   /* MJC 18/03/91 */
#define POLY_SYS_stack_trace         93   /* MJC 18/03/91 */
#define POLY_SYS_timing_dispatch     94   /* DCJM 10/4/00 */
#define POLY_SYS_objsize             99   /* MJC 27/04/88 */
#define POLY_SYS_showsize            100  /* MJC 09/03/89 */
#define POLY_SYS_quotrem             104  /* DCJM 05/03/10 */
#define POLY_SYS_is_short            105
#define POLY_SYS_aplus               106
#define POLY_SYS_aminus              107
#define POLY_SYS_amul                108
#define POLY_SYS_adiv                109
#define POLY_SYS_amod                110
#define POLY_SYS_aneg                111
#define POLY_SYS_xora                112 /* DCJM 21/6/00 */
#define POLY_SYS_equala              113
#define POLY_SYS_ora                 114 /* DCJM 21/6/00 */
#define POLY_SYS_anda                115 /* DCJM 21/6/00 */
#define POLY_SYS_Real_str            117 /* DCJM 6/4/00 */
#define POLY_SYS_Real_geq            118 /* DCJM 28/3/00 */
#define POLY_SYS_Real_leq            119 /* DCJM 28/3/00 */
#define POLY_SYS_Real_gtr            120 /* DCJM 28/3/00 */
#define POLY_SYS_Real_lss            121 /* DCJM 28/3/00 */
#define POLY_SYS_Real_eq             122 /* DCJM 28/3/00 */
#define POLY_SYS_Real_neq            123 /* DCJM 28/3/00 */
#define POLY_SYS_Real_Dispatch       124 /* DCJM 25/3/00 */
#define POLY_SYS_Add_real            125
#define POLY_SYS_Sub_real            126
#define POLY_SYS_Mul_real            127
#define POLY_SYS_Div_real            128
#define POLY_SYS_Abs_real            129 // DCJM 26/12/11
#define POLY_SYS_Neg_real            130
#define POLY_SYS_Repr_real           132
#define POLY_SYS_conv_real           133
#define POLY_SYS_real_to_int         134
#define POLY_SYS_int_to_real         135
#define POLY_SYS_sqrt_real           136
#define POLY_SYS_sin_real            137
#define POLY_SYS_cos_real            138
#define POLY_SYS_arctan_real         139
#define POLY_SYS_exp_real            140
#define POLY_SYS_ln_real             141
#define POLY_SYS_stdin               148
#define POLY_SYS_stdout              149
#define POLY_SYS_process_env         150  /* DCJM 25/4/00 */
#define POLY_SYS_set_string_length   151  /* DCJM 28/2/01 */
#define POLY_SYS_get_first_long_word 152  /* DCJM 28/2/01 */
#define POLY_SYS_poly_specific       153  /* DCJM 17/6/06 */
#define POLY_SYS_bytevec_eq          154  /* DCJM 13/1/10 */
#define POLY_SYS_io_operation        189
#define POLY_SYS_move_words_overlap  193
#define POLY_SYS_set_code_constant   194  /* DCJM 2/1/01 */
#define POLY_SYS_move_words          195  /* DCJM 9/10/99 */
#define POLY_SYS_shift_right_arith_word  196  /* DCJM 9/10/99 */
#define POLY_SYS_int_to_word         197  /* DCJM 9/10/99 */ /* Obsolete: Now replaced by POLY_SYS_get_first_long_word. */
#define POLY_SYS_move_bytes          198  /* DCJM 9/10/99 */
#define POLY_SYS_move_bytes_overlap  199
#define POLY_SYS_code_flags          200  /* Previously POLY_SYS_set_flags */
#define POLY_SYS_shrink_stack        201  /* SPF  9/12/96 */
#define POLY_SYS_stderr              202  /* SPF 29/11/96 */
#define POLY_SYS_callcode_tupled     204  /* SPF 07/07/94 */
#define POLY_SYS_foreign_dispatch    205  /* NIC 22/04/94 */
#define POLY_SYS_foreign_null        206  // DCJM 16/11/11
#define POLY_SYS_XWindows            209  /* MJC 27/09/90 */
#define POLY_SYS_is_big_endian       213  /* added 30/3/95 (DCJM) */
#define POLY_SYS_bytes_per_word      214  /* added 30/3/95 (DCJM) */
#define POLY_SYS_offset_address      215
#define POLY_SYS_shift_right_word    216
#define POLY_SYS_word_neq            217
#define POLY_SYS_not_bool            218
#define POLY_SYS_string_length       223
#define POLY_SYS_int_eq              229  // Deprecated - identical to word_eq
#define POLY_SYS_int_neq             230  // Deprecated - identical to word_ne
#define POLY_SYS_int_geq             231
#define POLY_SYS_int_leq             232
#define POLY_SYS_int_gtr             233
#define POLY_SYS_int_lss             234
#define POLY_SYS_load_byte_immut     235
#define POLY_SYS_load_word_immut     236
#define POLY_SYS_mul_word            238  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_plus_word           239  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_minus_word          240  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_div_word            241  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_or_word             242
#define POLY_SYS_and_word            243
#define POLY_SYS_xor_word            244
#define POLY_SYS_shift_left_word     245
#define POLY_SYS_mod_word            246  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_word_geq            247  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_word_leq            248  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_word_gtr            249  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_word_lss            250  /* Reinstated DCJM 9/10/99 */
#define POLY_SYS_word_eq             251
#define POLY_SYS_load_byte           252
#define POLY_SYS_load_word           253
#define POLY_SYS_assign_byte         254
#define POLY_SYS_assign_word         255



/*********************************************************************
 * 
 * The number of entries in the IO area. 
 *
 *********************************************************************/

#define POLY_SYS_vecsize 256


/******************************************************************
 *
 * Exception identifiers 
 *
 ******************************************************************/

#define EXC_interrupt   1 /* SML90.Interrupt */
#define EXC_syserr      2 /* System call failed. */
/* EXC_size (formerly EXC_range) is raised in a number of places,
   most particularly in alloc_store when given a length which is
   too large.  As "Size" it is used extensively in the Basis library. */
#define EXC_size        4 /* General.Size */
#define EXC_overflow    5 /* General.Overflow */
#define EXC_underflow   6 /* This does not correspond to any ML exception. */
#define EXC_divide      7 /* General.Div */
#define EXC_conversion  8
/*
  EXC_conversion is used within the compiler and by conversion routines
  added by the compiler in order to signal failure of conversion.
*/
#define EXC_XWindows    10
/* EXC_subscript is raised both by the string subscript function and
   also by substring.  As "Subscript" it is used extensively in the
   Basis library. */
#define EXC_subscript   11 /* General.Subscript */

#define EXC_thread      12 /* Thread.Thread. */  /* DCJM 13/3/07 */

#define EXC_extrace     13 // Used for exception tracing

#define EXC_foreign     23  /* nic 4/5/94 */

#define EXC_Fail        103  /* DCJM 11/5/06 */
// Make sure to add any additional exceptions to make_exn.

#endif

