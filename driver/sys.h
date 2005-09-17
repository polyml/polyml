/*
    Title: 	sys.h

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

#ifndef _SYS_H
#define _SYS_H

/* for type word */
#include "globals.h"

/* Poly/ML system interface level */
#define POLY_version_number 401

/*********************************************************************
 * 
 * IO area. 
 *
 *********************************************************************/

extern  word *interface_map[];

/*********************************************************************
 * 
 * Enties in the IO area. 
 *
 *********************************************************************/

#define POLY_SYS_exit                1
#define POLY_SYS_install_root        2
#define POLY_SYS_strconcat           6
#define POLY_SYS_chdir               9
#define POLY_SYS_alloc_store         11
#define POLY_SYS_substring           12	/* Obsolete */
#define POLY_SYS_return              13
#define POLY_SYS_raisex              14
#define POLY_SYS_get_length          15
#define POLY_SYS_get_flags           17
#define POLY_SYS_teststreq           24
#define POLY_SYS_teststrneq          25
#define POLY_SYS_teststrgtr          26
#define POLY_SYS_teststrlss          27
#define POLY_SYS_teststrgeq          28
#define POLY_SYS_teststrleq          29
#define POLY_SYS_exception_trace     30
#define POLY_SYS_commit              40
#define POLY_SYS_set_dbentry         42
#define POLY_SYS_get_dbentry         43
#define POLY_SYS_createf             46
#define POLY_SYS_lockseg             47
#define POLY_SYS_nullorzero          48
#define POLY_SYS_network			 51 /* DCJM 22/5/00 */
#define POLY_SYS_os_specific		 52 /* DCJM 22/5/00 */
#define POLY_SYS_foreign_result		 53 /* DCJM 7/6/01  */

/* Changed from  POLY_SYS_version_number_2 DCJM 13/3/00. */
/* Now redundant.  Replaced by call to process_env. DCJM 26/1/01. */
#define POLY_SYS_version_number		 55   /* MJC 15/09/89 */

/* New IO functions to replace stream functions. */
#define	POLY_SYS_io_dispatch		 61 /* DCJM 8/5/00 */
#define POLY_SYS_signal_handler		 62 /* DCJM 18/7/00 */

/* #define POLY_SYS_version_number_1    80 */

#define POLY_SYS_fork_process        82
#define POLY_SYS_choice_process      83
#define POLY_SYS_kill_self           84
#define POLY_SYS_int_process         85
#define POLY_SYS_send_on_channel     86
#define POLY_SYS_receive_on_channel  87
#define POLY_SYS_profiler            88
#define POLY_SYS_full_gc             92   /* MJC 18/03/91 */
#define POLY_SYS_stack_trace         93   /* MJC 18/03/91 */
#define POLY_SYS_timing_dispatch	 94	  /* DCJM 10/4/00 */
#define POLY_SYS_getdbasetime        98   /* MJC 15/09/89 */
#define POLY_SYS_objsize             99   /* MJC 27/04/88 */
#define POLY_SYS_showsize            100  /* MJC 09/03/89 */

#define POLY_SYS_interrupt_console_processes 103  /* MJC 01/08/90 */

#define POLY_SYS_is_short            105
#define POLY_SYS_aplus               106
#define POLY_SYS_aminus              107
#define POLY_SYS_amul                108
#define POLY_SYS_adiv                109
#define POLY_SYS_amod                110
#define POLY_SYS_aneg                111
#define	POLY_SYS_xora				 112 /* DCJM 21/6/00 */
#define POLY_SYS_equala              113
#define	POLY_SYS_ora				 114 /* DCJM 21/6/00 */
#define	POLY_SYS_anda				 115 /* DCJM 21/6/00 */

/* #define POLY_SYS_version_number_3    116 */

#define POLY_SYS_Real_str			 117 /* DCJM 6/4/00 */
#define POLY_SYS_Real_geq            118 /* DCJM 28/3/00 */
#define POLY_SYS_Real_leq            119 /* DCJM 28/3/00 */
#define POLY_SYS_Real_gtr            120 /* DCJM 28/3/00 */
#define POLY_SYS_Real_lss            121 /* DCJM 28/3/00 */
#define POLY_SYS_Real_eq             122 /* DCJM 28/3/00 */
#define POLY_SYS_Real_neq            123 /* DCJM 28/3/00 */
#define POLY_SYS_Real_Dispatch		 124 /* DCJM 25/3/00 */
#define POLY_SYS_Add_real            125
#define POLY_SYS_Sub_real            126
#define POLY_SYS_Mul_real            127
#define POLY_SYS_Div_real            128
/* #define POLY_SYS_Comp_real           129 */ /* Removed DCJM 28/3/00 */
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
#define POLY_SYS_process_env		 150  /* DCJM 25/4/00 */
#define POLY_SYS_set_string_length	 151  /* DCJM 28/2/01 */
#define POLY_SYS_get_first_long_word 152  /* DCJM 28/2/01 */
#define POLY_SYS_io_operation        189
#define POLY_SYS_set_code_constant	 194  /* DCJM 2/1/01 */
#define POLY_SYS_move_words			 195  /* DCJM 9/10/99 */
#define POLY_SYS_shift_right_arith_word	 196  /* DCJM 9/10/99 */
#define POLY_SYS_int_to_word		 197  /* DCJM 9/10/99 */ /* Now superseded */
#define POLY_SYS_move_bytes			 198  /* DCJM 9/10/99 */
#define POLY_SYS_set_flags           200  /* SPF 13/02/97 */
#define POLY_SYS_shrink_stack        201  /* SPF  9/12/96 */
#define POLY_SYS_stderr              202  /* SPF 29/11/96 */
#define POLY_SYS_callcode_tupled     204  /* SPF 07/07/94 */
#define POLY_SYS_foreign_dispatch    205  /* NIC 22/04/94 */
#define POLY_SYS_install_subshells   206  /* MJC 12/09/90 */
#define POLY_SYS_manage_licences     208  /* SPF 23/04/96 */
#define POLY_SYS_XWindows            209  /* MJC 27/09/90 */

#if 0
/* Only used in LEMMA. */
#define POLY_SYS_check_mutable_res   210  /* added 30/3/95 (DCJM) */
#endif

#define POLY_SYS_is_big_endian       213  /* added 30/3/95 (DCJM) */
#define POLY_SYS_bytes_per_word      214  /* added 30/3/95 (DCJM) */
#define POLY_SYS_offset_address      215
#define POLY_SYS_shift_right_word    216
#define POLY_SYS_word_neq            217
#define POLY_SYS_not_bool            218
#define POLY_SYS_string_length       223  /* Obsolete. */
#define POLY_SYS_int_eq              229
#define POLY_SYS_int_neq             230
#define POLY_SYS_int_geq             231
#define POLY_SYS_int_leq             232
#define POLY_SYS_int_gtr             233
#define POLY_SYS_int_lss             234
#define POLY_SYS_string_sub          235
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
 * Obsolete RTS calls (removed 21/1/97 and 23/2/01)
 *
 *********************************************************************/
#if 0

#define POLY_SYS_printi              3
#define POLY_SYS_printb              4
#define POLY_SYS_prints              5
#define POLY_SYS_convch              8
#define POLY_SYS_convint             7
#define POLY_SYS_getdbaddr           10
#define POLY_SYS_callcode            16
#define POLY_SYS_filedate            18
#define POLY_SYS_gettime             19
#define POLY_SYS_filemode            20
#define POLY_SYS_wconvint            21
#define POLY_SYS_immut_vec_length    22   /* added 30/3/95 (DCJM) */
#define POLY_SYS_lconvint            23
#define POLY_SYS_ptime               31
#define POLY_SYS_openstream          32
#define POLY_SYS_readstream          33
#define POLY_SYS_writestream         34
#define POLY_SYS_closestream         35
#define POLY_SYS_eofstream           36
#define POLY_SYS_repri               37
#define POLY_SYS_reprb               38
#define POLY_SYS_getstore            41
#define POLY_SYS_execstream          44
#define POLY_SYS_flushstream         45
#define POLY_SYS_is_term             49
#define POLY_SYS_can_input           50
#define POLY_SYS_lock_or_copy        51   /* added 30/3/95 (DCJM) */
#define POLY_SYS_wait_until          81 /* Obsolete. */
#define POLY_SYS_raiseexception      89
#define POLY_SYS_printw              90
#define POLY_SYS_reprw               91
#define POLY_SYS_testa               112
#define POLY_SYS_Print_real          131
#define POLY_SYS_repr_time           142
#define POLY_SYS_lookahead           191
#define POLY_SYS_get_environment     199
#define POLY_SYS_get_user_arglist    203
#define POLY_SYS_switch_subshells    207  /* MJC 12/09/90 */
#define POLY_SYS_load_immut_byte     211  /* added 30/3/95 (DCJM) */
#define POLY_SYS_load_immut_word     212  /* added 30/3/95 (DCJM) */
#define POLY_SYS_neg_word            219
#define POLY_SYS_neg_int             220 /* UNSUPPORTED */
#define POLY_SYS_succ_char           221
#define POLY_SYS_pred_char           222
#define POLY_SYS_mul_int             224 /* UNSUPPORTED */
#define POLY_SYS_plus_int            225 /* UNSUPPORTED */
#define POLY_SYS_minus_int           226 /* SUPPORTED!!! */
#define POLY_SYS_div_int             227 /* UNSUPPORTED */
#define POLY_SYS_mod_int             228 /* UNSUPPORTED */
#define POLY_SYS_or_bool             236
#define POLY_SYS_and_bool            237

#endif

/* Other obsolete calls removed DCJM 13/3/00. */
#if (0)
#define POLY_SYS_get_checksum_1      101  /* MJC 15/09/89 */
#define POLY_SYS_get_checksum_2      102  /* MJC 15/09/89 */
#define POLY_SYS_get_hosts           104  /* MJC 15/09/89 */
#endif

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
/* These correspond to indexes into the exception_names[] array. */

#define EXC_interrupt   1 /* SML90.Interrupt */
#define EXC_syserr      2 /* System call failed. */
#define EXC_commit      3
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
#define EXC_io_failure  9  /* No longer used. */
#define EXC_XWindows    10
/* EXC_subscript is raised both by the string subscript function and
   also by substring.  As "Subscript" it is used extensively in the
   Basis library. */
#define EXC_subscript   11 /* General.Subscript */

#define EXC_project     12 /* No longer used. */
#define EXC_stack       13 /* No longer used. */

#define EXC_Prod        14 /* No longer used. */
#define EXC_Quot        15 /* No longer used. */
#define EXC_Sum         16 /* No longer used. */
#define EXC_Diff        17 /* No longer used. */
#define EXC_Neg         18 /* No longer used. */
#define EXC_Sqrt        19 /* No longer used. */
#define EXC_undefined   20   /* reserved - used by ml_compiler/compilerbody */ /* No longer? */
#define EXC_Ln          21 /* No longer used. */
#define EXC_Exp         22 /* No longer used. */

#define EXC_foreign     23  /* nic 4/5/94 */

#endif

