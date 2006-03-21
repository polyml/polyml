(*
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
*)


(* These numbers should correspond with those in the sys.h file *)
(* in the run-time system. SPF 17/11/94.                        *)
structure RuntimeCalls =
struct  
  (* 0 not used (unusable?) *)
  val POLY_SYS_exit                = 1
  val POLY_SYS_install_root        = 2
  val POLY_SYS_strconcat           = 6
  val POLY_SYS_chdir               = 9
  val POLY_SYS_alloc_store         = 11
  val POLY_SYS_substring           = 12
  val POLY_SYS_return              = 13
  val POLY_SYS_raisex              = 14
  val POLY_SYS_get_length          = 15
  val POLY_SYS_get_flags           = 17
(* Now obsolete.
  val POLY_SYS_filedate            = 18
  val POLY_SYS_gettime             = 19
  val POLY_SYS_filemode            = 20
*)
  val POLY_SYS_str_compare         = 23 (* DCJM 21/3/06 *)
  val POLY_SYS_teststreq           = 24
  val POLY_SYS_teststrneq          = 25
  val POLY_SYS_teststrgtr          = 26
  val POLY_SYS_teststrlss          = 27
  val POLY_SYS_teststrgeq          = 28
  val POLY_SYS_teststrleq          = 29
  val POLY_SYS_exception_trace     = 30
(* Now obsolete.
  val POLY_SYS_ptime               = 31
  val POLY_SYS_openstream          = 32
  val POLY_SYS_readstream          = 33
  val POLY_SYS_writestream         = 34
  val POLY_SYS_closestream         = 35
  val POLY_SYS_eofstream           = 36
*)
  val POLY_SYS_commit              = 40
(*val POLY_SYS_getstore            = 41*) (* Removed a long time ago. *)
  val POLY_SYS_set_dbentry         = 42
  val POLY_SYS_get_dbentry         = 43
(* Now obsolete.
  val POLY_SYS_execstream          = 44
  val POLY_SYS_flushstream         = 45
*)
  val POLY_SYS_createf             = 46
  val POLY_SYS_lockseg             = 47
  val POLY_SYS_nullorzero          = 48
(* Now obsolete.
  val POLY_SYS_is_term             = 49
  val POLY_SYS_can_input           = 50
*)
  val POLY_SYS_network			   = 51 (* DCJM 22/5/00 *)
  val POLY_SYS_os_specific		   = 52 (* DCJM 22/5/00 *)
  (* 53 - 54 not used *)
(* Now obsolete.
  val POLY_SYS_version_number(*_2*) = 55   (* MJC 15/09/89 *)
*)
  (* 56-60 not used *)
  val POLY_SYS_io_dispatch		    = 61   (* DCJM 8/5/00 *)
  val POLY_SYS_signal_handler		= 62   (* DCJM 18/7/00 *)
  (* 65-77 not used *)
(* Now obsolete
  val POLY_SYS_gettime_milliseconds = 78  (* PANOS 14/02/95 *)
  val POLY_SYS_wait_milliseconds    = 79  (* PANOS 14/02/95 *)
*)
  
(*
  val POLY_SYS_version_number_1    = 80   (* MJC 15/09/89 *)
*)
(* Now obsolete.
  val POLY_SYS_wait_until          = 81 *)
  val POLY_SYS_fork_process        = 82
  val POLY_SYS_choice_process      = 83
  val POLY_SYS_kill_self           = 84
  val POLY_SYS_int_process         = 85
  val POLY_SYS_send_on_channel     = 86
  val POLY_SYS_receive_on_channel  = 87
  val POLY_SYS_profiler            = 88
  val POLY_SYS_full_gc             = 92   (* MJC 18/03/91 *)
  val POLY_SYS_stack_trace         = 93   (* MJC 18/03/91 *)
  val POLY_SYS_timing_dispatch	   = 94	  (* DCJM 10/4/00 *)
(*
  val POLY_SYS_gettime2            = 95   (* MJC 15/09/89 *)
*)
  (* 96 - 97 not used *)
  val POLY_SYS_getdbasetime        = 98   (* MJC 15/09/89 *)
  val POLY_SYS_objsize             = 99   (* MJC 27/04/88 *)
  val POLY_SYS_showsize            = 100  (* MJC 09/03/89 *)
(*
  val POLY_SYS_get_checksum_1      = 101  (* MJC 15/09/89 *)
  val POLY_SYS_get_checksum_2      = 102  (* MJC 15/09/89 *)
*)
  val POLY_SYS_interrupt_console_processes = 103  (* MJC 01/08/90 *)
(*
  val POLY_SYS_get_hosts           = 104  (* MJC 15/09/89 *)
*)
  val POLY_SYS_is_short            = 105
  val POLY_SYS_aplus               = 106
  val POLY_SYS_aminus              = 107
  val POLY_SYS_amul                = 108
  val POLY_SYS_adiv                = 109
  val POLY_SYS_amod                = 110
  val POLY_SYS_aneg                = 111
  val POLY_SYS_xora				   = 112 (* DCJM 21/6/00 *)
  val POLY_SYS_equala              = 113
  val POLY_SYS_ora				   = 114 (* DCJM 21/6/00 *)
  val POLY_SYS_anda				   = 115 (* DCJM 21/6/00 *)
(*
  val POLY_SYS_version_number_3    = 116  (* MJC 15/09/89 *)
*)
  val POLY_SYS_Real_str			   = 117 (* DCJM 6/4/00 *)
  val POLY_SYS_Real_geq            = 118 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_leq            = 119 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_gtr            = 120 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_lss            = 121 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_eq             = 122 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_neq            = 123 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_Dispatch	   = 124  (* DCJM 25/3/00 *)
  val POLY_SYS_Add_real            = 125
  val POLY_SYS_Sub_real            = 126
  val POLY_SYS_Mul_real            = 127
  val POLY_SYS_Div_real            = 128
(*  val POLY_SYS_Comp_real           = 129 *) (* Removed DCJM 28/3/00 *)
  val POLY_SYS_Neg_real            = 130
  val POLY_SYS_Repr_real           = 132
  val POLY_SYS_conv_real           = 133
  val POLY_SYS_real_to_int         = 134
  val POLY_SYS_int_to_real         = 135
  val POLY_SYS_sqrt_real           = 136
  val POLY_SYS_sin_real            = 137
  val POLY_SYS_cos_real            = 138
  val POLY_SYS_arctan_real         = 139
  val POLY_SYS_exp_real            = 140
  val POLY_SYS_ln_real             = 141
(* Now obsolete
  val POLY_SYS_repr_time           = 142 *)
  val POLY_SYS_stdin               = 148
  val POLY_SYS_stdout              = 149
  val POLY_SYS_process_env		   = 150  (* DCJM 25/4/00 *)
  val POLY_SYS_set_string_length   = 151  (* DCJM 28/2/01 *)
  val POLY_SYS_get_first_long_word = 152  (* DCJM 28/2/01 *)
  (* 154 - 188 not used *)
  val POLY_SYS_io_operation        = 189
  (* 190 not used *)
(* Now obsolete
  val POLY_SYS_lookahead           = 191 *)
  (* 191 - 193  not used *)
  val POLY_SYS_set_code_constant   = 194  (* DCJM 2/1/01 *)
  val POLY_SYS_move_words		   = 195  (* DCJM 9/10/99 *)
  val POLY_SYS_shift_right_arith_word	= 196  (* DCJM 9/10/99 *)
  val POLY_SYS_int_to_word		   = 197  (* DCJM 9/10/99 *) (* Obsolete *)
  val POLY_SYS_move_bytes		   = 198  (* DCJM 9/10/99 *)
(* Removed DCJM 25/4/00
  val POLY_SYS_get_environment     = 199  (* SPF 10/11/1998 *)
*)
  val POLY_SYS_set_flags           = 200  (* SPF 10/11/1998 *)
  val POLY_SYS_shrink_stack        = 201  (* SPF 10/11/1998 *)
  val POLY_SYS_stderr              = 202  (* SPF 10/11/1998 *)
(* Removed DCJM 25/4/00
  val POLY_SYS_get_user_arglist    = 203  (* SPF 10/11/1998 *)
*)
  val POLY_SYS_callcode_tupled     = 204  (* SPF 07/07/94 *)
  val POLY_SYS_foreign_dispatch    = 205  (* NIC 22/04/94 *)
  val POLY_SYS_install_subshells   = 206  (* MJC 12/09/90 *)
  val POLY_SYS_switch_subshells    = 207  (* MJC 12/09/90 *)
(*
  val POLY_SYS_manage_licences     = 208  (* SPF 10/11/1998 *)
*)
  val POLY_SYS_XWindows            = 209  (* MJC 27/09/90 *)

(* DCJM's system only ... 
  val POLY_SYS_check_mutable_res   = 210
... *)

  val POLY_SYS_is_big_endian       = 213  (* added 30/3/95 (DCJM) *)
  val POLY_SYS_bytes_per_word      = 214  (* added 30/3/95 (DCJM) *)

  val POLY_SYS_offset_address      = 215
  val POLY_SYS_shift_right_word    = 216
  val POLY_SYS_word_neq            = 217
  val POLY_SYS_not_bool            = 218
  val POLY_SYS_string_length       = 223
  val POLY_SYS_int_eq              = 229
  val POLY_SYS_int_neq             = 230
  val POLY_SYS_int_geq             = 231
  val POLY_SYS_int_leq             = 232
  val POLY_SYS_int_gtr             = 233
  val POLY_SYS_int_lss             = 234
  val POLY_SYS_string_sub          = 235
  val POLY_SYS_mul_word            = 238  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_plus_word           = 239  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_minus_word          = 240  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_div_word            = 241  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_or_word             = 242
  val POLY_SYS_and_word            = 243
  val POLY_SYS_xor_word            = 244
  val POLY_SYS_shift_left_word     = 245
  val POLY_SYS_mod_word            = 246  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_word_geq            = 247  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_word_leq            = 248  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_word_gtr            = 249  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_word_lss            = 250  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_word_eq             = 251
  val POLY_SYS_load_byte           = 252
  val POLY_SYS_load_word           = 253
  val POLY_SYS_assign_byte         = 254
  val POLY_SYS_assign_word         = 255

  (* val POLY_SYS_vecsize = 256 *)

(* Exception identifiers *)
  val EXC_interrupt   = 1
  val EXC_syserr      = 2
  val EXC_commit      = 3
  val EXC_size        = 4
  val EXC_overflow    = 5
  val EXC_underflow   = 6
  val EXC_divide      = 7
  val EXC_conversion  = 8
  (* val EXC_io_failure  = 9 *) (* Being phased out in favour of syscall *)
  val EXC_XWindows    = 10
  val EXC_subscript   = 11
  (* val EXC_project     = 12 *) (* Unused *)
  (* val EXC_stack       = 13 *)  (* Unused *)

  (* val EXC_Prod        = 14 *)  (* Unused *)
  (* val EXC_Quot        = 15 *)  (* Unused *)
  (* val EXC_Sum         = 16 *)  (* Unused *)
  (* val EXC_Diff        = 17 *)  (* Unused *)
  (* val EXC_Neg         = 18 *)  (* Unused *)
  (* val EXC_Sqrt        = 19 *)  (* Unused *)
  (* val EXC_undefined   = 20 *)  (* Unused??*)
  (* val EXC_Ln          = 21 *)  (* Unused *)
  (* val EXC_Exp         = 22 *)  (* Unused *)

  val EXC_foreign     = 23  (* nic 4/5/94 *)
  
  (* The following numbers are hard-coded into the ML compiler
     rather than the run-time system, but they're still hard to change. *)
  val EXC_Bind        = 100; (* SPF 17/11/94 *)
  val EXC_Match       = 101; (* SPF 17/11/94 *)
  (* val EXC_Subscript   = 102; *) (* SPF 17/11/94 *)
  val EXC_Fail		  = 103; (* Added DCJM 23/7/00 *)
end;
