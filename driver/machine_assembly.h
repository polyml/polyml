/*
    Title: 	machine_assembly.h - export signature for machine_assembly.s

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

/* created 27/10/93 SPF */

extern void MD_trap_handler(void);
extern void MD_switch_to_poly_X(void);

#ifdef PORTING
extern void call_MC_code(word, word, word, word);
#endif

#if defined(i386) || defined(INTERPRETED)
/* The Intel architecture seems to share instruction and data cache
   so there's no need to make sure that the instruction cache is
   flushed after a garbage-collection. */
#define MD_flush_instruction_cache(p,n)
#else
/* Use the hand-crafted assembly code (O/S doesn't provide this?!) */
extern void MD_flush_instruction_cache(void *, int);
#endif

/* be more explicit that these entries are functions */
extern int finisha();
extern int install_roota();
extern int strconcata();
extern int change_dira();
extern int substringa();
extern int teststreq();
extern int teststrneq();
extern int teststrgtr();
extern int teststrlss();
extern int teststrgeq();
extern int teststrleq();
extern int commita();
extern int set_dbentry_a();
extern int get_dbentrya();
extern int createfa();
extern int locksega();
extern int profilera();
extern int add_long();
extern int sub_long(); 
extern int mult_long();
extern int div_long();
extern int rem_long();
extern int neg_long();
extern int or_long();
extern int and_long();
extern int xor_long();
extern int equal_long();
extern int Real_stra();
extern int Real_geqa();
extern int Real_leqa();
extern int Real_gtra();
extern int Real_lssa();
extern int Real_eqa();
extern int Real_neqa();
extern int Real_dispatcha();
extern int Real_adda();
extern int Real_suba();
extern int Real_mula();
extern int Real_diva();
extern int Real_nega();
extern int Real_repra();
extern int Real_conva();
extern int Real_inta();
extern int Real_floata();
extern int Real_sqrta();
extern int Real_sina();
extern int Real_cosa();
extern int Real_arctana();
extern int Real_expa();
extern int Real_lna();
extern int io_operation();
extern int shift_right_word();
extern int word_neq();
extern int not_bool();
extern int string_length();
extern int int_eq();
extern int int_neq();
extern int int_geq();
extern int int_leq();
extern int int_gtr();
extern int int_lss();
extern int string_sub();
extern int or_word();
extern int and_word();
extern int xor_word();
extern int shift_left_word();
extern int word_eq();
extern int load_byte();
extern int load_word();
extern int assign_byte();
extern int assign_word();
extern int fork_processa();
extern int choice_processa();
extern int int_processa();
extern int kill_selfa();
extern int send_on_channela();
extern int receive_on_channela();
extern int alloc_store();
extern int get_length_a();
extern int get_flags_a();
extern int set_flags_a();
extern int int_to_word();
extern int set_code_constanta();
extern int move_bytes();
extern int move_words();
extern int shift_right_arith_word();
extern int raisex();
extern int exception_tracea();
extern int return_code();
extern int offset_address();
extern int is_shorta();
extern int is_big_endian();
extern int bytes_per_word();
extern int mul_word();
extern int plus_word();
extern int minus_word();
extern int div_word();
extern int mod_word();
extern int word_geq();
extern int word_leq();
extern int word_gtr();
extern int word_lss();

extern int objsize_a();                     /* MJC 27/04/88 */
extern int showsize_a();                    /* MJC 09/03/89 */
extern int BadOpCode_a();                   /* MJC 15/09/89 */
extern int timing_dispatch_a();             /* DCJM 10/4/00 */
extern int get_dbasetime_a();               /* MJC 15/09/89 */
extern int interrupt_console_processes_a(); /* MJC 01/08/90 */

extern int install_subshells_a();           /* MJC 12/09/90 */

extern int XWindows_a();                    /* MJC 27/09/90 */

extern int full_gc_a();                     /* MJC 18/03/91 */
extern int stack_trace_a();                 /* MJC 18/03/91 */

extern int foreign_dispatch_a();            /* NIC 22/04/94 */
extern int callcode_tupled();               /* SPF 07/07/94 - for ML version of compiler */
extern int process_env_dispatch_a();        /* DCJM 25/4/00 */
extern int set_string_length_a();			/* DCJM 28/2/01 */
extern int get_first_long_word_a();			/* DCJM 28/2/01 */
extern int shrink_stack_a();                /* SPF  1/12/96 */

extern int IO_dispatch_a();					/* DCJM 8/5/00 */
extern int Net_dispatch_a();				/* DCJM 22/5/00 */
extern int OS_spec_dispatch_a();			/* DCJM 22/5/00 */
extern int Sig_dispatch_a();				/* DCJM 18/7/00 */
extern int foreign_result_a();              /* DCJM 7/6/01 */
