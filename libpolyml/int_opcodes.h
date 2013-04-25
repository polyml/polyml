/*
    Title:  Definitions for the code-tree instructions.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

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

#define INSTR_enter_int     0x00

#if 0
/* for the mips version, we have the following relocations: */

INSTR_enter_int1 0x0d /* mips -> int at start of mips closure */
INSTR_enter_int2 0x91 /* used after int call instruction */
INSTR_return_w   0x92 /* int */

0x90 same as INSTR_local_w /* why? */

#endif

/* For use by the interpreter (only) SPF 26/6/95) */
#define INSTR_tail_0_0      0x01

#define INSTR_jump          0x02
#define INSTR_jump_false    0x03
#define INSTR_container     0x04    /* Added DCJM 5/10/05. */
#define INSTR_del_handler   0x05
#define INSTR_jump_i        0x06
#define INSTR_jump_i_false  0x07
#define INSTR_set_container 0x08    /* Added DCJM 5/10/05. */
#define INSTR_del_handler_i 0x09
#define INSTR_case          0x0a
#define INSTR_call_sl       0x0b
#define INSTR_call_closure  0x0c
#define INSTR_return_w      0x0d
#define INSTR_pad           0x0e
#define INSTR_jump_i_u      0x0f
#define INSTR_raise_ex      0x10
#define INSTR_get_store_w   0x11
#define INSTR_non_local     0x12
#define INSTR_local_w       0x13
#define INSTR_indirect_w    0x14
#define INSTR_move_to_vec_w 0x15
#define INSTR_call_sl_X     0x16    /* Added DCJM 25/9/00. */
#define INSTR_set_stack_val_w   0x17
#define INSTR_reset_w       0x18
#define INSTR_reset_r_w     0x19
#define INSTR_const_addr    0x1a
#define INSTR_const_int_w   0x1b
#define INSTR_io_vec_entry  0x1c
#define INSTR_const_nil     0x1d
#define INSTR_jump_back     0x1e
#define INSTR_return_b      0x1f
#define INSTR_jump_back16   0x20
#define INSTR_get_store_b   0x21
#define INSTR_local_b       0x22
#define INSTR_indirect_b    0x23
#define INSTR_move_to_vec_b 0x24
#define INSTR_set_stack_val_b   0x25
#define INSTR_reset_b       0x26
#define INSTR_reset_r_b     0x27
#define INSTR_const_int_b   0x28
#define INSTR_local_0       0x29
#define INSTR_local_1       0x2a
#define INSTR_local_2       0x2b
#define INSTR_local_3       0x2c
#define INSTR_local_4       0x2d
#define INSTR_local_5       0x2e
#define INSTR_local_6       0x2f
#define INSTR_local_7       0x30
#define INSTR_local_8       0x31
#define INSTR_local_9       0x32
#define INSTR_local_10      0x33
#define INSTR_local_11      0x34
#define INSTR_indirect_0    0x35
#define INSTR_indirect_1    0x36
#define INSTR_indirect_2    0x37
#define INSTR_indirect_3    0x38
#define INSTR_indirect_4    0x39
#define INSTR_indirect_5    0x3a
#define INSTR_const_0       0x3b
#define INSTR_const_1       0x3c
#define INSTR_const_2       0x3d
#define INSTR_const_3       0x3e
#define INSTR_const_4       0x3f
#define INSTR_const_10      0x40
#define INSTR_return_0      0x41
#define INSTR_return_1      0x42
#define INSTR_return_2      0x43
#define INSTR_return_3      0x44
#define INSTR_move_to_vec_0 0x45
#define INSTR_move_to_vec_1 0x46
#define INSTR_move_to_vec_2 0x47
#define INSTR_move_to_vec_3 0x48
#define INSTR_move_to_vec_4 0x49
#define INSTR_move_to_vec_5 0x4a
#define INSTR_move_to_vec_6 0x4b
#define INSTR_move_to_vec_7 0x4c
#define INSTR_const_addr_Xb 0x4d    /* Added DCJM 25/9/00. */
#define INSTR_const_addr_Xw 0x4e    /* Added DCJM 25/9/00. */
#define INSTR_call_sl_cX    0x4f    /* Added DCJM 25/9/00. */
#define INSTR_reset_1       0x50
#define INSTR_reset_2       0x51
#define INSTR_get_store_2   0x52
#define INSTR_get_store_3   0x53
#define INSTR_get_store_4   0x54
#define INSTR_tuple_container 0x55    /* Added DCJM 5/10/05. */
#define INSTR_non_local_l_1 0x56
#define INSTR_non_local_l_2 0x57
#define INSTR_non_local_l_3 0x58
#define INSTR_call_sl_c     0x59
#define INSTR_io_vec_5      0x5a
#define INSTR_io_vec_6      0x5b

#define INSTR_integer_add   0x5c
#define INSTR_integer_minus 0x5d
#define INSTR_integer_equal 0x5e
#define INSTR_integer_leq   0x5f
#define INSTR_integer_greater   0x60
#define INSTR_boolean_or    0x61
#define INSTR_word_equal    0x62
#define INSTR_assign_word   0x63

#define INSTR_reset_r_1     0x64
#define INSTR_reset_r_2     0x65
#define INSTR_reset_r_3     0x66
#define INSTR_tuple_w       0x67
#define INSTR_tuple_b       0x68
#define INSTR_tuple_2       0x69
#define INSTR_tuple_3       0x6a
#define INSTR_tuple_4       0x6b
#define INSTR_lock          0x6c
#define INSTR_ldexc         0x6d
#define INSTR_io_vec_225    0x6e
#define INSTR_io_vec_226    0x6f
#define INSTR_io_vec_229    0x70
#define INSTR_io_vec_233    0x71
#define INSTR_io_vec_236    0x72
#define INSTR_io_vec_251    0x73
#define INSTR_io_vec_253    0x74
#define INSTR_io_vec_255    0x75
#define INSTR_set_handler_old 0x76
#define INSTR_push_handler  0x78
#define INSTR_set_handler_old_i 0x7a
#define INSTR_tail_b_b      0x7b
#define INSTR_tail          0x7c
#define INSTR_tail_3_b      0x7d
#define INSTR_tail_4_b      0x7e
#define INSTR_tail_3_2      0x7f
#define INSTR_tail_3_3      0x80
#define INSTR_set_handler_new 0x81
#define INSTR_set_handler_new_i 0x82