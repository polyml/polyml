/*
    Title:  poly_specific.cpp - Poly/ML specific RTS calls.

    Copyright (c) 2006 David C. J. Matthews

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

/* This module is used for various run-time calls that are either in the
   PolyML structure or otherwise specific to Poly/ML. */

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#include "globals.h"
#include "poly_specific.h"
#include "arb.h"
#include "mpoly.h"
#include "sys.h"
#include "machine_dep.h"
#include "polystring.h"
#include "run_time.h"
#include "version.h"
#include "save_vec.h"
#include "exporter.h"
#include "version.h"
#include "sharedata.h"
#include "objsize.h"
#include "memmgr.h"
#include "processes.h"
#include "savestate.h"
#include "statistics.h"
#include "../polystatistics.h"

#define SAVE(x) taskData->saveVec.push(x)

static const char *poly_runtime_system_copyright =
"Copyright (c) 2002-10 CUTS, David C.J. Matthews and contributors.";

// Property bits for functions.  For compiled functions these are
// stored in the register mask word.  None of the architectures has
// more than 20 usable registers so we're safe putting them in these bits.
// N.B. These are untagged values.
// NORAISE means the function does not raise an exception.
// NOUPDATE means the function does not perform any side-effects so if
// its value is not used and it doesn't raise an exception it can be eliminated.
// NODEREF means the value of the function depends only on the value of its arguments
// and not on any other state.
#define PROPWORD_NORAISE    0x40000000
#define PROPWORD_NOUPDATE   0x20000000
#define PROPWORD_NODEREF    0x10000000

static POLYUNSIGNED rtsProperties(TaskData *taskData, int i)
{
    switch (i) {
    case POLY_SYS_exit: return 0;
    case POLY_SYS_chdir: return 0;
    case POLY_SYS_alloc_store:
    case POLY_SYS_alloc_uninit:
        // Allocating memory doesn't have any visible effect on the state.  It can raise Size.
        // Two allocations return different addresses.
        return PROPWORD_NOUPDATE;
    case POLY_SYS_raisex: return 0;
    case POLY_SYS_get_length: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_get_flags:
        // This isn't quite true.  It is possible to clear the mutable flag on a mutable segment.
        return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_str_compare: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_teststreq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_teststrneq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_teststrgtr: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_teststrlss: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_teststrgeq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_teststrleq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_exception_trace: return 0;
    case POLY_SYS_give_ex_trace: return 0;
    case POLY_SYS_lockseg: return PROPWORD_NORAISE|PROPWORD_NODEREF;
    case POLY_SYS_emptystring: return 0; // Not a function
    case POLY_SYS_nullvector: return 0; // Not a function
    case POLY_SYS_network: return 0;
    case POLY_SYS_os_specific: return 0;
    case POLY_SYS_io_dispatch: return 0;
    case POLY_SYS_signal_handler: return 0;
    case POLY_SYS_atomic_incr: return PROPWORD_NORAISE;
    case POLY_SYS_atomic_decr: return PROPWORD_NORAISE;
    case POLY_SYS_thread_self: return PROPWORD_NORAISE|PROPWORD_NOUPDATE;
    case POLY_SYS_thread_dispatch: return 0;
    case POLY_SYS_kill_self: return 0;
    case POLY_SYS_profiler: return 0;
    case POLY_SYS_full_gc: return PROPWORD_NORAISE; // Effectively has a side-effect
    case POLY_SYS_stack_trace: return 0;
    case POLY_SYS_timing_dispatch: return 0;
    case POLY_SYS_objsize: return PROPWORD_NORAISE;
    case POLY_SYS_showsize: return PROPWORD_NORAISE;
    case POLY_SYS_quotrem: return PROPWORD_NOUPDATE|PROPWORD_NODEREF; // Can raise Divide
    case POLY_SYS_is_short: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_aplus: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_aminus: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_amul: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_adiv: return PROPWORD_NOUPDATE|PROPWORD_NODEREF; // Can raise Divide
    case POLY_SYS_amod: return PROPWORD_NOUPDATE|PROPWORD_NODEREF; // Can raise Divide
    case POLY_SYS_aneg: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_xora: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_equala: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_ora: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_anda: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_str: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_geq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_leq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_gtr: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_lss: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_eq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_neq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Real_Dispatch: return 0;
    case POLY_SYS_Add_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Sub_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Mul_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Div_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Abs_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Neg_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_Repr_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_conv_real: return PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_real_to_int: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_to_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_sqrt_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_sin_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_cos_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_arctan_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_exp_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_ln_real: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_stdin: return 0; // Not a function
    case POLY_SYS_stdout: return 0; // Not a function
    case POLY_SYS_process_env: return 0;
    case POLY_SYS_set_string_length: return PROPWORD_NORAISE|PROPWORD_NODEREF;
    case POLY_SYS_get_first_long_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_poly_specific: return 0;
    case POLY_SYS_bytevec_eq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_io_operation: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_set_code_constant: return 0;
    case POLY_SYS_move_words: return PROPWORD_NORAISE;
    case POLY_SYS_shift_right_arith_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_to_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_move_bytes: return PROPWORD_NORAISE;
    case POLY_SYS_code_flags: return 0;
    case POLY_SYS_shrink_stack: return 0;
    case POLY_SYS_stderr: return 0; // Not a function
    case POLY_SYS_callcode_tupled: return 0;
    case POLY_SYS_foreign_dispatch: return 0;
    case POLY_SYS_XWindows: return 0;
    case POLY_SYS_is_big_endian: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_bytes_per_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_offset_address: return 0;
    case POLY_SYS_shift_right_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_word_neq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_not_bool: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_string_length: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_eq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_neq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_geq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_leq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_gtr: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_int_lss: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_mul_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_plus_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_minus_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_div_word: return PROPWORD_NOUPDATE|PROPWORD_NODEREF; // Can raise Divide
    case POLY_SYS_or_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_and_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_xor_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_shift_left_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_mod_word: return PROPWORD_NOUPDATE|PROPWORD_NODEREF; // Can raise Divide
    case POLY_SYS_word_geq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_word_leq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_word_gtr: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_word_lss: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_word_eq: return PROPWORD_NORAISE|PROPWORD_NOUPDATE|PROPWORD_NODEREF;
    case POLY_SYS_load_byte: return PROPWORD_NORAISE|PROPWORD_NOUPDATE;
    case POLY_SYS_load_word: return PROPWORD_NORAISE|PROPWORD_NOUPDATE;
    case POLY_SYS_assign_byte: return PROPWORD_NORAISE|PROPWORD_NODEREF;
    case POLY_SYS_assign_word: return PROPWORD_NORAISE|PROPWORD_NODEREF;
    default: raise_exception_string(taskData, EXC_Fail, "Unknown IO operation");
    }
}

// Convert the statistics into ML data.  This is further unpicked within ML.
static Handle unpackStats(TaskData *taskData, const polystatistics *stats)
{
    // Vector for the counts.  Initially created as mutable then locked.
    Handle counts = alloc_and_save(taskData, N_PS_COUNTERS, F_MUTABLE_BIT);
    for (unsigned i = 0; i < N_PS_COUNTERS; i++)
    {
        Handle mark = taskData->saveVec.mark();
        Handle counterValue = Make_unsigned(taskData, stats->psCounters[i]);
        counts->WordP()->Set(i, counterValue->Word());
        taskData->saveVec.reset(mark);
    }
    // Can now lock the count vector by removing the mutable flag.
    counts->WordP()->SetLengthWord(N_PS_COUNTERS);

    // Vector for the sizes.
    Handle sizes = alloc_and_save(taskData, N_PS_SIZES, F_MUTABLE_BIT);
    for (unsigned j = 0; j < N_PS_SIZES; j++)
    {
        Handle mark = taskData->saveVec.mark();
        Handle sizeValue = Make_unsigned(taskData, stats->psSizes[j]);
        sizes->WordP()->Set(j, sizeValue->Word());
        taskData->saveVec.reset(mark);
    }
    sizes->WordP()->SetLengthWord(N_PS_SIZES);

    // Vector for the times.
    Handle times = alloc_and_save(taskData, N_PS_TIMES, F_MUTABLE_BIT);
    for (unsigned k = 0; k < N_PS_TIMES; k++)
    {
        Handle mark = taskData->saveVec.mark();
#ifdef HAVE_WINDOWS_H
        Handle sizeValue = 
            Make_arb_from_pair(taskData, stats->psTimers[k].dwHighDateTime, stats->psTimers[k].dwLowDateTime);
#else
        Handle sizeValue =
            Make_arb_from_pair_scaled(taskData, stats->psTimers[k].tv_sec, stats->psTimers[k].tv_usec, 1000000);
#endif
        times->WordP()->Set(k, sizeValue->Word());
        taskData->saveVec.reset(mark);
    }
    times->WordP()->SetLengthWord(N_PS_TIMES);

    // Vector for the user stats
    Handle users = alloc_and_save(taskData, N_PS_USER, F_MUTABLE_BIT);
    for (unsigned l = 0; l < N_PS_USER; l++)
    {
        Handle mark = taskData->saveVec.mark();
        Handle userValue = Make_arbitrary_precision(taskData, stats->psUser[l]);
        users->WordP()->Set(l, userValue->Word());
        taskData->saveVec.reset(mark);
    }
    users->WordP()->SetLengthWord(N_PS_USER);

    // Result vector
    Handle resultVec = alloc_and_save(taskData, 4);
    resultVec->WordP()->Set(0, counts->Word());
    resultVec->WordP()->Set(1, sizes->Word());
    resultVec->WordP()->Set(2, times->Word());
    resultVec->WordP()->Set(3, users->Word());
    return resultVec;
}

Handle poly_dispatch_c(TaskData *taskData, Handle args, Handle code)
{
    int c = get_C_int(taskData, DEREFWORDHANDLE(code));
    switch (c)
    {
    case 1:
        return exportNative(taskData, args); // Export
    case 2:
        raise_syscall(taskData, "C Export has been withdrawn", 0);
        return 0;
    case 3:
        return exportPortable(taskData, args); // Export as portable format

    case 10: // Return the RTS version string.
        {
            const char *version;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    version = "Portable-" TextVersion; break;
            case MA_I386:           version = "I386-" TextVersion; break;
            case MA_PPC:            version = "PPC-" TextVersion; break;
            case MA_Sparc:          version = "Sparc-" TextVersion; break;
            case MA_X86_64:         version = "X86_64-" TextVersion; break;
            default:                version = "Unknown-" TextVersion; break;
            }
            return SAVE(C_string_to_Poly(taskData, version));
        }

    case 11: // Return the RTS copyright string
        return SAVE(C_string_to_Poly(taskData, poly_runtime_system_copyright));

    case 12: // Return the architecture
        {
            const char *arch;
            switch (machineDependent->MachineArchitecture())
            {
            case MA_Interpreted:    arch = "Interpreted"; break;
            case MA_I386:           arch = "I386"; break;
            case MA_PPC:            arch = "PPC"; break;
            case MA_Sparc:          arch = "SPARC"; break;
            case MA_X86_64:         arch = "X86_64"; break;
            default:                arch = "Unknown"; break;
            }
            return SAVE(C_string_to_Poly(taskData, arch));
        }

    case 13: // Share common immutable data.
        {
            ShareData(taskData, args);
            return SAVE(TAGGED(0));
        }

        // ObjSize and ShowSize have their own IO vector entries but really they don't
        // need them.  Include them here and add ObjProfile.
    case 14:
        return ObjSize(taskData, args);

    case 15:
        return ShowSize(taskData, args);

    case 16:
        return ObjProfile(taskData, args);

    /* 17 and 18 are no longer used. */

    case 19: // Return the RTS argument help string.
        return SAVE(C_string_to_Poly(taskData, RTSArgHelp()));

    case 20: // Write a saved state file.
        return SaveState(taskData, args);

    case 21: // Load a saved state file and any ancestors.
        return LoadState(taskData, args);

    case 22: // Show the hierarchy.
        return ShowHierarchy(taskData);

    case 23: // Change the name of the immediate parent stored in a child
        return RenameParent(taskData, args);

    case 24: // Return the name of the immediate parent stored in a child
        return ShowParent(taskData, args);

    case 25: // Get the local statistics
        {
            polystatistics localStats;
            if (! globalStats.getLocalsStatistics(&localStats))
                raise_exception_string(taskData, EXC_Fail, "No statistics available");
            return unpackStats(taskData, &localStats);
        }

    case 26: // Get remote statistics.  The argument is the process ID to get the statistics.
        {
            polystatistics localStats;
            if (! globalStats.getRemoteStatistics(get_C_ulong(taskData, DEREFHANDLE(args)), &localStats))
                raise_exception_string(taskData, EXC_Fail, "No statistics available");
            return unpackStats(taskData, &localStats);
        }

    case 27: // Get number of user statistics available
        return Make_arbitrary_precision(taskData, N_PS_USER);

    case 28: // Set an entry in the user stats table.
        {
            unsigned index = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(0));
            if (index >= N_PS_USER)
                raise_exception0(taskData, EXC_subscript);
            int value = get_C_int(taskData, DEREFHANDLE(args)->Get(1));
            globalStats.setUserCounter(index, value);
            Make_arbitrary_precision(taskData, 0);
        }

    case 50: // GCD
        return gcd_arbitrary(taskData, SAVE(DEREFHANDLE(args)->Get(0)), SAVE(DEREFHANDLE(args)->Get(1)));
    case 51: // LCM
        return lcm_arbitrary(taskData, SAVE(DEREFHANDLE(args)->Get(0)), SAVE(DEREFHANDLE(args)->Get(1)));

        // These next ones were originally in process_env and have now been moved here,
    case 100: /* Return the maximum word segment size. */
            return Make_arbitrary_precision(taskData, MAX_OBJECT_SIZE);
    case 101: /* Return the maximum string size (in bytes).
                 It is the maximum number of bytes in a segment
                 less one word for the length field. */
            return Make_arbitrary_precision(taskData,
                (MAX_OBJECT_SIZE)*sizeof(PolyWord) - sizeof(PolyWord));
    case 102: /* Test whether the supplied address is in the io area.
                 This was previously done by having get_flags return
                 256 but this was changed so that get_flags simply
                 returns the top byte of the length word. */
        {
            PolyWord *pt = (PolyWord*)DEREFWORDHANDLE(args);
            if (gMem.IsIOPointer(pt))
                return Make_arbitrary_precision(taskData, 1);
            else return Make_arbitrary_precision(taskData, 0);
        }
    case 103: /* Return the register mask for the given function.
                 This is used by the code-generator to find out
                 which registers are modified by the function and
                 so need to be saved if they are used by the caller. */
        {
            PolyObject *pt = DEREFWORDHANDLE(args);
            if (gMem.IsIOPointer(pt))
            {
                /* IO area.  We need to get this from the vector. */
                int i;
                for (i=0; i < POLY_SYS_vecsize; i++)
                {
                    if (pt == (PolyObject*)IoEntry(i))
                    {
                        int regMask = machineDependent->GetIOFunctionRegisterMask(i);
                        POLYUNSIGNED props = rtsProperties(taskData, i);
                        return Make_arbitrary_precision(taskData, regMask | props);
                    }
                }
                raise_exception_string(taskData, EXC_Fail, "Io pointer not found");
            }
            else
            {
                /* We may have a pointer to the code or a pointer to
                   a closure.  If it's a closure we have to find the
                   code. */
                if (! pt->IsCodeObject() && ! pt->IsByteObject())
                    pt = pt->Get(0).AsObjPtr();

                /* Should now be a code object. */
                if (pt->IsCodeObject())
                {
                    /* Compiled code.  This is the second constant in the
                       constant area. */
                    PolyWord *codePt = pt->ConstPtrForCode();
                    PolyWord mask = codePt[1];
                    // A real mask will be an integer.
                    if (IS_INT(mask)) return SAVE(mask);
                    else raise_exception_string(taskData, EXC_Fail, "Invalid mask");
                }
                else raise_exception_string(taskData, EXC_Fail, "Not a code pointer");
            }
        }

    case 104: return Make_arbitrary_precision(taskData, POLY_version_number);

    case 105: /* Get the name of the function. */
        {
            PolyObject *pt = DEREFWORDHANDLE(args);
            if (gMem.IsIOPointer(pt))
            {
                /* IO area. */
                int i;
                for (i=0; i < POLY_SYS_vecsize; i++)
                {
                    if (pt == (PolyObject*)IoEntry(i))
                    {
                        char buff[8];
                        sprintf(buff, "RTS%d", i);
                        return SAVE(C_string_to_Poly(taskData, buff));
                    }
                }
                raise_syscall(taskData, "Io pointer not found", 0);
            }
            else if (pt->IsCodeObject()) /* Should now be a code object. */ 
            {
                /* Compiled code.  This is the first constant in the constant area. */
                PolyWord *codePt = pt->ConstPtrForCode();
                PolyWord name = codePt[0];
                /* May be zero indicating an anonymous segment - return null string. */
                if (name == PolyWord::FromUnsigned(0))
                    return SAVE(C_string_to_Poly(taskData, ""));
                else return SAVE(name);
            }
            else raise_syscall(taskData, "Not a code pointer", 0);
        }

    default:
        {
            char msg[100];
            sprintf(msg, "Unknown poly-specific function: %d", c);
            raise_exception_string(taskData, EXC_Fail, msg);
            return 0;
        }
    }
}
