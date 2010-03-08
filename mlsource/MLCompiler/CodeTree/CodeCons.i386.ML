(*
    Copyright (c) 2000-10
        David C.J. Matthews and
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

structure X86OutputCode =
    X86OUTPUTCODE(
        structure DEBUG   = Debug
        structure PRETTY  = Pretty
    );

structure X86Optimise =
    X86OPTIMISE(
        structure X86CODE = X86OutputCode
    );

structure CodeCons =
    X86LOWLEVEL(
        structure X86CODE = X86OutputCode
        structure X86OPTIMISE = X86Optimise
    );
