(*
    Copyright (c) 2017 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure GCode =
struct
    structure xCode = X86Code and bCode = ByteCode
    open ByteCode

    structure Foreign =
    struct
        val rtsCallFull = xCode.Foreign.rtsCallFull
        and rtsCallFast = xCode.Foreign.rtsCallFast
        
        val rtsCallFastFloattoFloat = bCode.Foreign.rtsCallFastFloattoFloat
        and rtsCallFastGeneraltoFloat = bCode.Foreign.rtsCallFastGeneraltoFloat
    end
end;
