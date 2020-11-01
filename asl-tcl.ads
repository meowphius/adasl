-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This library is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--

with Interfaces.C.Strings;
with Cargv;
with Baseclass;
with Ada.Strings.Unbounded;

package Asl.Tcl is

   type Tcl_Interp is private;

   type ClientData is access all Baseclass.Limited_Object'Class;
   pragma Convention (C, ClientData);

   ------------------------------------------------------------------------
   -- Return values from Tcl calls.
   TCL_OK               : constant := 0;
   TCL_ERROR            : constant := 1;
   TCL_RETURN           : constant := 2;
   TCL_BREAK            : constant := 3;
   TCL_CONTINUE         : constant := 4;
   TCL_RESULT_SIZE      : constant := 200;

   ------------------------------------------------------------------------
   function Tcl_CreateInterp return Tcl_Interp;
   pragma Import(C, Tcl_CreateInterp, "Tcl_CreateInterp");

   ------------------------------------------------------------------------
   function Tcl_Init (Interp : in Tcl_Interp)
                      return Interfaces.C.Int;
   pragma Import(C, Tcl_Init, "Tcl_Init");

   ------------------------------------------------------------------------
   function Tcl_Eval (Interp : in Tcl_Interp;
                      Cmd    : in Interfaces.C.Char_Array)
                      return Interfaces.C.Int;
   pragma Import(C, Tcl_Eval, "Tcl_Eval");

   ------------------------------------------------------------------------
   function Tcl_Eval (Interp : in Tcl_Interp;
                      Cmd    : in Ada.Strings.Unbounded.Unbounded_String)
                      return Interfaces.C.Int;

   ------------------------------------------------------------------------
   function Tcl_Eval (Interp : in Tcl_Interp;
                      Cmd    : in String)
                      return Interfaces.C.Int;

   ------------------------------------------------------------------------
   function Tcl_GetStringResult (Interp : in Tcl_Interp)
                                 return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C, Tcl_GetStringResult, "Tcl_GetStringResult");

   ------------------------------------------------------------------------
   type Tcl_CmdCallback is
      access function (Data            : ClientData;
                       Interp          : Tcl_Interp;
                       Argc            : Interfaces.C.Int;
                       Argv            : CArgv.Chars_Ptr_Ptr)
                       return Interfaces.C.Int;
   pragma Convention (C, Tcl_CmdCallback);

   ------------------------------------------------------------------------
   type Tcl_CmdDeleteProc is access procedure (Data : ClientData);
   pragma Convention (C, Tcl_CmdDeleteProc);

   ------------------------------------------------------------------------
   procedure Tcl_CreateCommand (Interp      : in Tcl_Interp;
                                Name        : in Interfaces.C.Char_Array;
                                Proc        : in Tcl_CmdCallback;
                                Data        : in ClientData;
                                Delete_Proc : in Tcl_CmdDeleteProc);
   pragma Import (C, Tcl_CreateCommand, "Tcl_CreateCommand");

   ------------------------------------------------------------------------
   procedure Tcl_DeleteCommand (Interp      : in Tcl_Interp;
                                Name        : in Interfaces.C.Char_Array);
   pragma Import (C, Tcl_DeleteCommand, "Tcl_DeleteCommand");

private

   type Tcl_Interp_Rec is null record;
   type Tcl_Interp is access all Tcl_Interp_Rec;
   pragma Convention (C, Tcl_Interp);

end Asl.Tcl;
