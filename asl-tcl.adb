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

package body Asl.Tcl is

   function Tcl_Eval (Interp : in Tcl_Interp;
                      Cmd    : in Ada.Strings.Unbounded.Unbounded_String)
                      return Interfaces.C.Int is
   begin
      return Tcl_Eval(Interp,
                      Interfaces.C.To_C(Ada.Strings.Unbounded.To_String(Cmd)));
   end Tcl_Eval;


   function Tcl_Eval (Interp : in Tcl_Interp;
                      Cmd    : in String)
                      return Interfaces.C.Int is
   begin
      return Tcl_Eval(Interp,
                      Interfaces.C.To_C(Cmd));
   end Tcl_Eval;

end Asl.Tcl;
