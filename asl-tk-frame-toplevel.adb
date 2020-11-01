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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Asl.Tk.Frame.Toplevel is

   type Name_Char_Array is array (Character'Range) of Boolean;
   Name_Chars : Name_Char_Array := (Character'Range => False);

   ------------------------------------------------------------------------
   procedure Set_Name (W    : in out Widget;
                       Name : in String) is
   begin
      if (Name'Length = 0) then
         raise Invalid_Name;
      end if;

      for I in Name'Range loop
         if (not Name_Chars(Name(I))) then
            raise Invalid_Name;
         end if;
      end loop;

      W.Name := ASU.To_Unbounded_String("." & Name);
   end Set_Name;


   ------------------------------------------------------------------------
   function Get_Name (W : in Widget)
                      return String is
      Retval : String := ASU.To_String(W.Name);
   begin
      return Retval(Retval'First + 1 .. Retval'Last);
   end Get_Name;


   ------------------------------------------------------------------------
   procedure Display (W : in out Widget) is
      Rv : C.Int;
   begin
      if (ASU.Length(W.Name) = 0) then
         W.Name := ASU.To_Unbounded_String
           (".widget"
            & IntToStr_Chop_Space(Base_Widget_Count));
         Base_Widget_Count := Base_Widget_Count + 1;
      end if;

      Rv := Asl.Tcl.Tcl_Eval(Interp,
                             W.Cmd
                             & Space_ASU
                             & W.Name
                             & Space_ASU
                             & W.Attributes);

      W.Attributes := ASU.To_Unbounded_String("");

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Display;


   ------------------------------------------------------------------------
   procedure Set_Menu (W    : in out Widget;
                       Menu : in Asl.Tk.Menubar.Widget_Class) is
      Rv : C.Int;
   begin
      if (ASU.Length(Menu.Name) = 0) then
         Menu.Name := W.Name & ASU.To_Unbounded_String
           (".widget" & IntToStr_Chop_Space(W.Child_Num));
         W.Child_Num := W.Child_Num + 1;

         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                Menu.Cmd
                                & Space_ASU
                                & Menu.Name
                                & Space_ASU
                                & Menu.Attributes);

         W.Attributes := ASU.To_Unbounded_String("");

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;
      end if;

      Add_Attribute(W, "-menu " & ASU.To_String(Menu.Name));
   end Set_Menu;


   ------------------------------------------------------------------------
   procedure Initialize (W : in out Widget) is
   begin
      Asl.Tk.Frame.Initialize(Widget_Parent(W));
      W.Cmd := Toplevel_ASU;
   end Initialize;


begin
   for I in Character range 'a' .. 'z' loop
      Name_Chars(I) := True;
   end loop;

   for I in Character range 'A' .. 'Z' loop
      Name_Chars(I) := True;
   end loop;

   for I in Character range '0' .. '9' loop
      Name_Chars(I) := True;
   end loop;
end Asl.Tk.Frame.Toplevel;
