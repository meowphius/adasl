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

package body Asl.Tk.Label is

   ------------------------------------------------------------------------
   procedure Initialize (W : in out Widget) is
      Client_Parm : Asl.Tcl.ClientData := W'Unchecked_Access;
   begin
      W.Cmd := Label_ASU;
   end Initialize;


   ------------------------------------------------------------------------
   procedure Finalize (W : in out Widget) is
   begin
      -- Handle destroying widget if displayed
      Destroy(W);
   end Finalize;


   ------------------------------------------------------------------------
   procedure Set_Text (W    : in out Widget;
                       Name : in String) is
   begin
      Add_Attribute(W, "-text", """" & Name & """");
   end Set_Text;


   ------------------------------------------------------------------------
   function Get_Text (W : in Widget)
                      return String is
   begin
      return Get_Attribute(W, "-text");
   end Get_Text;


   ------------------------------------------------------------------------
   procedure Set_Font (W    : in out Widget;
                       Font : in Font_Attr) is
   begin
      Add_Attribute(W, "-font", """" & ASU.To_String(Font.Name) & """");
   end Set_Font;


   ------------------------------------------------------------------------
   function Get_Font (W : in Widget)
                      return Font_Attr is
      Retval : Font_Attr;
   begin
      Retval.Name := Retval.Name & Get_Attribute(W, "-font");

      return Retval;
   end Get_Font;


   ------------------------------------------------------------------------
   procedure Set_Justify (W     : in out Widget;
                          Align : in Justification) is
   begin
      case Align is
         when Left =>
            Add_Attribute(W, "-justify left");
         when Center =>
            Add_Attribute(W, "-justify center");
         when Right =>
            Add_Attribute(W, "-justify right");
      end case;
   end Set_Justify;


   ------------------------------------------------------------------------
   function Get_Justify (W : in Widget)
                         return Justification is
      Str_Just : String := Get_Attribute(W, "-justify");
   begin
      if (Str_Just = "left") then
         return Left;
      elsif (Str_Just = "right") then
         return right;
      elsif (Str_Just = "center") then
         return Center;
      else
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            "justify attribute not left, right, or center");
      end if;
   end Get_Justify;

end Asl.Tk.Label;

