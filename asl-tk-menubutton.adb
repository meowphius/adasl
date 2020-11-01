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

with Asl.Tk.Menubar;
with Ada.Exceptions;

package body Asl.Tk.Menubutton is

   MB_Configure_ASU : ASU.Unbounded_String
     := ASU.To_Unbounded_String(" entryconfigure ");

   MB_CGet_ASU : ASU.Unbounded_String
     := ASU.To_Unbounded_String(" entrycget ");

   ------------------------------------------------------------------------
   procedure MB_Apply_Attribute (W : in out Widget'Class) is
      Rv    : C.Int;
      Index : Natural;
      W_Ptr : Widget_Class := W'Unchecked_Access;
   begin
      if (W.Parent /= null) then
         Index := Asl.Tk.Menuitem.Get_Index
           (Asl.Tk.Menuitem.Menuitem_Parent'Class(W.Parent.all),
            Asl.Tk.Menuitem.Widget_Class(W_Ptr));

         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                W.Parent.Name
                                & MB_Configure_ASU
                                & IntToStr_Chop_Space(Index - 1)
                                & W.Attributes);

         W.Attributes := ASU.To_Unbounded_String("");

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;
      end if;
   end MB_Apply_Attribute;


   ------------------------------------------------------------------------
   function MB_Get_Attribute (W         : in Widget'Class;
                              Attr_Name : in String)
                              return String is
      Rv : C.Int;
   begin
      if (W.Name /= ASU.Null_Unbounded_String) then
         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                W.Name & MB_CGet_ASU & Attr_Name);

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;

         return CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp));
      else
         -- FIXME - need to search the attribute list if it hasn't been
         -- displayed yet.
         return "";
      end if;
   end MB_Get_Attribute;


   ------------------------------------------------------------------------
   procedure MB_Add_Attribute (W         : in out Widget'Class;
                               Attr_Name : in String) is
   begin
      W.Attributes :=
        W.Attributes
        & Space_ASU
        & ASU.To_Unbounded_String(Attr_Name);

      MB_Apply_Attribute(W);
   end MB_Add_Attribute;


   ------------------------------------------------------------------------
   procedure MB_Add_Attribute (W         : in out Widget'Class;
                               Attr_Name : in String;
                               Attr_Val  : in String) is
   begin
      W.Attributes :=
        W.Attributes
        & Space_ASU
        & ASU.To_Unbounded_String(Attr_Name & " " & Attr_Val);

      MB_Apply_Attribute(W);
   end MB_Add_Attribute;


   ------------------------------------------------------------------------
   procedure Initialize (W : in out Widget) is
   begin
      null;
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
      MB_Add_Attribute(W, "-label", """" & Name & """");
   end Set_Text;


   ------------------------------------------------------------------------
   function Get_Text (W : in Widget)
                      return String is
   begin
      return MB_Get_Attribute(W, "-label");
   end Get_Text;


   ------------------------------------------------------------------------
   procedure Set_Font (W    : in out Widget;
                       Font : in Font_Attr) is
   begin
      MB_Add_Attribute(W, "-font", """" & ASU.To_String(Font.Name) & """");
   end Set_Font;


   ------------------------------------------------------------------------
   function Get_Font (W : in Widget)
                      return Font_Attr is
      Retval : Font_Attr;
   begin
      Retval.Name := Retval.Name & MB_Get_Attribute(W, "-font");

      return Retval;
   end Get_Font;


   ------------------------------------------------------------------------
   procedure Set_Image (W     : in out Widget;
                        Image : in Asl.Tk.Image.Object) is
   begin
      MB_Add_Attribute(W, "-image", Asl.Tk.Image.Attribute(Image));
   end Set_Image;

end Asl.Tk.Menubutton;
