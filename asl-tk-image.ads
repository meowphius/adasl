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

package Asl.Tk.Image is

   subtype Object_Parent is Baseclass.Object;
   type Object is new Object_Parent with private;
   type Object_Ptr is access all Object;
   type Object_Class is access all Object'Class;

   type String_Array is array (Integer range <>)
     of Ada.Strings.Unbounded.String_Access;

   function Create_Bitmap (File           : in String;
                           Set_Background : in Boolean    := False;
                           Background     : in Color_Attr := Black_Color;
                           Set_Foreground : in Boolean    := False;
                           Foreground     : in Color_Attr := White_Color;
                           Maskfile       : in String     := "")
                           return Object;

   Dummy_Mask : constant String_Array;

   -- Takes a normal X11 bitmap
   function Create_Bitmap (Bitmap         : in String_Array;
                           Set_Background : in Boolean      := False;
                           Background     : in Color_Attr   := Black_Color;
                           Set_Foreground : in Boolean      := False;
                           Foreground     : in Color_Attr   := White_Color;
                           Mask           : in String_Array := Dummy_Mask)
                           return Object;

   function Create_Photo (File  : in String;
                          Gamma : in Float := 1.0)
                          return Object;

   -- Takes a base-64 encoded array of strings.
   function Create_Photo (Data  : in String_Array;
                          Gamma : in Float := 1.0)
                          return Object;

   procedure Finalize (O : in out Object);
   procedure Adjust (O : in out Object);

   -- Private internal to Tk.
   function Attribute (O : in Object)
                       return String;

private

   Dummy_Mask : constant String_Array := (1 => null);

   Curr_Image_Num : Natural := 0;
   type Natural_Ptr is access all Natural;

   type Object is new Object_Parent with record
      Name     : Ada.Strings.Unbounded.Unbounded_String;
      Refcount : Natural_Ptr := new Natural'(1);
   end record;

end Asl.Tk.Image;
