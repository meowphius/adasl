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

with Asl.Tk.Image;
with Asl.Tk.Menuitem;

package Asl.Tk.Menubutton is

   subtype Widget_Parent is Asl.Tk.Menuitem.Widget;
   type Widget is new Widget_Parent with private;
   type Widget_Ptr is access all Widget;
   type Widget_Class is access all Widget'Class;

   procedure Set_Text (W    : in out Widget;
                       Name : in String);

   function Get_Text (W : in Widget)
                      return String;

   procedure Set_Font (W    : in out Widget;
                       Font : in Font_Attr);

   function Get_Font (W : in Widget)
                      return Font_Attr;

   procedure Set_Image (W     : in out Widget;
                        Image : in Asl.Tk.Image.Object);


   procedure Initialize (W : in out Widget);
   procedure Finalize (W : in out Widget);

private

   type Widget is new Widget_Parent with null record;

end Asl.Tk.Menubutton;
