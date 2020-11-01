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

package Asl.Tk.Menuitem is

   -- All the items that can be in a menu descend from this.
   subtype Widget_Parent is Asl.Tk.Widget;
   type Widget is abstract new Widget_Parent with private;
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


   subtype Menuitem_Parent_Parent is Asl.Tk.Container;
   type Menuitem_Parent is abstract new Menuitem_Parent_Parent with private;
   type Menuitem_Parent_Class is access all Menuitem_Parent'Class;

   function Get_Index (W      : in Menuitem_Parent;
                       Button : in Widget_Class)
                       return Positive
      is abstract;

private

   type Widget is abstract new Widget_Parent with record
      Text : ASU.Unbounded_String;
   end record;

   type Menuitem_Parent is abstract
    new Menuitem_Parent_Parent with null record;

   function MI_Get_Attribute (W         : in Widget'Class;
                              Attr_Name : in String)
                              return String;

   procedure MI_Add_Attribute (W         : in out Widget'Class;
                               Attr_Name : in String);

   procedure MI_Add_Attribute (W         : in out Widget'Class;
                               Attr_Name : in String;
                               Attr_Val  : in String);

end Asl.Tk.Menuitem;
