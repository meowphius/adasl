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

with Asgc.Ordered.Alist.Expandable;
with Asl.Tk.Menuitem.Cascade; use type Asl.Tk.Menuitem.Cascade.Widget_Class;

package Asl.Tk.Menubar is

   subtype Widget_Parent is Asl.Tk.Menuitem.Menuitem_Parent;
   type Widget is new Widget_Parent with private;
   type Widget_Ptr is access all Widget;
   type Widget_Class is access all Widget'Class;

   Last_Position : constant Natural := 0;

   procedure Add_Button (W      : in out Widget;
                         Button : in Asl.Tk.Menuitem.Cascade.Widget_Class;
                         Index  : in Natural := Last_Position);

   procedure Remove_Button (W      : in out Widget;
                            Button : in Asl.Tk.Menuitem.Cascade.Widget_Class);

   procedure Remove_Child (W     : in out Widget;
                           Child : in Asl.Tk.Widget_Class);

   function Get_Index (W      : in Widget;
                       Button : in Asl.Tk.Menuitem.Widget_Class)
                       return Positive;

   procedure Shutdown (W : in out Widget);

   procedure Initialize (W : in out Widget);
   procedure Finalize (W : in out Widget);


   subtype Child_Iterator_Parent is Asl.Tk.Child_Iterator;
   type Child_Iterator is new Child_Iterator_Parent with private;
   type Child_Iterator_Ptr is access all Child_Iterator;

   function Get_Iterator (W : in Widget)
                          return Asl.Tk.Child_Iterator_Class;

   procedure Free_Iterator (Iter : access Child_Iterator);

   procedure First (Iter   : in out Child_Iterator;
                    At_End : out Boolean);

   procedure Last (Iter   : in out Child_Iterator;
                   At_End : out Boolean);

   procedure Next (Iter   : in out Child_Iterator;
                   At_End : out Boolean);

   procedure Prev (Iter   : in out Child_Iterator;
                   At_End : out Boolean);

   function Get (Iter : in Child_Iterator)
                 return Asl.Tk.Widget_Class;


private

   Menubar_ASU : ASU.Unbounded_String := ASU.To_Unbounded_String("menu");

   package Child_List_Base is
     new Asgc(Contained_Type => Asl.Tk.Menuitem.Cascade.Widget_Class);
   package Child_List_Ordered is new Child_List_Base.Ordered;
   package Child_List_Alist is new Child_List_Ordered.Alist;
   package Child_List is new Child_List_Alist.Expandable;
   use type Child_List_Base.End_Marker;

   type Widget is new Widget_Parent with record
      Children  : aliased Child_List.Object_Ptr
        := new Child_List.Object(Initial_Size => 10, Increment => 10);
   end record;

   type Child_Iterator is new Child_Iterator_Parent with record
      Iterator : Child_List.Iterator;
   end record;

end Asl.Tk.Menubar;
