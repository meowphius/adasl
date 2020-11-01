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

with Asgc.Ordered.Dlist.Dynamic;

package Asl.Tk.Frame is

   subtype Widget_Parent is Asl.Tk.Managed;
   type Widget is new Widget_Parent with private;
   type Widget_Ptr is access all Widget;
   type Widget_Class is access all Widget'Class;

   procedure Add_Child (W     : in out Widget;
                        Child : in Asl.Tk.Widget_Class);

   procedure Remove_Child (W     : in out Widget;
                           Child : in Asl.Tk.Widget_Class);

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

   Frame_ASU : ASU.Unbounded_String := ASU.To_Unbounded_String("frame");

   package Child_List_Base is new Asgc(Contained_Type => Asl.Tk.Widget_Class);
   package Child_List_Ordered is new Child_List_Base.Ordered;
   package Child_List_Dlist is new Child_List_Ordered.Dlist;
   package Child_List is new Child_List_Dlist.Dynamic;
   use type Child_List_Base.End_Marker;

   type Frame_Callbacks is new Child_List_Base.Callbacks with null record;
   type Frame_Callbacks_Ptr is access all Frame_Callbacks;

   procedure Added (Cb  : access Frame_Callbacks;
                    O   : in Child_List_Base.Object'Class;
                    Val : in out Asl.Tk.Widget_Class);

   procedure Copied (Cb  : access Frame_Callbacks;
                     O   : in Child_List_Base.Object'Class;
                     Val : in out Asl.Tk.Widget_Class);

   procedure Deleted (Cb  : access Frame_Callbacks;
                      O   : in Child_List_Base.Object'Class;
                      Val : in out Asl.Tk.Widget_Class);

   type Widget is new Widget_Parent with record
      Children  : Child_List.Object_Ptr := new Child_List.Object;
      Callbacks : aliased Frame_Callbacks;
   end record;

   type Child_Iterator is new Child_Iterator_Parent with record
      Iterator : Child_List.Iterator;
   end record;

end Asl.Tk.Frame;
