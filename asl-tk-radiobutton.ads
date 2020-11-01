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

package Asl.Tk.Radiobutton is

   subtype Widget_Parent is Asl.Tk.Widget;
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

   function Is_Set (W : in Widget)
                    return Boolean;

   procedure Set_Value (W     : in out Widget;
                        Value : in Boolean);

   Radiobutton_Not_In_Group : exception;

   procedure Display (W : in out Widget);


   -- What do I do when the button is pressed?
   type Press_Event is abstract new Event with private;
   type Press_Event_Class is access all Press_Event'Class;
   procedure Handle (Event : in out Press_Event;
                     W     : in Widget'Class)
      is abstract;

   procedure Add_Event (W     : in out Widget;
                        Event : in Press_Event_Class);

   procedure Remove_Event (W     : in out Widget;
                           Event : in Press_Event_Class);

   procedure Initialize (W : in out Widget);
   procedure Finalize (W : in out Widget);


   subtype Group_Parent is Baseclass.Object;
   type Group is new Group_Parent with private;
   type Group_Ptr is access all Group;
   type Group_Class is access all Group'Class;

   procedure Add_To_Group (G : in out Group;
                           W : in Widget_Class);

   procedure Remove_From_Group (W : in out Widget);


   -- Note that it is possible for this to return null if not buttons are
   -- pressed.
   function Get_Current_Button (W : in Group)
                                return Widget_Class;


   -- What do I do when the the current button changes for the group?
   type Change_Event is abstract new Event with private;
   type Change_Event_Class is access all Change_Event'Class;
   procedure Handle (Event : in out Change_Event;
                     W     : in Group'Class)
      is abstract;

   procedure Add_Event (W     : in out Group;
                        Event : in Change_Event_Class);

   procedure Remove_Event (W     : in out Group;
                           Event : in Change_Event_Class);

   procedure Initialize (W : in out Group);
   procedure Finalize (W : in out Group);


   subtype Group_Iterator_Parent is Baseclass.Object;
   type Group_Iterator is new Group_Iterator_Parent with private;
   type Group_Iterator_Class is access all Group_Iterator'Class;
   type Group_Iterator_Ptr is access all Group_Iterator;

   function Get_Iterator (W : in Group)
                          return Group_Iterator_Class;

   procedure Free_Iterator (Iter : access Group_Iterator);

   procedure First (Iter   : in out Group_Iterator;
                    At_End : out Boolean);

   procedure Last (Iter   : in out Group_Iterator;
                   At_End : out Boolean);

   procedure Next (Iter   : in out Group_Iterator;
                   At_End : out Boolean);

   procedure Prev (Iter   : in out Group_Iterator;
                   At_End : out Boolean);

   function Get (Iter : in Group_Iterator)
                 return Widget_Class;


private

   Radiobutton_ASU : ASU.Unbounded_String
     := ASU.To_Unbounded_String("radiobutton");

   Curr_Varnum : Natural := 0;

   type Widget is new Widget_Parent with record
      On_Press  : Asl.Tk.Event_Class   := null;
      Press_Cmd : ASU.Unbounded_String;
      Group     : Group_Class;
      Value     : Positive;
   end record;

   type Press_Event is abstract new Event with null record;


   package Group_List_Base is new Asgc(Contained_Type => Widget_Class);
   package Group_List_Ordered is new Group_List_Base.Ordered;
   package Group_List_Dlist is new Group_List_Ordered.Dlist;
   package Group_List is new Group_List_Dlist.Dynamic;
   use type Group_List_Base.End_Marker;

   type Group is new Group_Parent with record
      On_Change   : Asl.Tk.Event_Class   := null;
      Change_Cmd  : ASU.Unbounded_String;
      Group_Value : Positive := 1;
      Varname     : ASU.Unbounded_String;
      Current     : Widget_Class;
      Members     : Group_List.Object_Ptr := new Group_List.Object;
   end record;

   type Group_Iterator is new Group_Iterator_Parent with record
      Iterator : Group_List.Iterator;
   end record;

   type Change_Event is abstract new Event with null record;

end Asl.Tk.Radiobutton;
