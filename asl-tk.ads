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

with Baseclass;
with Ada.Strings.Unbounded; use type Ada.Strings.Unbounded.Unbounded_String;
with Asl.Tcl;
with Interfaces.C.Strings;
with Asgc.Ordered.Dlist.Dynamic;

package Asl.Tk is

   type Color_Attr is private;

   function To_Color (Name  : in String)
                      return Color_Attr;

   type RGB_Value is mod 2**16;
   function To_Color (Red   : in RGB_Value;
                      Green : in RGB_Value;
                      Blue  : in RGB_Value)
                      return Color_Attr;

   Black_Color : constant Color_Attr;
   White_Color : constant Color_Attr;

   type Point is record
      X, Y : Natural;
   end record;

   type Size_Attr is record
      Width    : Natural;
      Height   : Natural;
   end record;

   type Rectangle is record
      Top_Left : Point;
      Size     : Size_Attr;
   end record;

   type Font_Attr is private;

   function To_Font (Name : in String)
                     return Font_Attr;


   subtype Widget_Parent is Baseclass.Limited_Object;
   type Widget is abstract new Widget_Parent with private;
   type Widget_Class is access all Widget'Class;

   -- Foreground and background colors.
   procedure Set_Foreground (W     : in out Widget;
                             Color : in Color_Attr);

   function Get_Foreground (W : in Widget)
                            return Color_Attr;

   procedure Set_Background (W     : in out Widget;
                             Color : in Color_Attr);

   function Get_Background (W : in Widget)
                            return Color_Attr;

   -- Widget Size follows the Tk model, if holding text it is characters
   -- and lines, if holding images or other stuff it is pixels.
   procedure Set_Size (W      : in out Widget;
                       Size   : in Size_Attr);

   function Get_Size (W : in Widget)
                      return Size_Attr;


   Widget_Error : exception;


   subtype Event_Parent is Baseclass.Object;
   type Event is abstract new Event_Parent with private;
   type Event_Class is access all Event'Class;

   Event_Not_Found : exception;

   procedure Initialize;

   procedure Mainloop;

   -- This must be called before exit after initialize has been called.
   procedure Shutdown;


   subtype Container_Parent is Widget;
   type Container is abstract new Container_Parent with private;
   type Container_Class is access all Container'Class;

   procedure Shutdown (W : in out Container)
      is abstract;

   Child_Not_Found : exception;
   procedure Remove_Child (W     : in out Container;
                           Child : in Widget_Class)
      is abstract;


   subtype Managed_Parent is Container;
   type Managed is abstract new Managed_Parent with private;
   type Managed_Class is access all Managed'Class;

   subtype Layout_Parent is Baseclass.Object;
   type Layout is abstract new Layout_Parent with private;
   type Layout_Class is access all Layout'Class;

   subtype Constraint_Parent is Baseclass.Object;
   type Constraint is abstract new Constraint_Parent with private;
   type Constraint_Class is access all Constraint'Class;


   -- A layout manager must be set for the root and for each container
   -- before adding children.
   procedure Set_Layout_Manager (W       : in out Managed;
                                 Manager : in Layout_Class);

   Root_Frame : Managed_Class := null;


   -- Set the parent of the widget.  This must be done before the widgets
   -- is displayed the first time.  Also, the parent must be set before any
   -- children are added to the widget.
   Already_Displayed : exception;
   Already_Named : exception;
   procedure Set_Parent (W           : in Widget_Class;
                         Parent      : in Managed_Class);

   procedure Set_Parent (W           : in Widget_Class;
                         Parent      : in Managed_Class;
                         Constraints : in Constraint'Class);

   function Get_Parent (W : in Widget)
                        return Container_Class;

   procedure Set_Constraints (W           : in Widget_Class;
                              Constraints : in Constraint'Class);

   -- Map and unmap the widget from the screen.
   procedure Display (W : in out Widget);
   procedure Undisplay (W : in out Widget);

   -- Destroy the widget, removing it from the screen.  The widget data
   -- structure will still be allocated, but cannot be operated on.
   procedure Destroy (W : in out Widget);


   -- Layout methods that only widgets can call.
   procedure Add_Child (W     : in out Managed;
                        Child : in Widget_Class)
      is abstract;

   -- Routines called between the container and the layout manager.
   procedure Set_Layout_Master (Manager : in out Layout;
                                Master  : in Managed_Class)
      is abstract;

   procedure Child_Added (Manager : in out Layout;
                          Child   : in Widget_Class)
      is abstract;

   procedure Child_Removed (Manager : in out Layout;
                            Child   : in Widget_Class)
      is abstract;

   procedure Set_Default_Constraints (Manager : in out Layout;
                                      Child   : in Widget_Class)
      is abstract;

   procedure Set_Constraints (Manager     : in out Layout;
                              Child       : in Widget_Class;
                              Constraints : in Constraint'Class)
      is abstract;

   procedure Unmap (Manager : in out Layout;
                    W       : in out Widget'Class)
      is abstract;

   procedure Map (Manager : in out Layout;
                  W       : in out Widget'Class)
      is abstract;


   -- An iterator for a layout manager to scan through all the children
   -- of a container.
   subtype Child_Iterator_Parent is Baseclass.Object;
   type Child_Iterator is abstract new Child_Iterator_Parent with private;
   type Child_Iterator_Class is access all Child_Iterator'Class;

   function Get_Iterator (W : in Container)
                          return Child_Iterator_Class
      is abstract;

   procedure Free_Iterator (Iter : access Child_Iterator)
      is abstract;

   procedure First (Iter   : in out Child_Iterator;
                    At_End : out Boolean)
      is abstract;

   procedure Last (Iter   : in out Child_Iterator;
                   At_End : out Boolean)
      is abstract;

   procedure Next (Iter   : in out Child_Iterator;
                   At_End : out Boolean)
      is abstract;

   procedure Prev (Iter   : in out Child_Iterator;
                   At_End : out Boolean)
      is abstract;

   function Get (Iter : in Child_Iterator)
                 return Widget_Class
      is abstract;

private

   package ASU renames Ada.Strings.Unbounded;
   package C renames Interfaces.C;
   package CStr renames Interfaces.C.Strings;
   use type C.Int;

   -- Tk doesn't like being called after getting an X shutdown event, so
   -- when we shutdown we use this to avoid doing any Tk calls.
   In_Shutdown : Boolean := False;

   -- Count each widget created, used to give each a unique name.
   Base_Widget_Count : Natural := 0;

   -- Count each event handler added to give each a unique name.
   Event_Handler_Count : Natural := 0;

   Configure_ASU : ASU.Unbounded_String
     := ASU.To_Unbounded_String(" configure ");

   CGet_ASU : ASU.Unbounded_String := ASU.To_Unbounded_String(" cget ");

   Destroy_ASU : ASU.Unbounded_String := ASU.To_Unbounded_String("destroy ");

   Space_ASU : ASU.Unbounded_String := ASU.To_Unbounded_String(" ");

   Interp : Asl.Tcl.Tcl_Interp;

   type Color_Attr is record
      Name : ASU.Unbounded_String;
   end record;

   Black_Color : constant Color_Attr
     := (Name => ASU.To_Unbounded_String("black"));
   White_Color : constant Color_Attr
     := (Name => ASU.To_Unbounded_String("white"));

   type Font_Attr is record
      Name :  ASU.Unbounded_String;
   end record;

   type Layout_Info is abstract new Baseclass.Object with null record;
   type Layout_Info_Class is access all Layout_Info'Class;

   type Widget is abstract new Widget_Parent with record
      Parent     : Container_Class      := null;
      Name       : ASU.Unbounded_String;
      Cmd        : ASU.Unbounded_String;
      Displayed  : Boolean              := False;

      -- Used to build up attribute strings.
      Attributes : ASU.Unbounded_String;

      -- Used to number the child widgets of this widget.
      Child_Num  : Natural := 0;

      -- Let the layout manager store info here.
      For_Layout : Layout_Info_Class    := null;
   end record;

   function IntToStr_Chop_Space (I : in Integer)
                                 return String;

   procedure Apply_Attribute (W : in out Widget'Class);

   function Get_Attribute (W         : in Widget'Class;
                           Attr_Name : in String)
                           return String;

   procedure Add_Attribute (W         : in out Widget'Class;
                            Attr_Name : in String);

   procedure Add_Attribute (W         : in out Widget'Class;
                            Attr_Name : in String;
                            Attr_Val  : in String);


   type Event is abstract new Event_Parent with record
      Next : Event_Class;
   end record;

   procedure Add_Event (Event_List : in out Event_Class;
                        Event      : in Event_Class);

   procedure Remove_Event (Event_List : in out Event_Class;
                           Event      : in Event_Class);

   type Layout is abstract new Layout_Parent with null record;

   type Constraint is abstract new Constraint_Parent with null record;

   type Container is abstract new Container_Parent with null record;

   type Managed is abstract new Managed_Parent with record
      Manager : Layout_Class;
   end record;

   type Child_Iterator is abstract new Child_Iterator_Parent with null record;

   package Container_List_Base is new Asgc(Contained_Type => Container_Class);
   package Container_List_Ordered is new Container_List_Base.Ordered;
   package Container_List_Dlist is new Container_List_Ordered.Dlist;
   package Container_List is new Container_List_Dlist.Dynamic;
   use type Container_List_Base.End_Marker;

   All_Containers : Container_List.Object;


end Asl.Tk;
