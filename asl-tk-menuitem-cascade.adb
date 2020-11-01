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

package body Asl.Tk.Menuitem.Cascade is

   procedure Free is new Ada.Unchecked_Deallocation (Child_Iterator,
                                                     Child_Iterator_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (Container,
                                                     Container_Ptr);

   ------------------------------------------------------------------------
   procedure Initialize (W : in out Widget) is
   begin
      W.Cmd := Cascade_ASU;
   end Initialize;


   ------------------------------------------------------------------------
   procedure Finalize (W : in out Widget) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Child_List.Object, Child_List.Object_Ptr);
   begin
      Free(W.My_Container.Children);
      Free(W.My_Container);

      -- Handle destroying widget if displayed
      Destroy(Asl.Tk.Widget(W));
   end Finalize;


   ------------------------------------------------------------------------
   function Get_Container (W : in Widget)
                           return Asl.Tk.Container_Class is
   begin
      return Asl.Tk.Container_Class(W.My_Container);
   end Get_Container;


   ------------------------------------------------------------------------
   procedure Add_Button (W      : in out Container;
                         Button : in Asl.Tk.Menuitem.Widget_Class;
                         Index  : in Natural := Last_Position) is
      Rv     : C.Int;
      Parent : Container_Ptr := W'Unchecked_Access;
   begin
      if (ASU.Length(Button.Name) /= 0) then
         raise Already_Displayed;
      end if;

      Button.Name := W.Name & ASU.To_Unbounded_String
        (".widget" & IntToStr_Chop_Space(W.Child_Num));
      W.Child_Num := W.Child_Num + 1;

      if (ASU.To_String(W.Cmd) = "cascade") then
         Rv := Asl.Tcl.Tcl_Eval(Interp, "menu " & Button.Name);
         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;

         Button.Attributes := Button.Attributes & " -menu " & Button.Name;
      end if;


      if (Index = Last_Position) then
         Child_List.Enqueue(W.Children.all, Button);
         Rv := Asl.Tcl.Tcl_Eval
           (Interp,
            W.Name
            & " add "
            & Button.Cmd
            & Button.Attributes);
      else
         Child_List.Add_At(W.Children.all, Index, Button);
         Rv := Asl.Tcl.Tcl_Eval
           (Interp,
            W.Name
            & " insert "
            & IntToStr_Chop_Space(Index - 1)
            & Button.Cmd
            & Button.Attributes);
      end if;

      Button.Attributes := ASU.To_Unbounded_String("");
      Button.Parent := Container_Class(Parent);

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Add_Button;


   ------------------------------------------------------------------------
   procedure Remove_Button (W      : in out Container;
                            Button : in Asl.Tk.Menuitem.Widget_Class)
   is
      Iter   : Child_List.Iterator;
      Found  : Boolean;
      Is_End : Child_List_Base.End_Marker;
      Rv     : C.Int;
   begin
      Child_List.Set_Container
        (Iter, Child_List_Base.Object_Class(W.Children));
      Child_List.Search(Iter, Button, Found);
      if (not Found) then
         raise Child_Not_Found;
      end if;

      Rv := Asl.Tcl.Tcl_Eval
        (Interp,
         W.Name
         & ASU.To_Unbounded_String
             (" delete "
              & IntToStr_Chop_Space(Child_List.Get_Loc(Iter))));

      Child_List.Delete(Iter, Is_End);

      -- FIXME - Button can be redisplayed but attributes are lost.
      Button.Name := ASU.To_Unbounded_String("");
      Button.Parent := null;
   end Remove_Button;


   ------------------------------------------------------------------------
   procedure Remove_Child (W     : in out Container;
                           Child : in Asl.Tk.Widget_Class) is
   begin
      Remove_Button(W, Asl.Tk.Menuitem.Widget_Class(Child));
   end Remove_Child;


   ------------------------------------------------------------------------
   procedure Add_Button (W      : in out Widget;
                         Button : in Asl.Tk.Menuitem.Widget_Class;
                         Index  : in Natural := Last_Position) is
   begin
      if (ASU.Length(W.My_Container.Name) = 0) then
         W.My_Container.Name := W.Name;
      end if;
      Add_Button(W.My_Container.all, Button, Index);
   end Add_Button;


   ------------------------------------------------------------------------
   procedure Remove_Button (W      : in out Widget;
                            Button : in Asl.Tk.Menuitem.Widget_Class) is
   begin
      Remove_Button(W.My_Container.all, Button);
   end Remove_Button;


   ------------------------------------------------------------------------
   function Get_Index (W      : in Container;
                       Button : in Asl.Tk.Menuitem.Widget_Class)
                       return Positive is
      Iter   : Child_List.Iterator;
      Found  : Boolean;
   begin
      Child_List.Set_Container
        (Iter, Child_List_Base.Object_Class(W.Children));
      Child_List.Search(Iter, Asl.Tk.Menuitem.Widget_Class(Button),
                        Found);
      if (not Found) then
         raise Child_Not_Found;
      end if;

      return Child_List.Get_Loc(Iter);
   end Get_Index;


   ------------------------------------------------------------------------
   function Get_Iterator (W : in Container)
                          return Asl.Tk.Child_Iterator_Class is
      Retval : Child_Iterator_Ptr;
   begin
      Retval := new Child_Iterator;
      Child_List.Set_Container
        (Retval.Iterator,
         Child_List_Base.Object_Class(W.Children));

      return Asl.Tk.Child_Iterator_Class(Retval);
   end Get_Iterator;


   ------------------------------------------------------------------------
   function Get_Iterator (W : in Widget)
                          return Asl.Tk.Child_Iterator_Class is
   begin
      return Get_Iterator(W.My_Container.all);
   end Get_Iterator;


   ------------------------------------------------------------------------
   procedure Free_Iterator (Iter : access Child_Iterator) is
      To_Free : Child_Iterator_Ptr := Child_Iterator_Ptr(Iter);
   begin
      Free(To_Free);
   end Free_Iterator;


   ------------------------------------------------------------------------
   procedure First (Iter   : in out Child_Iterator;
                    At_End : out Boolean) is
      Is_End : Child_List_Base.End_Marker;
   begin
      Child_List.First(Iter.Iterator, Is_End);
      if (Is_End = Child_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end First;


   ------------------------------------------------------------------------
   procedure Last (Iter   : in out Child_Iterator;
                   At_End : out Boolean) is
      Is_End : Child_List_Base.End_Marker;
   begin
      Child_List.Last(Iter.Iterator, Is_End);
      if (Is_End = Child_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end Last;


   ------------------------------------------------------------------------
   procedure Next (Iter   : in out Child_Iterator;
                   At_End : out Boolean) is
      Is_End : Child_List_Base.End_Marker;
   begin
      Child_List.Next(Iter.Iterator, Is_End);
      if (Is_End = Child_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end Next;


   ------------------------------------------------------------------------
   procedure Prev (Iter   : in out Child_Iterator;
                   At_End : out Boolean) is
      Is_End : Child_List_Base.End_Marker;
   begin
      Child_List.Prev(Iter.Iterator, Is_End);
      if (Is_End = Child_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end Prev;


   ------------------------------------------------------------------------
   function Get (Iter : in Child_Iterator)
                 return Asl.Tk.Widget_Class is
   begin
      return Asl.Tk.Widget_Class(Child_List.Get(Iter.Iterator));
   end Get;


   ------------------------------------------------------------------------
   procedure Shutdown (W : in out Container) is
   begin
      for J in 1 .. Child_List.Member_Count(W.Children.all) loop
         Child_List.Delete_At(W.Children.all, 1);
      end loop;
   end Shutdown;

end Asl.Tk.Menuitem.Cascade;
