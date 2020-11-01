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

with Ada.Unchecked_Deallocation;

package body Asl.Tk.Frame is

   procedure Free is new Ada.Unchecked_Deallocation (Child_Iterator,
                                                     Child_Iterator_Ptr);

   procedure Initialize (W : in out Widget) is
   begin
      W.Cmd := Frame_ASU;
      Child_List.Set_Callbacks(W.Children.all, W.Callbacks'Unchecked_Access);
      Container_List.Enqueue(All_Containers, W'Unchecked_Access);
   end Initialize;

   procedure Finalize (W : in out Widget) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Child_List.Object, Child_List.Object_Ptr);
   begin
      Free(W.Children);
      if (not In_Shutdown) then
         Container_List.Delete(All_Containers, W'Unchecked_Access);
      end if;

      -- Handle destroying widget if displayed
      Destroy(W);
   end Finalize;

   procedure Added (Cb  : access Frame_Callbacks;
                    O   : in Child_List_Base.Object'Class;
                    Val : in out Asl.Tk.Widget_Class) is
   begin
      Child_Added(Managed_Class(Val.Parent).Manager.all, Val);
   end Added;

   procedure Copied (Cb  : access Frame_Callbacks;
                     O   : in Child_List_Base.Object'Class;
                     Val : in out Asl.Tk.Widget_Class) is
   begin
      -- This should never be called.
      raise Program_Error;
   end Copied;


   procedure Deleted (Cb  : access Frame_Callbacks;
                      O   : in Child_List_Base.Object'Class;
                      Val : in out Asl.Tk.Widget_Class) is
   begin
      Child_Removed(Managed_Class(Val.Parent).Manager.all, Val);
   end Deleted;

   procedure Add_Child (W     : in out Widget;
                        Child : in Asl.Tk.Widget_Class) is
   begin
      Child_List.Enqueue(W.Children.all, Child);
   end Add_Child;

   procedure Remove_Child (W     : in out Widget;
                           Child : in Asl.Tk.Widget_Class) is
   begin
      Child_List.Delete(W.Children.all, Child);
   end Remove_Child;

   function Get_Iterator (W : in Widget)
                          return Asl.Tk.Child_Iterator_Class is
      Retval : Child_Iterator_Ptr;
   begin
      Retval := new Child_Iterator;
      Child_List.Set_Container(Retval.Iterator,
                               Child_List_Base.Object_Class(W.Children));

      return Asl.Tk.Child_Iterator_Class(Retval);
   end Get_Iterator;


   procedure Free_Iterator (Iter : access Child_Iterator) is
      To_Free : Child_Iterator_Ptr := Child_Iterator_Ptr(Iter);
   begin
      Free(To_Free);
   end Free_Iterator;


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

   function Get (Iter : in Child_Iterator)
                 return Asl.Tk.Widget_Class is
   begin
      return Child_List.Get(Iter.Iterator);
   end Get;


   ------------------------------------------------------------------------
   procedure Shutdown (W : in out Widget) is
   begin
      for J in 1 .. Child_List.Member_Count(W.Children.all) loop
         Child_List.Delete_At(W.Children.all, 1);
      end loop;
   end Shutdown;


end Asl.Tk.Frame;
