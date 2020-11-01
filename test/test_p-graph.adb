-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
--
-- This code is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This code is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--

with Text_IO; use Text_IO;
with Ada.Unchecked_Deallocation;

package body Test_P.Graph is

   type Op_Item;
   type Op_Item_Ptr is access all Op_Item;
   type Op_Item is record
      Val  : Integer;
      Next : Op_Item_Ptr;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation(Op_Item, Op_Item_Ptr);

   Add_List : Op_Item_Ptr := null;
   Delete_List : Op_Item_Ptr := null;
   Copied_List : Op_Item_Ptr := null;

   procedure Added (Cb  : access Print_Val;
                    O   : in Test1_Graph.Object'Class;
                    Val : in out Integer) is
      New_Op : Op_Item_Ptr := new Op_Item'(Val => Val, Next => Add_List);
   begin
      Add_List := New_Op;
   end Added;

   procedure Copied (Cb  : access Print_Val;
                     O   : in Test1_Graph.Object'Class;
                     Val : in out Integer) is
      New_Op : Op_Item_Ptr := new Op_Item'(Val => Val, Next => Copied_List);
   begin
      Copied_List := New_Op;
   end Copied;

   procedure Deleted (Cb  : access Print_Val;
                      O   : in Test1_Graph.Object'Class;
                      Val : in out Integer) is
      New_Op : Op_Item_Ptr := new Op_Item'(Val => Val, Next => Delete_List);
   begin
      Delete_List := New_Op;
   end Deleted;

   procedure Verify_Add (Val : in Integer) is
      Curr : Op_Item_Ptr := Add_List;
      Prev : Op_Item_Ptr;
   begin
      if (Curr = null) then
         raise Add_Not_Found;
      elsif (Curr.Val = Val) then
         Add_List := Curr.Next;
         Free(Curr);
      else
         Prev := Curr;
         Curr := Curr.Next;
         while ((Curr /= null) and then (Curr.Val /= Val)) loop
            Prev := Curr;
            Curr := Curr.Next;
         end loop;
         if (Curr = null) then
            raise Add_Not_Found;
         else
            Prev.Next := Curr.Next;
            Free(Curr);
         end if;
      end if;
   end Verify_Add;


   procedure Verify_Delete (Val : in Integer) is
      Curr : Op_Item_Ptr := Delete_List;
      Prev : Op_Item_Ptr;
   begin
      if (Curr = null) then
         raise Delete_Not_Found;
      elsif (Curr.Val = Val) then
         Delete_List := Curr.Next;
         Free(Curr);
      else
         Prev := Curr;
         Curr := Curr.Next;
         while ((Curr /= null) and then (Curr.Val /= Val)) loop
            Prev := Curr;
            Curr := Curr.Next;
         end loop;
         if (Curr = null) then
            raise Delete_Not_Found;
         else
            Prev.Next := Curr.Next;
            Free(Curr);
         end if;
      end if;
   end Verify_Delete;


   procedure Verify_Copied (Val : in Integer) is
      Curr : Op_Item_Ptr := Copied_List;
      Prev : Op_Item_Ptr;
   begin
      if (Curr = null) then
         raise Copied_Not_Found;
      elsif (Curr.Val = Val) then
         Copied_List := Curr.Next;
         Free(Curr);
      else
         Prev := Curr;
         Curr := Curr.Next;
         while ((Curr /= null) and then (Curr.Val /= Val)) loop
            Prev := Curr;
            Curr := Curr.Next;
         end loop;
         if (Curr = null) then
            raise Copied_Not_Found;
         else
            Prev.Next := Curr.Next;
            Free(Curr);
         end if;
      end if;
   end Verify_Copied;


   procedure Verify_Empty is
   begin
      if ((Add_List /= null)
          or (Delete_List /= null)
          or (Copied_List /= null))
      then
         raise Not_Empty;
      end if;
   end Verify_Empty;


   procedure Verify_Add_List (Vals : in Integer_Array) is
   begin
      for I in Vals'Range loop
         Verify_Add(Vals(I));
      end loop;
   end Verify_Add_List;


   procedure Verify_Delete_List (Vals : in Integer_Array) is
   begin
      for I in Vals'Range loop
         Verify_Delete(Vals(I));
      end loop;
   end Verify_Delete_List;


   procedure Verify_Copied_List (Vals : in Integer_Array) is
   begin
      for I in Vals'Range loop
         Verify_Copied(Vals(I));
      end loop;
   end Verify_Copied_List;


   procedure Verify_Contents (Vals  : in Node_Entry_Array;
                              Iter1 : in out Test1_Graph.Iterator'Class;
                              Iter2 : in out Test1_Graph.Iterator'Class;
                              O     : in Test1_Graph.Object'Class) is
      Found   : Boolean;
      To_Free : IAP;
   begin
      if (Test1_Graph.Member_Count(O) /= Vals'Length) then
         raise Invalid_Contents;
      end if;

      for I in Vals'Range loop
         Test1_Graph.Search(Iter1, Vals(I).Val, Found);
         if (not Found) then
            raise Invalid_Contents;
         end if;

         if (Vals(I).Links = null) then
            if (Test1_Graph.Link_Count(Iter1) /= 0) then
               raise Invalid_Contents;
            end if;
         else
            if (Test1_Graph.Link_Count(Iter1) /= Vals(I).Links'Length) then
               raise Invalid_Contents;
            end if;

            for J in Vals(I).Links'Range loop
               Test1_Graph.Search(Iter2, Vals(I).Links(J).Link, Found);
               if (not Found) then
                  raise Invalid_Contents;
               end if;

               Test1_Graph.Find_Link(Iter1, Iter2, Found);
               if (not Found) then
                  raise Invalid_Contents;
               end if;

               -- Search for all the links to the other graph for the
               -- specified value.
               while (Test1_Graph.Get_Link(Iter1) /= Vals(I).Links(J).Val) loop
                  Test1_Graph.Find_Link_Again(Iter1, Found);
                  if (not Found) then
                     raise Invalid_Contents;
                  end if;
               end loop;
            end loop;

            To_Free := Vals(I).Links;
            Free(To_Free);
         end if;
      end loop;
   end Verify_Contents;
end Test_P.Graph;
