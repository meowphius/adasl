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

with Asl.Leak_Detect_Pool; use type Asl.Leak_Detect_Pool.End_Marker;
with System;
with System.Storage_Elements;
with Text_IO; use Text_IO;
with Ada.Unchecked_Deallocation;

package body Test_P is

   function Hash_Integer (Val : in Integer) return Natural is
   begin
      if (Val < 0) then
         return -Val;
      else
         return Val;
      end if;
   end Hash_Integer;


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
                    O   : in Test1_Con.Object'Class;
                    Val : in out Integer) is
      New_Op : Op_Item_Ptr := new Op_Item'(Val => Val, Next => Add_List);
   begin
      Add_List := New_Op;
   end Added;

   procedure Copied (Cb  : access Print_Val;
                     O   : in Test1_Con.Object'Class;
                     Val : in out Integer) is
      New_Op : Op_Item_Ptr := new Op_Item'(Val => Val, Next => Copied_List);
   begin
      Copied_List := New_Op;
   end Copied;

   procedure Deleted (Cb  : access Print_Val;
                      O   : in Test1_Con.Object'Class;
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


   procedure Verify_Contents_Ordered (Vals : in Integer_Array;
                                      Iter : in out Test1_Con.Iterator'Class;
                                      O    : in Test1_Con.Object'Class) is
      Is_End : Test1_Con.End_Marker;
   begin
      Test1_Con.First(Iter, Is_End);
      for I in Vals'Range loop
         if ((Is_End = Test1_Con.Past_End) or else (Vals(I) /= Iter)) then
            raise Incorrect_Contents;
         end if;
         Test1_Con.Next(Iter, Is_End);
      end loop;
   end Verify_Contents_Ordered;


   procedure Verify_Contents_Unordered (Vals : in Integer_Array;
                                        Iter : in out Test1_Con.Iterator'Class;
                                        O    : in Test1_Con.Object'Class) is
      Is_End : Test1_Con.End_Marker;
      Count  : Natural := 0;
   begin
      Test1_Con.First(Iter, Is_End);
      while (Is_End /= Test1_Con.Past_End) loop
         Count := Count + 1;
         Test1_Con.Next(Iter, Is_End);
      end loop;
      if (Count /= Vals'Length) then
         raise Incorrect_Contents;
      end if;

      for I in Vals'Range loop
         if (not Test1_Con.Value_Exists(O, Vals(I))) then
            raise Incorrect_Contents;
         end if;
      end loop;
   end Verify_Contents_Unordered;


   procedure Check_Leaks is
      Leak_It      : Asl.Leak_Detect_Pool.Iterator;
      Leak_Address : System.Address;
      Leak_Size    : System.Storage_Elements.Storage_Count;
      Is_End       : Asl.Leak_Detect_Pool.End_Marker;
      Leak_Count   : Natural := 0;
   begin
      Asl.Leak_Detect_Pool.First(My_Pool,
                                 Leak_It,
                                 Is_End,
                                 Leak_Address,
                                 Leak_Size);
      while (Is_End /= Asl.Leak_Detect_Pool.Past_End) loop
         Put_Line("Leaked"
                  & System.Storage_Elements.Storage_Count'Image(Leak_Size)
                  & " Bytes");
         Leak_Count := Leak_Count + 1;
         Asl.Leak_Detect_Pool.Next(Leak_It,
                                   Is_End,
                                   Leak_Address,
                                   Leak_Size);
      end loop;

      if (Leak_Count /= 0) then
         raise Leak_Found;
      end if;
   end Check_Leaks;

end Test_P;
