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
with Ada.Command_Line;

with Test_P; use type Test_P.Test1_Con.End_Marker;
with Test_P.Heap; use type Test_P.Heap.Test1_Heap.Object;

procedure Test_Heap is
   package Thp1 renames Test_P.Test1_Con;
   package Thp1_Heap renames Test_P.Heap.Test1_Heap;
   package Thp1_Heap_Dyn renames Test_P.Heap.Test1_Heap_Dyn;
   package Thp1_Heap_Fix renames Test_P.Heap.Test1_Heap_Fix;
   package Thp1_Heap_Exp renames Test_P.Heap.Test1_Heap_Exp;

   Heap_Error : exception;

   procedure Test_Heap (H1 : in Thp1_Heap.Object_Class;
                        H2 : in Thp1_Heap.Object_Class;
                        I1 : in Thp1_Heap.Iterator_Class) is
      Is_End : Thp1.End_Marker;
      Count  : Natural;
      Found  : Boolean;
      Rv     : Integer;
   begin
      Thp1.Set_Container(Thp1.Iterator'Class(I1.all), Thp1.Object_Class(H1));
      Test_P.Verify_Empty;

      -- Adding Elements To heap
      Thp1_Heap.Add(H1.all, 1);
      Test_P.Verify_Add(1);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 2);
      Test_P.Verify_Add(2);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, -3);
      Test_P.Verify_Add(-3);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 3);
      Test_P.Verify_Add(3);
      Thp1_Heap.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((1, 2, -3, 3),
                                       I1.all,
                                       H1.all);


      -- Deleting the whole heap
      Thp1_Heap.First(I1.all, Is_End);
      Count := 0;
      while (Is_End = Thp1.Not_Past_End) loop
         Thp1_Heap.Delete(I1.all, Is_End);
         Thp1_Heap.Verify_Integrity(H1.all);
         Count := Count + 1;
      end loop;
      if (Count /= 4) then
         raise Heap_Error;
      end if;
      if (Thp1_Heap.Member_Count(H1.all) /= 0) then
         raise Heap_Error;
      end if;
      Test_P.Verify_Delete_List((1, 2, -3, 3));

      -- Adding 50 then deleting it
      Thp1_Heap.Add(H1.all, 50);
      Test_P.Verify_Add(50);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.First(I1.all, Is_End);
      Thp1_Heap.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(50);
      if (Thp1_Heap.Member_Count(H1.all) /= 0) then
         raise Heap_Error;
      end if;

      -- Miscellaneous adds
      Thp1_Heap.Add(H1.all, 101);
      Test_P.Verify_Add(101);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 102);
      Test_P.Verify_Add(102);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 103);
      Test_P.Verify_Add(103);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 104);
      Test_P.Verify_Add(104);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 105);
      Test_P.Verify_Add(105);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 106);
      Test_P.Verify_Add(106);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 107);
      Test_P.Verify_Add(107);
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Add(H1.all, 108);
      Test_P.Verify_Add(108);
      Thp1_Heap.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((101, 102, 103, 104,
                                        105, 106, 107, 108),
                                       I1.all,
                                       H1.all);

      -- Searching for some values
      Thp1_Heap.Search(I1.all, 144, Found);
      if (Found) then
         raise Heap_Error;
      end if;

      Thp1_Heap.Search(I1.all, 106, Found);
      if (not Found) then
         raise Heap_Error;
      end if;

      -- Invalidate the iterator
      Thp1_Heap.Add(H1.all, 109);
      Test_P.Verify_Add(109);
      Thp1_Heap.Verify_Integrity(H1.all);


      begin
         Thp1_Heap.Next(I1.all, Is_End);
         Put_Line("invalid iterator operation failed, no exception");
         raise Heap_Error;
      exception
         when Thp1.Object_Updated => null;
      end;

      if (Thp1_Heap.Member_Count(H1.all) /= 9) then
         raise Heap_Error;
      end if;

      Test_P.Verify_Contents_Unordered((101, 102, 103, 104,
                                        105, 106, 107, 108, 109),
                                       I1.all,
                                       H1.all);


      -- Checking and removing top element
      if (Thp1_Heap.Get_Head(H1.all) /= 109) then
         raise Heap_Error;
      end if;
      Thp1_Heap.Remove_Head(H1.all, Rv);
      if (Rv /= 109) then
         raise Heap_Error;
      end if;
      Test_P.Verify_Delete(109);
      Thp1_Heap.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((101, 102, 103, 104,
                                        105, 106, 107, 108),
                                       I1.all,
                                       H1.all);

      -- Deleting element 104
      Thp1_Heap.Search(I1.all, 104, Found);
      Thp1_Heap.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(104);
      Thp1_Heap.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((101, 102, 103,
                                        105, 106, 107, 108),
                                       I1.all,
                                       H1.all);

      -- Adding to the object about to be assigned to
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Add(H2.all, 987);
      Test_P.Verify_Add(987);
      Thp1_Heap.Verify_Integrity(H2.all);

      if (H2.all = H1.all) then
         raise Heap_Error;
      end if;

      -- Attempting a copy
      H2.all := H1.all;
      Test_P.Verify_Delete(987);
      Test_P.Verify_Copied_List((101, 102, 103, 105, 106, 107, 108));
      Thp1_Heap.Verify_Integrity(H1.all);
      Thp1_Heap.Verify_Integrity(H2.all);

      Thp1.Set_Container(Thp1.Iterator'Class(I1.all), Thp1.Object_Class(H2));
      Test_P.Verify_Contents_Unordered((101, 102, 103,
                                        105, 106, 107, 108),
                                       I1.all,
                                       H2.all);

      if (H2.all /= H1.all) then
         raise Heap_Error;
      end if;

      -- Getting element off the heap in order
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 108) then raise Heap_Error; end if;
      Test_P.Verify_Delete(108);
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 107) then raise Heap_Error; end if;
      Test_P.Verify_Delete(107);
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 106) then raise Heap_Error; end if;
      Test_P.Verify_Delete(106);
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 105) then raise Heap_Error; end if;
      Test_P.Verify_Delete(105);
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 103) then raise Heap_Error; end if;
      Test_P.Verify_Delete(103);
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 102) then raise Heap_Error; end if;
      Test_P.Verify_Delete(102);
      Thp1_Heap.Verify_Integrity(H2.all);
      Thp1_Heap.Remove_Head(H2.all, Rv);
      if (Rv /= 101) then raise Heap_Error; end if;
      Test_P.Verify_Delete(101);
      Thp1_Heap.Verify_Integrity(H2.all);

      if (Thp1_Heap.Member_Count(H2.all) /= 0) then
         raise Heap_Error;
      end if;

      Test_P.Verify_Empty;
   end Test_Heap;

   H1_Dyn_Ptr : Thp1_Heap_Dyn.Object_Ptr := new Thp1_Heap_Dyn.Object;
   H2_Dyn_Ptr : Thp1_Heap_Dyn.Object_Ptr := new Thp1_Heap_Dyn.Object;

   H1_Fix_Ptr : Thp1_Heap_Fix.Object_Ptr
     := new Thp1_Heap_Fix.Object(Size => 20);
   H2_Fix_Ptr : Thp1_Heap_Fix.Object_Ptr
     := new Thp1_Heap_Fix.Object(Size => 20);

   H1_Exp_Ptr : Thp1_Heap_Exp.Object_Ptr
     := new Thp1_Heap_Exp.Object(Initial_Size => 5,
                                 Increment    => 10);
   H2_Exp_Ptr : Thp1_Heap_Exp.Object_Ptr
     := new Thp1_Heap_Exp.Object(Initial_Size => 5,
                                 Increment    => 10);

   I1_Ptr : Thp1.Iterator_Class;

   Cb1    : Thp1.Callbacks_Class
     := new Test_P.Print_Val(new String'("H1"));

begin
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Dyn_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Dyn_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Fix_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Fix_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Exp_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Exp_Ptr.all), Cb1);

   -- Testing Dynamic Heaps
   I1_Ptr := Thp1_Heap_Dyn.New_Iterator(H1_Dyn_Ptr);
   Test_Heap(Thp1_Heap.Object_Class(H1_Dyn_Ptr),
             Thp1_Heap.Object_Class(H2_Dyn_Ptr),
             Thp1_Heap.Iterator_Class(I1_Ptr));
   Thp1.Free(I1_Ptr);
   -- Testing Fixed Heaps
   I1_Ptr := Thp1_Heap_Fix.New_Iterator(H1_Fix_Ptr);
   Test_Heap(Thp1_Heap.Object_Class(H1_Fix_Ptr),
             Thp1_Heap.Object_Class(H2_Fix_Ptr),
             Thp1_Heap.Iterator_Class(I1_Ptr));
   Thp1.Free(I1_Ptr);
   -- Testing Expandable Heaps
   I1_Ptr := Thp1_Heap_Exp.New_Iterator(H1_Exp_Ptr);
   Test_Heap(Thp1_Heap.Object_Class(H1_Exp_Ptr),
             Thp1_Heap.Object_Class(H2_Exp_Ptr),
             Thp1_Heap.Iterator_Class(I1_Ptr));
   Thp1.Free(I1_Ptr);

   -- Free all the data.
   Test_P.Heap.Free(H1_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108));
   Test_P.Heap.Free(H2_Dyn_Ptr);

   Test_P.Heap.Free(H1_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108));
   Test_P.Heap.Free(H2_Fix_Ptr);

   Test_P.Heap.Free(H1_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108));
   Test_P.Heap.Free(H2_Exp_Ptr);

   Test_P.Verify_Empty;

   Test_P.Check_Leaks;
   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Heap;
