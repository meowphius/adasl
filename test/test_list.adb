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

with Test_P.List; use type Test_P.List.Test1_List.Iterator;
use type Test_P.Test1_Con.End_Marker;
use type Test_P.List.Test1_List.Object;

procedure Test_List is
   package Tlp1 renames Test_P.Test1_Con;
   package Tlp1_List renames Test_P.List.Test1_List;
   package Tlp1_List_Dyn renames Test_P.List.Test1_List_Dyn;
   package Tlp1_List_Fix renames Test_P.List.Test1_List_Fix;
   package Tlp1_List_Exp renames Test_P.List.Test1_List_Exp;


   List_Error : exception;

   procedure Test_List (L1 : in Tlp1_List.Object_Class;
                        L2 : in Tlp1_List.Object_Class;
                        I1 : in Tlp1_List.Iterator_Class) is
      Is_End : Tlp1.End_Marker;
   begin
      Tlp1.Set_Container(Tlp1.Iterator'Class(I1.all), Tlp1.Object_Class(L1));
      Test_P.Verify_Empty;

      -- Adding Elements To List
      Tlp1_List.Add_Tail(L1.all, 1);
      Test_P.Verify_Add(1);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Tail(L1.all, 2);
      Test_P.Verify_Add(2);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Tail(L1.all, 3);
      Test_P.Verify_Add(3);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Tail(L1.all, -3);
      Test_P.Verify_Add(-3);
      Test_P.Verify_Contents_Ordered((1, 2, 3, -3),
                                     I1.all,
                                     L1.all);

      -- Adding more stuff to head
      Tlp1_List.Add_Head(L1.all, 9);
      Test_P.Verify_Add(9);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Head(L1.all, 0);
      Test_P.Verify_Add(0);
      Tlp1_List.Verify_Integrity(L1.all);
      Test_P.Verify_Contents_Ordered((0, 9, 1, 2, 3, -3),
                                     I1.all,
                                     L1.all);

      -- Delete the whole list
      Tlp1_List.First(I1.all, Is_End);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(0);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(9);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(1);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(2);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(3);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(-3);
      if (Tlp1_List.Member_Count(L1.all) /= 0) then
         raise List_Error;
      end if;

      -- Adding 50 at the head
      Tlp1_List.Add_Head(L1.all, 50);
      Test_P.Verify_Add(50);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.First(I1.all, Is_End);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(50);
      if (Tlp1_List.Member_Count(L1.all) /= 0) then
         raise List_Error;
      end if;

      -- Adding to the before of an empty list
      Tlp1_List.First(I1.all, Is_End);
      Tlp1_List.Add_Before(I1.all, 48);
      Test_P.Verify_Add(48);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(48);
      if (Tlp1_List.Member_Count(L1.all) /= 0) then
         raise List_Error;
      end if;

      -- Adding to the after of an empty list
      Tlp1_List.First(I1.all, Is_End);
      Tlp1_List.Add_After(I1.all, 49);
      Test_P.Verify_Add(49);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(49);
      if (Tlp1_List.Member_Count(L1.all) /= 0) then
         raise List_Error;
      end if;

      -- Miscellaneous adds
      Tlp1_List.First(I1.all, Is_End);
      Tlp1_List.Add_Before(I1.all, 101);
      Test_P.Verify_Add(101);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_After(I1.all, 102);
      Test_P.Verify_Add(102);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Before(I1.all, 103);
      Test_P.Verify_Add(103);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Next(I1.all, Is_End);
      Tlp1_List.Add_After(I1.all, 104);
      Test_P.Verify_Add(104);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Before(I1.all, 105);
      Test_P.Verify_Add(105);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Before(I1.all, 106);
      Test_P.Verify_Add(106);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Before(I1.all, 107);
      Test_P.Verify_Add(107);
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Add_Before(I1.all, 108);
      Test_P.Verify_Add(108);
      Tlp1_List.Verify_Integrity(L1.all);
      Test_P.Verify_Contents_Ordered((101, 103, 102, 108, 107, 106, 105, 104),
                                     I1.all,
                                     L1.all);

      -- Make the iterator stale.
      Tlp1_List.Add_Tail(L1.all, 109);
      Test_P.Verify_Add(109);
      Tlp1_List.Verify_Integrity(L1.all);

      if (Tlp1_List.Member_Count(L1.all) /= 9) then
         raise List_Error;
      end if;

      begin
         Tlp1_List.Next(I1.all, Is_End);
         Put_Line("Test on stale iterator failed, no exception");
         raise List_Error;
      exception
         when Tlp1.Object_Updated => null;
      end;

      Test_P.Verify_Contents_Ordered((101, 103, 102, 108, 107,
                                      106, 105, 104, 109),
                                     I1.all,
                                     L1.all);

      -- Deleting 9th element
      Tlp1_List.First(I1.all, Is_End);
      for I in 1 .. 8 loop
         Tlp1_List.Next(I1.all, Is_End);
      end loop;
      Tlp1_List.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(109);
      Tlp1_List.Verify_Integrity(L1.all);

      Test_P.Verify_Contents_Ordered((101, 103, 102, 108, 107,
                                      106, 105, 104),
                                     I1.all,
                                     L1.all);

      -- Adding to the object about to be assigned to
      Tlp1_List.Verify_Integrity(L2.all);
      Tlp1_List.Add_Head(L2.all, 987);
      Test_P.Verify_Add(987);
      Tlp1_List.Verify_Integrity(L2.all);

      if (L2.all = L1.all) then
         raise List_Error;
      end if;

      -- Attempting a copy
      L2.all := L1.all;
      Test_P.Verify_Delete(987);
      Test_P.Verify_Copied_List((101, 103, 102, 108, 107, 106, 105, 104));
      Tlp1_List.Verify_Integrity(L1.all);
      Tlp1_List.Verify_Integrity(L2.all);

      Tlp1.Set_Container(Tlp1.Iterator'Class(I1.all), Tlp1.Object_Class(L2));
      Test_P.Verify_Contents_Ordered((101, 103, 102, 108, 107, 106, 105, 104),
                                     I1.all,
                                     L2.all);

      if (L2.all /= L1.all) then
         raise List_Error;
      end if;

      Test_P.Verify_Empty;
   end Test_List;


   L1_Dyn_Ptr : Tlp1_List_Dyn.Object_Ptr := new Tlp1_List_Dyn.Object;
   L2_Dyn_Ptr : Tlp1_List_Dyn.Object_Ptr := new Tlp1_List_Dyn.Object;

   L1_Fix_Ptr : Tlp1_List_Fix.Object_Ptr
     := new Tlp1_List_Fix.Object(Size => 100);
   L2_Fix_Ptr : Tlp1_List_Fix.Object_Ptr
     := new Tlp1_List_Fix.Object(Size => 100);

   L1_Exp_Ptr : Tlp1_List_Exp.Object_Ptr
     := new Tlp1_List_Exp.Object(Initial_Size => 10,
                                 Increment    => 10);
   L2_Exp_Ptr : Tlp1_List_Exp.Object_Ptr
     := new Tlp1_List_Exp.Object(Initial_Size => 10,
                                 Increment    => 10);

   I1_Ptr : Tlp1.Iterator_Class;

   Cb1        : Tlp1.Callbacks_Class
     := new Test_P.Print_Val(new String'("T1"));

begin
   Tlp1.Set_Callbacks(Tlp1.Object'Class(L1_Dyn_Ptr.all), Cb1);
   Tlp1.Set_Callbacks(Tlp1.Object'Class(L1_Fix_Ptr.all), Cb1);
   Tlp1.Set_Callbacks(Tlp1.Object'Class(L1_Exp_Ptr.all), Cb1);
   Tlp1.Set_Callbacks(Tlp1.Object'Class(L2_Dyn_Ptr.all), Cb1);
   Tlp1.Set_Callbacks(Tlp1.Object'Class(L2_Fix_Ptr.all), Cb1);
   Tlp1.Set_Callbacks(Tlp1.Object'Class(L2_Exp_Ptr.all), Cb1);

   -- Testing Dynamic Lists
   I1_Ptr := Tlp1_List_Dyn.New_Iterator(L1_Dyn_Ptr);
   Test_List(Tlp1_List.Object_Class(L1_Dyn_Ptr),
             Tlp1_List.Object_Class(L2_Dyn_Ptr),
             Tlp1_List.Iterator_Class(I1_Ptr));
   Tlp1.Free(I1_Ptr);

   -- Testing Fixed Lists
   I1_Ptr := Tlp1_List_Fix.New_Iterator(L1_Fix_Ptr);
   Test_List(Tlp1_List.Object_Class(L1_Fix_Ptr),
             Tlp1_List.Object_Class(L2_Fix_Ptr),
             Tlp1_List.Iterator_Class(I1_Ptr));
   Tlp1.Free(I1_Ptr);

   -- Testing Expandable Lists
   I1_Ptr := Tlp1_List_Exp.New_Iterator(L1_Exp_Ptr);
   Test_List(Tlp1_List.Object_Class(L1_Exp_Ptr),
             Tlp1_List.Object_Class(L2_Exp_Ptr),
             Tlp1_List.Iterator_Class(I1_Ptr));
   Tlp1.Free(I1_Ptr);

   Test_P.List.Free(L1_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 103, 102, 108, 107, 106, 105, 104));
   Test_P.List.Free(L2_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 103, 102, 108, 107, 106, 105, 104));

   Test_P.List.Free(L1_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 103, 102, 108, 107, 106, 105, 104));
   Test_P.List.Free(L2_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 103, 102, 108, 107, 106, 105, 104));

   Test_P.List.Free(L1_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 103, 102, 108, 107, 106, 105, 104));
   Test_P.List.Free(L2_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 103, 102, 108, 107, 106, 105, 104));

   Test_P.Verify_Empty;

   Test_P.Check_Leaks;
   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_List;
