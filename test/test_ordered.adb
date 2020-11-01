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

with Test_P;
with Test_P.Ordered;
use type Test_P.Ordered.Test1_Ord.Object;
use type Test_P.Ordered.Test1_Ord.Iterator;

procedure Test_Ordered is
   package Tvp1 renames Test_P.Test1_Con;
   package Tvp1_Ord renames Test_P.Ordered.Test1_Ord;
   package Tvp1_Vec renames Test_P.Ordered.Test1_Vec;
   package Tvp1_Vec_Exp renames Test_P.Ordered.Test1_Vec_Exp;
   package Tvp1_Vec_Fix renames Test_P.Ordered.Test1_Vec_Fix;
   package Tvp1_Alist renames Test_P.Ordered.Test1_Alist;
   package Tvp1_Alist_Exp renames Test_P.Ordered.Test1_Alist_Exp;
   package Tvp1_Alist_Fix renames Test_P.Ordered.Test1_Alist_Fix;
   package Tvp1_Dlist renames Test_P.Ordered.Test1_Dlist;
   package Tvp1_Dlist_Exp renames Test_P.Ordered.Test1_Dlist_Exp;
   package Tvp1_Dlist_Fix renames Test_P.Ordered.Test1_Dlist_Fix;
   package Tvp1_Dlist_Dyn renames Test_P.Ordered.Test1_Dlist_Dyn;

   Ord_Error : exception;

   procedure Test_Ord (V1 : in Tvp1_Ord.Object_Class;
                       V2 : in Tvp1_Ord.Object_Class;
                       I1 : in Tvp1_Ord.Iterator_Class) is
      I2 : Tvp1_Ord.Iterator_Class
        := Tvp1_Ord.Iterator_Class(Tvp1_Ord.New_Iterator(V1));
      Is_End : Tvp1.End_Marker;
   begin
      Tvp1.Set_Container(Tvp1.Iterator'Class(I1.all), Tvp1.Object_Class(V1));
      Test_P.Verify_Empty;

      Tvp1_Ord.First(I1.all, Is_End);
      -- Adding Elements To Ord
      Tvp1_Ord.Add_After(I1.all, 1);
      Test_P.Verify_Add(1);
      Tvp1_Ord.Add_After(I1.all, 2);
      Test_P.Verify_Add(2);
      Tvp1_Ord.Add_After(I1.all, 3);
      Test_P.Verify_Add(3);
      Tvp1_Ord.Add_After(I1.all, -3);
      Test_P.Verify_Add(-3);
      Tvp1_Ord.Add_After(I1.all, 9);
      Test_P.Verify_Add(9);
      Tvp1_Ord.Add_After(I1.all, 0);
      Test_P.Verify_Add(0);
      Tvp1_Ord.Verify_Integrity(V1.all);
      Test_P.Verify_Contents_Ordered((1, 2, 3, -3, 9, 0),
                                     I1.all,
                                     V1.all);

      -- Adding stuff to the beginning
      Tvp1_Ord.First(I1.all, Is_End);
      Tvp1_Ord.Add_Before(I1.all, 50);
      Test_P.Verify_Add(50);
      Tvp1_Ord.Add_Before(I1.all, 49);
      Test_P.Verify_Add(49);
      Tvp1_Ord.Add_Before(I1.all, 48);
      Test_P.Verify_Add(48);
      Tvp1_Ord.Add_Before(I1.all, 109);
      Test_P.Verify_Add(109);
      Tvp1_Ord.Add_Before(I1.all, 37);
      Test_P.Verify_Add(37);
      Tvp1_Ord.Add_Before(I1.all, 78);
      Test_P.Verify_Add(78);
      Tvp1_Ord.Verify_Integrity(V1.all);
      Test_P.Verify_Contents_Ordered((78, 37, 109, 48, 49, 50,
                                      1, 2, 3, -3, 9, 0),
                                     I1.all,
                                     V1.all);

      -- Changing 3rd element
      Tvp1_Ord.First(I1.all, Is_End);
      Tvp1_Ord.Next(I1.all, Is_End);
      Tvp1_Ord.Next(I1.all, Is_End);
      Tvp1_Ord.Set(I1.all, 888);
      Test_P.Verify_Delete(109);
      Test_P.Verify_Add(888);
      Tvp1_Ord.Verify_Integrity(V1.all);
      Test_P.Verify_Contents_Ordered((78, 37, 888, 48, 49, 50,
                                      1, 2, 3, -3, 9, 0),
                                     I1.all,
                                     V1.all);

      Tvp1_Ord.First(I1.all, Is_End);
      Tvp1_Ord.Next(I1.all, Is_End);
      Tvp1_Ord.Next(I1.all, Is_End);
      if (Tvp1_Ord.Get(I1.all) /= 888) then
         raise Ord_Error;
      end if;

      -- Adding 5 to position
      I1.all := I1.all + 5;
      Tvp1_Ord.Verify_Integrity(V1.all);
      if (Tvp1_Ord.Get_Loc(I1.all) /= 8) then
         raise Ord_Error;
      end if;

      -- Subtracting 7 from position
      I1.all := I1.all - 7;
      Tvp1_Ord.Verify_Integrity(V1.all);
      if (Tvp1_Ord.Get_Loc(I1.all) /= 1) then
         raise Ord_Error;
      end if;

      -- Changing 9th element
      Tvp1_Ord.Set_At(V1.all, 9, 777);
      Test_P.Verify_Add(777);
      Test_P.Verify_Delete(3);
      Tvp1_Ord.Verify_Integrity(V1.all);
      Test_P.Verify_Contents_Ordered((78, 37, 888, 48, 49, 50,
                                      1, 2, 777, -3, 9, 0),
                                     I1.all,
                                     V1.all);

      -- Adding an 8th element
      Tvp1_Ord.Add_At(V1.all, 8, 666);
      Test_P.Verify_Add(666);
      Tvp1_Ord.Verify_Integrity(V1.all);
      Test_P.Verify_Contents_Ordered((78, 37, 888, 48, 49, 50,
                                      1, 666, 2, 777, -3, 9, 0),
                                     I1.all,
                                     V1.all);

      begin
         Tvp1_Ord.Add_At(V1.all, 15, 999);
         Put_Line("Error, invalid add didn't raise exception");
         raise Ord_Error;
      exception
         when Constraint_Error => null;
      end;

      begin
         Tvp1_Ord.Set_At(V1.all, 14, 998);
         Put_Line("Error, invalid add didn't raise exception");
         raise Ord_Error;
      exception
         when Constraint_Error => null;
      end;
      Tvp1_Ord.Verify_Integrity(V1.all);

      Tvp1_Ord.Last(I1.all, Is_End);
      if (Tvp1_Ord.Get(I1.all) /= 0) then
         raise Ord_Error;
      end if;

      Tvp1_Ord.Set_Loc(I1.all, 4);
      if (Tvp1_Ord.Get(I1.all) /= 48) then
         raise Ord_Error;
      end if;

      if (Tvp1_Ord.Member_Count(V1.all) /= 13) then
         raise Ord_Error;
      end if;

      -- Deleting 4th element
      Tvp1_Ord.First(I2.all, Is_End);
      Tvp1_Ord.Next(I2.all, Is_End);
      Tvp1_Ord.Next(I2.all, Is_End);
      Tvp1_Ord.Next(I2.all, Is_End);
      Tvp1_Ord.Delete(I2.all, Is_End);
      Test_P.Verify_Delete(48);
      Tvp1_Ord.Verify_Integrity(V1.all);
      begin
         Tvp1_Ord.Next(I1.all, Is_End);
         Put_Line("Error, no exception on stale iterator");
      exception
         when Tvp1.Object_Updated => null;
      end;

      -- Deleting 9th element
      Tvp1_Ord.Delete_At(V1.all, 9);
      Test_P.Verify_Delete(777);
      Tvp1_Ord.Verify_Integrity(V1.all);

      Test_P.Verify_Contents_Ordered((78, 37, 888, 49, 50,
                                      1, 666, 2, -3, 9, 0),
                                     I1.all,
                                     V1.all);

      -- Adding to the object about to be assigned to
      Tvp1_Ord.Verify_Integrity(V2.all);
      Tvp1_Ord.Add_At(V2.all, 1, 37849);
      Test_P.Verify_Add(37849);
      Tvp1_Ord.Verify_Integrity(V2.all);

      if (V2.all = V1.all) then
         raise Ord_Error;
      end if;
      Tvp1_Ord.Verify_Integrity(V1.all);
      Tvp1_Ord.Verify_Integrity(V2.all);

      -- Attempting a copy
      V2.all := V1.all;
      Test_P.Verify_Delete(37849);
      Test_P.Verify_Copied_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
      Tvp1_Ord.Verify_Integrity(V1.all);
      Tvp1_Ord.Verify_Integrity(V2.all);

      Tvp1.Set_Container(Tvp1.Iterator'Class(I1.all), Tvp1.Object_Class(V2));
      Test_P.Verify_Contents_Ordered((78, 37, 888, 49, 50,
                                      1, 666, 2, -3, 9, 0),
                                     I1.all,
                                     V2.all);


      if (V2.all /= V1.all) then
         raise Ord_Error;
      end if;

      if (Tvp1_Ord.Member_Count(V1.all) /= 11) then
         raise Ord_Error;
      end if;

      Tvp1_Ord.Free(I2);

      Test_P.Verify_Empty;
   end Test_Ord;

   V1_Exp_Ptr : Tvp1_Vec_Exp.Object_Ptr := new Tvp1_Vec_Exp.Object(10, 10);
   V2_Exp_Ptr : Tvp1_Vec_Exp.Object_Ptr := new Tvp1_Vec_Exp.Object(10, 10);

   V1_Fix_Ptr : Tvp1_Vec_Fix.Object_Ptr := new Tvp1_Vec_Fix.Object(20);
   V2_Fix_Ptr : Tvp1_Vec_Fix.Object_Ptr := new Tvp1_Vec_Fix.Object(20);

   A1_Exp_Ptr : Tvp1_Alist_Exp.Object_Ptr
     := new Tvp1_Alist_Exp.Object(10, 10);
   A2_Exp_Ptr : Tvp1_Alist_Exp.Object_Ptr
     := new Tvp1_Alist_Exp.Object(10, 10);

   A1_Fix_Ptr : Tvp1_Alist_Fix.Object_Ptr := new Tvp1_Alist_Fix.Object(20);
   A2_Fix_Ptr : Tvp1_Alist_Fix.Object_Ptr := new Tvp1_Alist_Fix.Object(20);

   D1_Exp_Ptr : Tvp1_Dlist_Exp.Object_Ptr
     := new Tvp1_Dlist_Exp.Object(10, 10);
   D2_Exp_Ptr : Tvp1_Dlist_Exp.Object_Ptr
     := new Tvp1_Dlist_Exp.Object(10, 10);

   D1_Fix_Ptr : Tvp1_Dlist_Fix.Object_Ptr := new Tvp1_Dlist_Fix.Object(20);
   D2_Fix_Ptr : Tvp1_Dlist_Fix.Object_Ptr := new Tvp1_Dlist_Fix.Object(20);

   D1_Dyn_Ptr : Tvp1_Dlist_Dyn.Object_Ptr := new Tvp1_Dlist_Dyn.Object;
   D2_Dyn_Ptr : Tvp1_Dlist_Dyn.Object_Ptr := new Tvp1_Dlist_Dyn.Object;

   I1_Ptr : Tvp1.Iterator_Class;

   Cb1    : Tvp1.Callbacks_Class
     := new Test_P.Print_Val(new String'("H1"));

begin
   Tvp1.Set_Callbacks(Tvp1.Object'Class(V1_Exp_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(V2_Exp_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(V1_Fix_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(V2_Fix_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(A1_Exp_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(A2_Exp_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(A1_Fix_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(A2_Fix_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(D1_Exp_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(D2_Exp_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(D1_Fix_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(D2_Fix_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(D1_Dyn_Ptr.all), Cb1);
   Tvp1.Set_Callbacks(Tvp1.Object'Class(D2_Dyn_Ptr.all), Cb1);

   -- Testing Expandable Vector
   I1_Ptr := Tvp1_Vec_Exp.New_Iterator(V1_Exp_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(V1_Exp_Ptr),
            Tvp1_Ord.Object_Class(V2_Exp_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   -- Testing Fixed Vector
   I1_Ptr := Tvp1_Vec_Fix.New_Iterator(V1_Fix_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(V1_Fix_Ptr),
            Tvp1_Ord.Object_Class(V2_Fix_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   -- Testing Expandable Alist
   I1_Ptr := Tvp1_Alist_Exp.New_Iterator(A1_Exp_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(A1_Exp_Ptr),
            Tvp1_Ord.Object_Class(A2_Exp_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   -- Testing Fixed Alist
   I1_Ptr := Tvp1_Alist_Fix.New_Iterator(A1_Fix_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(A1_Fix_Ptr),
            Tvp1_Ord.Object_Class(A2_Fix_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   -- Testing Expandable Dlist
   I1_Ptr := Tvp1_Dlist_Exp.New_Iterator(D1_Exp_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(D1_Exp_Ptr),
            Tvp1_Ord.Object_Class(D2_Exp_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   -- Testing Fixed Dlist
   I1_Ptr := Tvp1_Dlist_Fix.New_Iterator(D1_Fix_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(D1_Fix_Ptr),
            Tvp1_Ord.Object_Class(D2_Fix_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   -- Testing Dynamic Dlist
   I1_Ptr := Tvp1_Dlist_Dyn.New_Iterator(D1_Dyn_Ptr);
   Test_Ord(Tvp1_Ord.Object_Class(D1_Dyn_Ptr),
            Tvp1_Ord.Object_Class(D2_Dyn_Ptr),
            Tvp1_Ord.Iterator_Class(I1_Ptr));
   Tvp1.Free(I1_Ptr);

   Test_P.Ordered.Free(V1_Exp_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(V2_Exp_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(V1_Fix_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(V2_Fix_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));

   Test_P.Ordered.Free(A1_Exp_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(A2_Exp_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(A1_Fix_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(A2_Fix_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));

   Test_P.Ordered.Free(D1_Exp_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(D2_Exp_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(D1_Fix_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(D2_Fix_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(D1_Dyn_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));
   Test_P.Ordered.Free(D2_Dyn_Ptr);
   Test_P.Verify_Delete_List((78, 37, 888, 49, 50, 1, 666, 2, -3, 9, 0));

   Test_P.Verify_Empty;

   Test_P.Check_Leaks;
   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Ordered;
