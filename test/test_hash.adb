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
with Test_P.Hash; use type Test_P.Hash.Test1_Hash.Object;

procedure Test_Hash is
   package Thp1 renames Test_P.Test1_Con;
   package Thp1_Hash renames Test_P.Hash.Test1_Hash;
   package Thp1_Hash_Dyn renames Test_P.Hash.Test1_Hash_Dyn;
   package Thp1_Hash_Fix renames Test_P.Hash.Test1_Hash_Fix;
   package Thp1_Hash_Exp renames Test_P.Hash.Test1_Hash_Exp;

   Hash_Error : exception;

   procedure Test_Hash (H1   : in Thp1_Hash.Object_Class;
                        H2   : in Thp1_Hash.Object_Class;
                        I1   : in Thp1_Hash.Iterator_Class;
                        Dups : in Boolean) is
      Is_End : Thp1.End_Marker;
      Count  : Natural;
      Found  : Boolean;
   begin
      Thp1.Set_Container(Thp1.Iterator'Class(I1.all), Thp1.Object_Class(H1));
      Test_P.Verify_Empty;

      -- Adding Elements To table
      Thp1_Hash.Add(H1.all, 1);
      Test_P.Verify_Add(1);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 2);
      Test_P.Verify_Add(2);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, -3);
      Test_P.Verify_Add(-3);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 3);
      Test_P.Verify_Add(3);
      Thp1_Hash.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((1, 2, -3, 3),
                                       I1.all,
                                       H1.all);

      -- Check duplicate handling
      if (Dups) then
         Thp1_Hash.Add(H1.all, 3);
         Test_P.Verify_Add(3);
         Thp1_Hash.Search(I1.all, 3, Found);
         if (not Found) then
            raise Hash_Error;
         end if;
         Thp1_Hash.Search_Again(I1.all, Found);
         if (not Found) then
            raise Hash_Error;
         end if;
         Thp1_Hash.Delete(I1.all, Is_End);
         Test_P.Verify_Delete(3);
      else
         begin
            Thp1_Hash.Add(H1.all, 3);
            -- duplicates should not get here, they should raise an
            -- exception.
            raise Hash_Error;
         exception
            when Thp1.Item_Already_Exists => null;
         end;
      end if;


      -- Deleting the whole table
      Thp1_Hash.First(I1.all, Is_End);
      Count := 0;
      while (Is_End = Thp1.Not_Past_End) loop
         Thp1_Hash.Delete(I1.all, Is_End);
         Thp1_Hash.Verify_Integrity(H1.all);
         Count := Count + 1;
      end loop;
      if (Count /= 4) then
         raise Hash_Error;
      end if;
      if (Thp1_Hash.Member_Count(H1.all) /= 0) then
         raise Hash_Error;
      end if;
      Test_P.Verify_Delete_List((1, 2, -3, 3));

      -- Adding 50 then deleting it
      Thp1_Hash.Add(H1.all, 50);
      Test_P.Verify_Add(50);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.First(I1.all, Is_End);
      Thp1_Hash.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(50);
      if (Thp1_Hash.Member_Count(H1.all) /= 0) then
         raise Hash_Error;
      end if;

      -- Miscellaneous adds
      Thp1_Hash.Add(H1.all, 101);
      Test_P.Verify_Add(101);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 102);
      Test_P.Verify_Add(102);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 103);
      Test_P.Verify_Add(103);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 104);
      Test_P.Verify_Add(104);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 105);
      Test_P.Verify_Add(105);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 106);
      Test_P.Verify_Add(106);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 107);
      Test_P.Verify_Add(107);
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Add(H1.all, 108);
      Test_P.Verify_Add(108);
      Thp1_Hash.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((101, 102, 103, 104,
                                        105, 106, 107, 108),
                                       I1.all,
                                       H1.all);

      -- Searching for some values
      Thp1_Hash.Search(I1.all, 144, Found);
      if (Found) then
         raise Hash_Error;
      end if;

      Thp1_Hash.Search(I1.all, 106, Found);
      if (not Found) then
         raise Hash_Error;
      end if;

      -- Invalidate the iterator
      Thp1_Hash.Add(H1.all, 109);
      Test_P.Verify_Add(109);
      Thp1_Hash.Verify_Integrity(H1.all);


      begin
         Thp1_Hash.Next(I1.all, Is_End);
         Put_Line("invalid iterator operation failed, no exception");
         raise Hash_Error;
      exception
         when Thp1.Object_Updated => null;
      end;

      if (Thp1_Hash.Member_Count(H1.all) /= 9) then
         raise Hash_Error;
      end if;

      Test_P.Verify_Contents_Unordered((101, 102, 103, 104,
                                        105, 106, 107, 108, 109),
                                       I1.all,
                                       H1.all);
      -- Deleting element 104
      Thp1_Hash.Search(I1.all, 104, Found);
      Thp1_Hash.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(104);
      Thp1_Hash.Verify_Integrity(H1.all);
      Test_P.Verify_Contents_Unordered((101, 102, 103,
                                        105, 106, 107, 108, 109),
                                       I1.all,
                                       H1.all);

      -- Adding to the object about to be assigned to
      Thp1_Hash.Verify_Integrity(H2.all);
      Thp1_Hash.Add(H2.all, 987);
      Test_P.Verify_Add(987);
      Thp1_Hash.Verify_Integrity(H2.all);

      if (H2.all = H1.all) then
         raise Hash_Error;
      end if;

      -- Attempting a copy
      H2.all := H1.all;
      Test_P.Verify_Delete(987);
      Test_P.Verify_Copied_List((101, 102, 103, 105, 106, 107, 108, 109));
      Thp1_Hash.Verify_Integrity(H1.all);
      Thp1_Hash.Verify_Integrity(H2.all);

      Thp1.Set_Container(Thp1.Iterator'Class(I1.all), Thp1.Object_Class(H2));
      Test_P.Verify_Contents_Unordered((101, 102, 103,
                                        105, 106, 107, 108, 109),
                                       I1.all,
                                       H2.all);

      if (H2.all /= H1.all) then
         raise Hash_Error;
      end if;

      Test_P.Verify_Empty;
   end Test_Hash;

   H1_Dyn_Ptr : Thp1_Hash_Dyn.Object_Ptr
     := new Thp1_Hash_Dyn.Object(Allow_Duplicates => False,
                                 Size             => 20);
   H2_Dyn_Ptr : Thp1_Hash_Dyn.Object_Ptr
     := new Thp1_Hash_Dyn.Object(Allow_Duplicates => False,
                                 Size             => 20);

   H1_Fix_Ptr : Thp1_Hash_Fix.Object_Ptr
     := new Thp1_Hash_Fix.Object(Allow_Duplicates => False,
                                 Size             => 20);
   H2_Fix_Ptr : Thp1_Hash_Fix.Object_Ptr
     := new Thp1_Hash_Fix.Object(Allow_Duplicates => False,
                                 Size             => 20);

   H1_Exp_Ptr : Thp1_Hash_Exp.Object_Ptr
     := new Thp1_Hash_Exp.Object(Allow_Duplicates => False,
                                 Size             => 10,
                                 Max_Fill_Percent => 70);
   H2_Exp_Ptr : Thp1_Hash_Exp.Object_Ptr
     := new Thp1_Hash_Exp.Object(Allow_Duplicates => False,
                                 Size             => 10,
                                 Max_Fill_Percent => 70);

   H1_Dyn_Dup_Ptr : Thp1_Hash_Dyn.Object_Ptr
     := new Thp1_Hash_Dyn.Object(Allow_Duplicates => True,
                                 Size             => 20);
   H2_Dyn_Dup_Ptr : Thp1_Hash_Dyn.Object_Ptr
     := new Thp1_Hash_Dyn.Object(Allow_Duplicates => True,
                                 Size             => 20);

   H1_Fix_Dup_Ptr : Thp1_Hash_Fix.Object_Ptr
     := new Thp1_Hash_Fix.Object(Allow_Duplicates => True,
                                 Size             => 20);
   H2_Fix_Dup_Ptr : Thp1_Hash_Fix.Object_Ptr
     := new Thp1_Hash_Fix.Object(Allow_Duplicates => True,
                                 Size             => 20);

   H1_Exp_Dup_Ptr : Thp1_Hash_Exp.Object_Ptr
     := new Thp1_Hash_Exp.Object(Allow_Duplicates => True,
                                 Size             => 10,
                                 Max_Fill_Percent => 70);
   H2_Exp_Dup_Ptr : Thp1_Hash_Exp.Object_Ptr
     := new Thp1_Hash_Exp.Object(Allow_Duplicates => True,
                                 Size             => 10,
                                 Max_Fill_Percent => 70);

   I1_Ptr : Thp1.Iterator_Class;

   Cb1    : Thp1.Callbacks_Class
     := new Test_P.Print_Val(new String'("H1"));

begin
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Dyn_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Fix_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Exp_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Dyn_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Fix_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Exp_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Dyn_Dup_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Fix_Dup_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H1_Exp_Dup_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Dyn_Dup_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Fix_Dup_Ptr.all), Cb1);
   Thp1.Set_Callbacks(Thp1.Object'Class(H2_Exp_Dup_Ptr.all), Cb1);

   -- Testing Dynamic Hashs
   I1_Ptr := Thp1_Hash_Dyn.New_Iterator(H1_Dyn_Ptr);
   Test_Hash(Thp1_Hash.Object_Class(H1_Dyn_Ptr),
             Thp1_Hash.Object_Class(H2_Dyn_Ptr),
             Thp1_Hash.Iterator_Class(I1_Ptr),
             Dups => False);
   Thp1.Free(I1_Ptr);

   -- Testing Fixed Hashs
   I1_Ptr := Thp1_Hash_Fix.New_Iterator(H1_Fix_Ptr);
   Test_Hash(Thp1_Hash.Object_Class(H1_Fix_Ptr),
             Thp1_Hash.Object_Class(H2_Fix_Ptr),
             Thp1_Hash.Iterator_Class(I1_Ptr),
             Dups => False);
   Thp1.Free(I1_Ptr);

   -- Testing Expandable Hashs
   I1_Ptr := Thp1_Hash_Exp.New_Iterator(H1_Exp_Ptr);
   Test_Hash(Thp1_Hash.Object_Class(H1_Exp_Ptr),
             Thp1_Hash.Object_Class(H2_Exp_Ptr),
             Thp1_Hash.Iterator_Class(I1_Ptr),
             Dups => False);
   Thp1.Free(I1_Ptr);

   -- Testing Dynamic Hashs with duplicates
   I1_Ptr := Thp1_Hash_Dyn.New_Iterator(H1_Dyn_Dup_Ptr);
   Test_Hash(Thp1_Hash.Object_Class(H1_Dyn_Dup_Ptr),
             Thp1_Hash.Object_Class(H2_Dyn_Dup_Ptr),
             Thp1_Hash.Iterator_Class(I1_Ptr),
             Dups => True);
   Thp1.Free(I1_Ptr);

   -- Testing Fixed Hashs with duplicates
   I1_Ptr := Thp1_Hash_Fix.New_Iterator(H1_Fix_Dup_Ptr);
   Test_Hash(Thp1_Hash.Object_Class(H1_Fix_Dup_Ptr),
             Thp1_Hash.Object_Class(H2_Fix_Dup_Ptr),
             Thp1_Hash.Iterator_Class(I1_Ptr),
             Dups => True);
   Thp1.Free(I1_Ptr);

   -- Testing Expandable Hashs with duplicates
   I1_Ptr := Thp1_Hash_Exp.New_Iterator(H1_Exp_Dup_Ptr);
   Test_Hash(Thp1_Hash.Object_Class(H1_Exp_Dup_Ptr),
             Thp1_Hash.Object_Class(H2_Exp_Dup_Ptr),
             Thp1_Hash.Iterator_Class(I1_Ptr),
             Dups => True);
   Thp1.Free(I1_Ptr);

   Test_P.Hash.Free(H1_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));
   Test_P.Hash.Free(H2_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));

   Test_P.Hash.Free(H1_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));
   Test_P.Hash.Free(H2_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));

   Test_P.Hash.Free(H1_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));
   Test_P.Hash.Free(H2_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));

   Test_P.Hash.Free(H1_Dyn_Dup_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));
   Test_P.Hash.Free(H2_Dyn_Dup_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));

   Test_P.Hash.Free(H1_Fix_Dup_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));
   Test_P.Hash.Free(H2_Fix_Dup_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));

   Test_P.Hash.Free(H1_Exp_Dup_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));
   Test_P.Hash.Free(H2_Exp_Dup_Ptr);
   Test_P.Verify_Delete_List((101, 102, 103, 105, 106, 107, 108, 109));

   Test_P.Verify_Empty;

   Test_P.Check_Leaks;
   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Hash;
