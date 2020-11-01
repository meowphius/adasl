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

procedure Test_Sets is
   package Thp1 renames Test_P.Test1_Con;
   package Thp1_Setops renames Test_P.Test1_Setops;
   package Thp1_Hash renames Test_P.Hash.Test1_Hash;
   package Thp1_Hash_Dyn renames Test_P.Hash.Test1_Hash_Dyn;

   procedure Test_Set (H1 : in Thp1.Object_Class;
                       H2 : in Thp1.Object_Class;
                       H3 : in Thp1.Object_Class) is
      I1 : Thp1.Iterator_Class := Thp1.New_Iterator(H3);
   begin
      -- Adding Elements To Set 1
      Thp1.Add(H1.all, 1);
      Thp1.Verify_Integrity(H1.all);
      Thp1.Add(H1.all, 2);
      Thp1.Verify_Integrity(H1.all);
      Thp1.Add(H1.all, -3);
      Thp1.Verify_Integrity(H1.all);
      Thp1.Add(H1.all, 3);
      Thp1.Verify_Integrity(H1.all);

      -- Adding Elements To Set 2
      Thp1.Add(H2.all, 9);
      Thp1.Verify_Integrity(H2.all);
      Thp1.Add(H2.all, 2);
      Thp1.Verify_Integrity(H2.all);
      Thp1.Add(H2.all, -3);
      Thp1.Verify_Integrity(H2.all);
      Thp1.Add(H2.all, 3);
      Thp1.Verify_Integrity(H2.all);

      -- Doing a Union of sets 1 and 2
      Thp1_Setops.Union(H3, H1, H2);
      Thp1.Set_Container(I1.all, H3);
      Test_P.Verify_Contents_Unordered((1, 2, -3, 3, 9),
                                       I1.all,
                                       H3.all);

      -- Doing an intersection of sets 1 and 2
      Thp1_Setops.Intersection(H3, H1, H2);
      Thp1.Set_Container(I1.all, H3);
      Test_P.Verify_Contents_Unordered((2, -3, 3),
                                       I1.all,
                                       H3.all);

      Thp1.Free(I1);
   end Test_Set;

   procedure Test_Bag (H1 : in Thp1.Object_Class;
                       H2 : in Thp1.Object_Class;
                       H3 : in Thp1.Object_Class) is
      I1 : Thp1.Iterator_Class := Thp1.New_Iterator(H3);
   begin
      -- Adding Elements To Bag 1
      Thp1.Add(H1.all, 1);
      Thp1.Verify_Integrity(H1.all);
      Thp1.Add(H1.all, 2);
      Thp1.Verify_Integrity(H1.all);
      Thp1.Add(H1.all, -3);
      Thp1.Verify_Integrity(H1.all);
      Thp1.Add(H1.all, 3);
      Thp1.Verify_Integrity(H1.all);

      -- Adding Elements To Bag 2
      Thp1.Add(H2.all, 9);
      Thp1.Verify_Integrity(H2.all);
      Thp1.Add(H2.all, 2);
      Thp1.Verify_Integrity(H2.all);
      Thp1.Add(H2.all, -3);
      Thp1.Verify_Integrity(H2.all);
      Thp1.Add(H2.all, 3);
      Thp1.Verify_Integrity(H2.all);

      -- Doing a Union of bags 1 and 2
      Thp1_Setops.Bag_Union(H3, H1, H2);
      Test_P.Verify_Contents_Unordered((1, 2, 2, -3, -3, 3, 3, 9),
                                       I1.all,
                                       H3.all);

      -- Doing an intersection of bags 1 and 2
      Thp1_Setops.Bag_Intersection(H3, H1, H2);
      Test_P.Verify_Contents_Unordered((2, -3, 3),
                                       I1.all,
                                       H3.all);

      Thp1.Free(I1);
   end Test_Bag;

   H1_Dyn     : aliased Thp1_Hash_Dyn.Object(Allow_Duplicates => False,
                                             Size             => 20);
   H2_Dyn     : aliased Thp1_Hash_Dyn.Object(Allow_Duplicates => False,
                                             Size             => 20);
   H3_Dyn     : aliased Thp1_Hash_Dyn.Object(Allow_Duplicates => False,
                                             Size             => 20);
   B1_Dyn     : aliased Thp1_Hash_Dyn.Object(Allow_Duplicates => True,
                                             Size             => 20);
   B2_Dyn     : aliased Thp1_Hash_Dyn.Object(Allow_Duplicates => True,
                                             Size             => 20);
   B3_Dyn     : aliased Thp1_Hash_Dyn.Object(Allow_Duplicates => True,
                                             Size             => 20);

begin
   -- Testing Sets
   Test_Set(H1_Dyn'Unchecked_Access,
            H2_Dyn'Unchecked_Access,
            H3_Dyn'Unchecked_Access);

   -- Testing Bags
   Test_Bag(B1_Dyn'Unchecked_Access,
            B2_Dyn'Unchecked_Access,
            B3_Dyn'Unchecked_Access);

   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Sets;
