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
with Test_P.Graph.DiGraph; use type Test_P.Graph.Test1_Graph.Object;

procedure Test_DiGraph is
   package Tgp1 renames Test_P.Test1_Con;
   package Tgp1_Graph renames Test_P.Graph.Test1_Graph;
   package Tgp1_Graph_Dyn_Dyn renames Test_P.Graph.DiGraph.Test1_Graph_Dyn_Dyn;
   package Tgp1_Graph_Dyn_Fix renames Test_P.Graph.DiGraph.Test1_Graph_Dyn_Fix;
   package Tgp1_Graph_Dyn_Exp renames Test_P.Graph.DiGraph.Test1_Graph_Dyn_Exp;
   package Tgp1_Graph_Fix_Dyn renames Test_P.Graph.DiGraph.Test1_Graph_Fix_Dyn;
   package Tgp1_Graph_Fix_Fix renames Test_P.Graph.DiGraph.Test1_Graph_Fix_Fix;
   package Tgp1_Graph_Fix_Exp renames Test_P.Graph.DiGraph.Test1_Graph_Fix_Exp;
   package Tgp1_Graph_Exp_Dyn renames Test_P.Graph.DiGraph.Test1_Graph_Exp_Dyn;
   package Tgp1_Graph_Exp_Fix renames Test_P.Graph.DiGraph.Test1_Graph_Exp_Fix;
   package Tgp1_Graph_Exp_Exp renames Test_P.Graph.DiGraph.Test1_Graph_Exp_Exp;

   subtype IA is Test_P.Graph.IA;

   Graph_Error : exception;

   procedure Test_Graph (H1 : in Tgp1_Graph.Object_Class;
                         H2 : in Tgp1_Graph.Object_Class;
                         I1 : in Tgp1_Graph.Iterator_Class;
                         I2 : in Tgp1_Graph.Iterator_Class) is
      Is_End : Tgp1.End_Marker;
      Count  : Natural;
      Found  : Boolean;

      procedure Search (Iter : in out Tgp1_Graph.Iterator'Class;
                        Val  : in Integer) is
         Found : Boolean;
      begin
         Tgp1_Graph.Search(Iter, Val, Found);
         if (not Found) then
            raise Graph_Error;
         end if;
      end Search;

   begin
      Test_P.Verify_Empty;
      Test_P.Graph.Verify_Empty;

      Tgp1_Graph.Set_Container(I1.all, Tgp1.Object_Class(H1));
      Tgp1_Graph.Set_Container(I2.all, Tgp1.Object_Class(H1));

      -- Adding Elements To the graph
      Tgp1_Graph.Add(H1.all, 1);
      Test_P.Verify_Add(1);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 2);
      Test_P.Verify_Add(2);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, -3);
      Test_P.Verify_Add(-3);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 3);
      Test_P.Verify_Add(3);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Test_P.Graph.Verify_Contents(((1,    null),
                                    (2,    null),
                                    (-3,   null),
                                    (3,    null)),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Adding links to the graph
      Search(I1.all, 1);
      Search(I2.all, 2);
      Tgp1_Graph.Add_Link(I1.all, I2.all, 900);
      Test_P.Graph.Verify_Add(900);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Search(I2.all, -3);
      Tgp1_Graph.Add_Link(I1.all, I2.all, 901);
      Test_P.Graph.Verify_Add(901);
      Tgp1_Graph.Verify_Integrity(H1.all);
      if (not Tgp1_Graph.Link_Exists(H1.all, 1, -3)) then
         raise Graph_Error;
      end if;
      if (Tgp1_Graph.Link_Exists(H1.all, 2, 3)) then
         raise Graph_Error;
      end if;
      if (not Tgp1_Graph.Link_Exists(I1.all, 2)) then
         raise Graph_Error;
      end if;
      if (Tgp1_Graph.Link_Exists(2, I1.all)) then
         raise Graph_Error;
      end if;
      Test_P.Graph.Verify_Contents(((1,    new IA'((2, 900), (-3, 901))),
                                    (2,    null),
                                    (-3,   null),
                                    (3,    null)),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Attempt to add a duplicate link (#899)
      Search(I1.all, 1);
      Search(I2.all, -3);
      begin
         Tgp1_Graph.Add_Link(I1.all, I2.all, 899, Ignore_Dup => False);
         Put_Line("Adding duplicate didn't raise exception");
         raise Graph_Error;
      exception
         when Tgp1.Item_Already_Exists => null;
      end;

      -- Deleting link (#901)
      Search(I1.all, 1);
      Search(I2.all, -3);
      Tgp1_Graph.Find_Link(I1.all, I2.all, Found);
      if (not Found) then
         raise Graph_Error;
      end if;
      while (Tgp1_Graph.Get_Link(I1.all) /= 901) loop
         Tgp1_Graph.Find_Link_Again(I1.all, Found);
         if (not Found) then
            raise Graph_Error;
         end if;
      end loop;
      Tgp1_Graph.Delete_Link(I1.all, Is_End);
      Test_P.Graph.Verify_Delete(901);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Test_P.Graph.Verify_Contents(((1,    new IA'(1 => (2, 900))),
                                    (2,    null),
                                    (-3,   null),
                                    (3,    null)),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Now add link #899
      Search(I1.all, 1);
      Search(I2.all, -3);
      Tgp1_Graph.Add_Link(I1.all, I2.all, 899);
      Test_P.Graph.Verify_Add(899);
      Test_P.Graph.Verify_Contents(((1,    new IA'((2, 900), (-3, 899))),
                                    (2,    null),
                                    (-3,   null),
                                    (3,    null)),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Deleting the whole graph
      Tgp1_Graph.First(I1.all, Is_End);
      Count := 0;
      while (Is_End = Tgp1.Not_Past_End) loop
         Tgp1_Graph.Delete(I1.all, Is_End);
         Tgp1_Graph.Verify_Integrity(H1.all);
         Count := Count + 1;
      end loop;
      Test_P.Verify_Delete_List((1, 2, -3, 3));
      Test_P.Graph.Verify_Delete_List((899, 900));
      if (Count /= 4) then
         raise Graph_Error;
      end if;
      if (Tgp1_Graph.Member_Count(H1.all) /= 0) then
         raise Graph_Error;
      end if;

      -- Adding 50 then deleting it
      Tgp1_Graph.Add(H1.all, 50);
      Test_P.Verify_Add(50);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.First(I1.all, Is_End);
      Tgp1_Graph.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(50);
      if (Tgp1_Graph.Member_Count(H1.all) /= 0) then
         raise Graph_Error;
      end if;

      -- Miscellaneous adds
      Tgp1_Graph.Add(H1.all, 101);
      Test_P.Verify_Add(101);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 102);
      Test_P.Verify_Add(102);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 103);
      Test_P.Verify_Add(103);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 104);
      Test_P.Verify_Add(104);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 105);
      Test_P.Verify_Add(105);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 106);
      Test_P.Verify_Add(106);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 107);
      Test_P.Verify_Add(107);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Add(H1.all, 108);
      Test_P.Verify_Add(108);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Search(I1.all, 108);
      Search(I2.all, 106);
      Tgp1_Graph.Add_Link(I1.all, I2.all, 902);
      Test_P.Graph.Verify_Add(902);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Search(I2.all, 101);
      Tgp1_Graph.Add_Link(I2.all, I1.all, 903);
      Test_P.Graph.Verify_Add(903);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Search(I2.all, 102);
      Tgp1_Graph.Add_Link(I1.all, I2.all, 904);
      Test_P.Graph.Verify_Add(904);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Search(I1.all, 102);
      Search(I2.all, 103);
      Tgp1_Graph.Add_Link(I1.all, I2.all, 905);
      Test_P.Graph.Verify_Add(905);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Test_P.Graph.Verify_Contents(((101,  new IA'(1 => (108, 903))),
                                    (102,  new IA'(1 => (103, 905))),
                                    (103,  null),
                                    (104,  null),
                                    (105,  null),
                                    (106,  null),
                                    (107,  null),
                                    (108,  new IA'((106, 902), (102, 904)))),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Deleting link from 108 to 106
      Search(I1.all, 108);
      Search(I2.all, 106);
      Tgp1_Graph.Find_Link(I1.all, I2.all, Found);
      if (not Found) then
         raise Graph_Error;
      end if;
      Tgp1_Graph.Delete_Link(I1.all, Is_End);
      Test_P.Graph.Verify_Delete(902);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Test_P.Graph.Verify_Contents(((101,  new IA'(1 => (108, 903))),
                                    (102,  new IA'(1 => (103, 905))),
                                    (103,  null),
                                    (104,  null),
                                    (105,  null),
                                    (106,  null),
                                    (107,  null),
                                    (108,  new IA'(1 => (102, 904)))),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Searching for some values
      Tgp1_Graph.Search(I1.all, 144, Found);
      if (Found) then
         raise Graph_Error;
      end if;

      Tgp1_Graph.Search(I1.all, 106, Found);
      if (not Found) then
         raise Graph_Error;
      end if;

      -- Invalidate the iterator
      Tgp1_Graph.Add(H1.all, 109);
      Test_P.Verify_Add(109);
      Tgp1_Graph.Verify_Integrity(H1.all);

      if (Tgp1_Graph.Member_Count(H1.all) /= 9) then
         raise Graph_Error;
      end if;

      begin
         Tgp1_Graph.Next(I1.all, Is_End);
         Put_Line("No exception on stale iterator");
         raise Graph_Error;
      exception
         when Tgp1.Object_Updated => null;
      end;

      -- Deleting 103
      Search(I1.all, 103);
      Tgp1_Graph.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(103);
      Test_P.Graph.Verify_Delete(905);
      Tgp1_Graph.Verify_Integrity(H1.all);
      Test_P.Graph.Verify_Contents(((101,  new IA'(1 => (108, 903))),
                                    (102,  null),
                                    (104,  null),
                                    (105,  null),
                                    (106,  null),
                                    (107,  null),
                                    (108,  new IA'(1 => (102, 904))),
                                    (109,  null)),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      -- Adding to the object about to be assigned to
      Tgp1_Graph.Verify_Integrity(H2.all);
      Tgp1_Graph.Add(H2.all, 987);
      Test_P.Verify_Add(987);
      Tgp1_Graph.Verify_Integrity(H2.all);

      if (H2.all = H1.all) then
         raise Graph_Error;
      end if;

      -- Attempting a copy
      H2.all := H1.all;
      Test_P.Verify_Delete(987);
      Test_P.Verify_Copied_List((101, 102, 104, 105, 106, 107, 108, 109));
      Test_P.Graph.Verify_Copied_List((903, 904));
      Tgp1_Graph.Verify_Integrity(H1.all);
      Tgp1_Graph.Verify_Integrity(H2.all);

      Tgp1_Graph.Set_Container(I1.all, Tgp1.Object_Class(H2));
      Tgp1_Graph.Set_Container(I2.all, Tgp1.Object_Class(H2));
      Test_P.Graph.Verify_Contents(((101,  new IA'(1 => (108, 903))),
                                    (102,  null),
                                    (104,  null),
                                    (105,  null),
                                    (106,  null),
                                    (107,  null),
                                    (108,  new IA'(1 => (102, 904))),
                                    (109,  null)),
                                   I1.all,
                                   I2.all,
                                   H1.all);

      if (H2.all /= H1.all) then
         raise Graph_Error;
      end if;

      Test_P.Verify_Empty;
      Test_P.Graph.Verify_Empty;
   end Test_Graph;

   H1_Dyn_Dyn_Ptr : Tgp1_Graph_Dyn_Dyn.Object_Ptr
     := new Tgp1_Graph_Dyn_Dyn.Object(Hash_Size => 20);
   H2_Dyn_Dyn_Ptr : Tgp1_Graph_Dyn_Dyn.Object_Ptr
     := new Tgp1_Graph_Dyn_Dyn.Object(Hash_Size => 20);

   H1_Dyn_Fix_Ptr : Tgp1_Graph_Dyn_Fix.Object_Ptr
     := new Tgp1_Graph_Dyn_Fix.Object(Hash_Size => 20);
   H2_Dyn_Fix_Ptr : Tgp1_Graph_Dyn_Fix.Object_Ptr
     := new Tgp1_Graph_Dyn_Fix.Object(Hash_Size => 20);

   H1_Dyn_Exp_Ptr : Tgp1_Graph_Dyn_Exp.Object_Ptr
     := new Tgp1_Graph_Dyn_Exp.Object(Hash_Size => 20);
   H2_Dyn_Exp_Ptr : Tgp1_Graph_Dyn_Exp.Object_Ptr
     := new Tgp1_Graph_Dyn_Exp.Object(Hash_Size => 20);

   H1_Fix_Dyn_Ptr : Tgp1_Graph_Fix_Dyn.Object_Ptr
     := new Tgp1_Graph_Fix_Dyn.Object(Hash_Size => 20, Size => 30);
   H2_Fix_Dyn_Ptr : Tgp1_Graph_Fix_Dyn.Object_Ptr
     := new Tgp1_Graph_Fix_Dyn.Object(Hash_Size => 20, Size => 30);

   H1_Fix_Fix_Ptr : Tgp1_Graph_Fix_Fix.Object_Ptr
     := new Tgp1_Graph_Fix_Fix.Object(Hash_Size => 20, Size => 30);
   H2_Fix_Fix_Ptr : Tgp1_Graph_Fix_Fix.Object_Ptr
     := new Tgp1_Graph_Fix_Fix.Object(Hash_Size => 20, Size => 30);

   H1_Fix_Exp_Ptr : Tgp1_Graph_Fix_Exp.Object_Ptr
     := new Tgp1_Graph_Fix_Exp.Object(Hash_Size => 20, Size => 30);
   H2_Fix_Exp_Ptr : Tgp1_Graph_Fix_Exp.Object_Ptr
     := new Tgp1_Graph_Fix_Exp.Object(Hash_Size => 20, Size => 30);

   H1_Exp_Dyn_Ptr : Tgp1_Graph_Exp_Dyn.Object_Ptr
     := new Tgp1_Graph_Exp_Dyn.Object(Hash_Size    => 20,
                                      Initial_Size => 7,
                                      Increment    => 3);
   H2_Exp_Dyn_Ptr : Tgp1_Graph_Exp_Dyn.Object_Ptr
     := new Tgp1_Graph_Exp_Dyn.Object(Hash_Size    => 20,
                                      Initial_Size => 7,
                                      Increment    => 3);

   H1_Exp_Fix_Ptr : Tgp1_Graph_Exp_Fix.Object_Ptr
     := new Tgp1_Graph_Exp_Fix.Object(Hash_Size    => 20,
                                      Initial_Size => 7,
                                      Increment    => 3);
   H2_Exp_Fix_Ptr : Tgp1_Graph_Exp_Fix.Object_Ptr
     := new Tgp1_Graph_Exp_Fix.Object(Hash_Size    => 20,
                                      Initial_Size => 7,
                                      Increment    => 3);

   H1_Exp_Exp_Ptr : Tgp1_Graph_Exp_Exp.Object_Ptr
     := new Tgp1_Graph_Exp_Exp.Object(Hash_Size    => 20,
                                      Initial_Size => 7,
                                      Increment    => 3);
   H2_Exp_Exp_Ptr : Tgp1_Graph_Exp_Exp.Object_Ptr
     := new Tgp1_Graph_Exp_Exp.Object(Hash_Size    => 20,
                                      Initial_Size => 7,
                                      Increment    => 3);

   I1_Ptr : aliased Tgp1.Iterator_Class;
   I2_Ptr : aliased Tgp1.Iterator_Class;

   Cb1        : Tgp1.Callbacks_Class
     := new Test_P.Print_Val(new String'("H1"));
   LCb1       : Tgp1_Graph.Link_Callbacks_Class
     := new Test_P.Graph.Print_Val(new String'("L1"));

begin
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Dyn_Dyn_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Dyn_Fix_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Dyn_Exp_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Dyn_Dyn_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Dyn_Fix_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Dyn_Exp_Ptr.all), Cb1);

   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Dyn_Dyn_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Dyn_Fix_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Dyn_Exp_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Dyn_Dyn_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Dyn_Fix_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Dyn_Exp_Ptr.all),
                                 LCb1);

   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Fix_Dyn_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Fix_Fix_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Fix_Exp_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Fix_Dyn_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Fix_Fix_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Fix_Exp_Ptr.all), Cb1);

   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Fix_Dyn_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Fix_Fix_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Fix_Exp_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Fix_Dyn_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Fix_Fix_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Fix_Exp_Ptr.all),
                                 LCb1);

   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Exp_Dyn_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Exp_Fix_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H1_Exp_Exp_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Exp_Dyn_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Exp_Fix_Ptr.all), Cb1);
   Tgp1.Set_Callbacks(Tgp1.Object'Class(H2_Exp_Exp_Ptr.all), Cb1);

   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Exp_Dyn_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Exp_Fix_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H1_Exp_Exp_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Exp_Dyn_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Exp_Fix_Ptr.all),
                                 LCb1);
   Tgp1_Graph.Set_Link_Callbacks(Tgp1_Graph.Object'Class(H2_Exp_Exp_Ptr.all),
                                 LCb1);

   -- Testing Dynamic-Dynamic Graphs
   I1_Ptr := Tgp1_Graph_Dyn_Dyn.New_Iterator(H1_Dyn_Dyn_Ptr);
   I2_Ptr := Tgp1_Graph_Dyn_Dyn.New_Iterator(H1_Dyn_Dyn_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Dyn_Dyn_Ptr),
              Tgp1_Graph.Object_Class(H2_Dyn_Dyn_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);
   -- Testing Dynamic-Fixed Graphs
   I1_Ptr := Tgp1_Graph_Dyn_Fix.New_Iterator(H1_Dyn_Fix_Ptr);
   I2_Ptr := Tgp1_Graph_Dyn_Fix.New_Iterator(H1_Dyn_Fix_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Dyn_Fix_Ptr),
              Tgp1_Graph.Object_Class(H2_Dyn_Fix_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);
   -- Testing Dynamic-Expandable Graphs
   I1_Ptr := Tgp1_Graph_Dyn_Exp.New_Iterator(H1_Dyn_Exp_Ptr);
   I2_Ptr := Tgp1_Graph_Dyn_Exp.New_Iterator(H1_Dyn_Exp_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Dyn_Exp_Ptr),
              Tgp1_Graph.Object_Class(H2_Dyn_Exp_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);


   -- Testing Fixed-Dynamic Graphs
   I1_Ptr := Tgp1_Graph_Fix_Dyn.New_Iterator(H1_Fix_Dyn_Ptr);
   I2_Ptr := Tgp1_Graph_Fix_Dyn.New_Iterator(H1_Fix_Dyn_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Fix_Dyn_Ptr),
              Tgp1_Graph.Object_Class(H2_Fix_Dyn_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);
   -- Testing Fixed-Fixed Graphs
   I1_Ptr := Tgp1_Graph_Fix_Fix.New_Iterator(H1_Fix_Fix_Ptr);
   I2_Ptr := Tgp1_Graph_Fix_Fix.New_Iterator(H1_Fix_Fix_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Fix_Fix_Ptr),
              Tgp1_Graph.Object_Class(H2_Fix_Fix_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);
   -- Testing Fixed-Expandable Graphs
   I1_Ptr := Tgp1_Graph_Fix_Exp.New_Iterator(H1_Fix_Exp_Ptr);
   I2_Ptr := Tgp1_Graph_Fix_Exp.New_Iterator(H1_Fix_Exp_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Fix_Exp_Ptr),
              Tgp1_Graph.Object_Class(H2_Fix_Exp_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);


   -- Testing Expandable-Dynamic Graphs
   I1_Ptr := Tgp1_Graph_Exp_Dyn.New_Iterator(H1_Exp_Dyn_Ptr);
   I2_Ptr := Tgp1_Graph_Exp_Dyn.New_Iterator(H1_Exp_Dyn_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Exp_Dyn_Ptr),
              Tgp1_Graph.Object_Class(H2_Exp_Dyn_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);
   -- Testing Expandable-Fixed Graphs
   I1_Ptr := Tgp1_Graph_Exp_Fix.New_Iterator(H1_Exp_Fix_Ptr);
   I2_Ptr := Tgp1_Graph_Exp_Fix.New_Iterator(H1_Exp_Fix_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Exp_Fix_Ptr),
              Tgp1_Graph.Object_Class(H2_Exp_Fix_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);
   -- Testing Expandable-Expandable Graphs
   I1_Ptr := Tgp1_Graph_Exp_Exp.New_Iterator(H1_Exp_Exp_Ptr);
   I2_Ptr := Tgp1_Graph_Exp_Exp.New_Iterator(H1_Exp_Exp_Ptr);
   Test_Graph(Tgp1_Graph.Object_Class(H1_Exp_Exp_Ptr),
              Tgp1_Graph.Object_Class(H2_Exp_Exp_Ptr),
              Tgp1_Graph.Iterator_Class(I1_Ptr),
              Tgp1_Graph.Iterator_Class(I2_Ptr));
   Tgp1.Free(I1_Ptr);
   Tgp1.Free(I2_Ptr);

   -- Free all the data.
   Test_P.Graph.DiGraph.Free(H1_Dyn_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Dyn_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Dyn_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Dyn_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Dyn_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Dyn_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Fix_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Fix_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Fix_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Fix_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Fix_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Fix_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Exp_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Exp_Dyn_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Exp_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Exp_Fix_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Graph.DiGraph.Free(H1_Exp_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));
   Test_P.Graph.DiGraph.Free(H2_Exp_Exp_Ptr);
   Test_P.Verify_Delete_List((101, 102, 104, 105, 106, 107, 108, 109));
   Test_P.Graph.Verify_Delete_List((903, 904));

   Test_P.Verify_Empty;
   Test_P.Graph.Verify_Empty;

   Test_P.Check_Leaks;
   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_DiGraph;
