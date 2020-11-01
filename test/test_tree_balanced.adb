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

with Test_P.Tree; use type Test_P.Tree.Test1_Tree.Iterator;
use type Test_P.Tree.Test1_Tree.Object;

procedure Test_Tree_Balanced is
   package Ttp1 renames Test_P.Test1_Con;
   use type Ttp1.End_Marker;
   package Ttp1_Tree renames Test_P.Tree.Test1_Tree;
   package Ttp1_Tree_Dyn renames Test_P.Tree.Test1_Tree_Dyn;
   package Ttp1_Tree_Fix renames Test_P.Tree.Test1_Tree_Fix;
   package Ttp1_Tree_Exp renames Test_P.Tree.Test1_Tree_Exp;

   Tree_Error : exception;

   procedure Test_Tree (T1 : in Ttp1_Tree.Object_Class;
                        T2 : in Ttp1_Tree.Object_Class;
                        I1 : in Ttp1_Tree.Iterator_Class) is
      Is_End     : Ttp1.End_Marker;
   begin
      Ttp1.Set_Container(Ttp1.Iterator'Class(I1.all), Ttp1.Object_Class(T1));
      Test_P.Verify_Empty;

      -- Test the different balancing cases.
      Ttp1_Tree.Add(T1.all, 100);
      Test_P.Verify_Add(100);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 200);
      Test_P.Verify_Add(200);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 300);
      Test_P.Verify_Add(300);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        200
      --                     100   300
      --
      Ttp1_Tree.Add(T1.all, -100);
      Test_P.Verify_Add(-100);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        200
      --                     100   300
      --                 -100
      --
      Ttp1_Tree.Add(T1.all, -200);
      Test_P.Verify_Add(-200);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        200
      --                 -100           300
      --             -200    100
      --
      Ttp1_Tree.Add(T1.all, 150);
      Test_P.Verify_Add(150);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        100
      --                 -100           200
      --             -200            150   300
      --
      Ttp1_Tree.Add(T1.all, 125);
      Test_P.Verify_Add(125);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 175);
      Test_P.Verify_Add(175);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 250);
      Test_P.Verify_Add(250);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 400);
      Test_P.Verify_Add(400);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        100
      --                 -100              200
      --             -200            150        300
      --                          125   175  250   400
      --
      Ttp1_Tree.Add(T1.all, 160);
      Test_P.Verify_Add(160);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        150
      --                 100              200
      --             -100   125      175        300
      --         -200             160        250   400
      --
      Ttp1_Tree.Root(I1.all, Is_End);
      if (I1.all /= 150) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (I1.all /= 100) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (I1.all /= -100) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (I1.all /= -200) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (Is_End = Ttp1.Not_Past_End) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Right(I1.all, Is_End);
      if (Is_End = Ttp1.Not_Past_End) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Right(I1.all, Is_End);
      if (I1.all /= 125) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Right(I1.all, Is_End);
      if (I1.all /= 200) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (I1.all /= 175) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (I1.all /= 160) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (Is_End = Ttp1.Not_Past_End) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Right(I1.all, Is_End);
      if (Is_End = Ttp1.Not_Past_End) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Right(I1.all, Is_End);
      if (I1.all /= 300) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (I1.all /= 250) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Left(I1.all, Is_End);
      if (Is_End = Ttp1.Not_Past_End) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Up(I1.all, Is_End);
      Ttp1_Tree.Right(I1.all, Is_End);
      if (I1.all /= 400) then
         raise Tree_Error;
      end if;
      Ttp1_Tree.Right(I1.all, Is_End);
      if (Is_End = Ttp1.Not_Past_End) then
         raise Tree_Error;
      end if;

      Ttp1_Tree.Delete(T1.all, 125);
      Test_P.Verify_Delete(125);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        150
      --                 -100             200
      --             -200    100     175        300
      --                          160        250   400
      --
      Ttp1_Tree.Delete(T1.all, 175);
      Test_P.Verify_Delete(175);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Delete(T1.all, 250);
      Test_P.Verify_Delete(250);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Delete(T1.all, 160);
      Test_P.Verify_Delete(160);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        150
      --                 -100             300
      --             -200    100      200     400
      --
      Ttp1_Tree.Add(T1.all, 500);
      Test_P.Verify_Add(500);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 175);
      Test_P.Verify_Add(175);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Delete(T1.all, -200);
      Test_P.Verify_Delete(-200);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        150
      --                 -100             300
      --                     100      200     400
      --                          175             500
      --
      Ttp1_Tree.Delete(T1.all, -100);
      Test_P.Verify_Delete(-100);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        300
      --                  150         400
      --              100    200           500
      --                  175
      --
      Ttp1_Tree.Delete(T1.all, 300);
      Test_P.Verify_Delete(300);
      Ttp1_Tree.Verify_Integrity(T1.all);
      --
      --                        200
      --                  150         400
      --              100    175          500
      --

      Test_P.Verify_Contents_Ordered((100, 150, 175, 200, 400, 500),
                                     I1.all,
                                     T1.all);

      -- Adding more stuff
      Ttp1_Tree.Add(T1.all, 50);
      Test_P.Verify_Add(50);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 49);
      Test_P.Verify_Add(49);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 48);
      Test_P.Verify_Add(48);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 109);
      Test_P.Verify_Add(109);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 37);
      Test_P.Verify_Add(37);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Add(T1.all, 78);
      Test_P.Verify_Add(78);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Test_P.Verify_Contents_Ordered((37, 48, 49, 50, 78, 100, 109,
                                      150, 175, 200, 400, 500),
                                     I1.all,
                                     T1.all);

      Ttp1_Tree.Last(I1.all, Is_End);
      if (Ttp1_Tree.Get(I1.all) /= 500) then
         raise Tree_Error;
      end if;
      if (Ttp1_Tree.Member_Count(T1.all) /= 12) then
         raise Tree_Error;
      end if;

      -- Deleting 48
      Ttp1_Tree.Delete(T1.all, 48);
      Test_P.Verify_Delete(48);
      Ttp1_Tree.Verify_Integrity(T1.all);

      begin
         -- Attempting operation on stale iterator
         Ttp1_Tree.Next(I1.all, Is_End);
         Put_Line("operation on stale iterator failed, no exception");
         raise Tree_Error;
      exception
         when Ttp1.Object_Updated => null;
      end;

      Test_P.Verify_Contents_Ordered((37, 49, 50, 78, 100, 109,
                                      150, 175, 200, 400, 500),
                                     I1.all,
                                     T1.all);

      -- Deleting 9th element
      Ttp1_Tree.First(I1.all, Is_End);
      for I in 1 .. 8 loop
         Ttp1_Tree.Next(I1.all, Is_End);
      end loop;
      Ttp1_Tree.Delete(I1.all, Is_End);
      Test_P.Verify_Delete(200);
      Ttp1_Tree.Verify_Integrity(T1.all);
      Test_P.Verify_Contents_Ordered((37, 49, 50, 78, 100, 109,
                                      150, 175, 400, 500),
                                     I1.all,
                                     T1.all);

      -- Adding to the object about to be assigned to
      Ttp1_Tree.Verify_Integrity(T2.all);
      Ttp1_Tree.Add(T2.all, 987);
      Test_P.Verify_Add(987);
      Ttp1_Tree.Verify_Integrity(T2.all);

      if (T2.all = T1.all) then
         raise Tree_Error;
      end if;

      -- Attempting a copy
      T2.all := T1.all;
      Test_P.Verify_Delete(987);
      Test_P.Verify_Copied_List((37, 49, 50, 78, 100, 109,
                                 150, 175, 400, 500));
      Ttp1_Tree.Verify_Integrity(T1.all);
      Ttp1_Tree.Verify_Integrity(T2.all);

      Test_P.Verify_Contents_Ordered((37, 49, 50, 78, 100, 109,
                                      150, 175, 400, 500),
                                     I1.all,
                                     T1.all);

      Ttp1.Set_Container(Ttp1.Iterator'Class(I1.all), Ttp1.Object_Class(T2));
      Test_P.Verify_Contents_Ordered((37, 49, 50, 78, 100, 109,
                                      150, 175, 400, 500),
                                     I1.all,
                                     T2.all);

      if (T2.all /= T1.all) then
         raise Tree_Error;
      end if;
   end Test_Tree;

   T1_Dyn_Ptr : Ttp1_Tree_Dyn.Object_Ptr
     := new Ttp1_Tree_Dyn.Object(Balanced => True);
   T2_Dyn_Ptr : Ttp1_Tree_Dyn.Object_Ptr
     := new Ttp1_Tree_Dyn.Object(Balanced => True);
   I1_Dyn_Ptr : Ttp1.Iterator_Class := Ttp1_Tree_Dyn.New_Iterator(T1_Dyn_Ptr);

   T1_Fix_Ptr : Ttp1_Tree_Fix.Object_Ptr
     := new Ttp1_Tree_Fix.Object(Balanced => True,
                                 Size     => 100);
   T2_Fix_Ptr : Ttp1_Tree_Fix.Object_Ptr
     := new Ttp1_Tree_Fix.Object(Balanced => True,
                                 Size     => 100);
   I1_Fix_Ptr : Ttp1.Iterator_Class := Ttp1_Tree_Fix.New_Iterator(T1_Fix_Ptr);

   T1_Exp_Ptr : Ttp1_Tree_Exp.Object_Ptr
     := new Ttp1_Tree_Exp.Object(Balanced     => True,
                                 Initial_Size => 10,
                                 Increment    => 10);
   T2_Exp_Ptr : Ttp1_Tree_Exp.Object_Ptr
     := new Ttp1_Tree_Exp.Object(Balanced     => True,
                                 Initial_Size => 10,
                                 Increment    => 10);
   I1_Exp_Ptr : Ttp1.Iterator_Class := Ttp1_Tree_Exp.New_Iterator(T1_Exp_Ptr);

   Cb1        : Test_P.Print_Val_Ptr
     := new Test_P.Print_Val(new String'("T1"));
   Cb2        : Test_P.Print_Val_Ptr
     := new Test_P.Print_Val(new String'("T2"));

begin
   Ttp1.Set_Callbacks(Ttp1.Object'Class(T1_Dyn_Ptr.all),
                      Ttp1.Callbacks_Class(Cb1));
   Ttp1.Set_Callbacks(Ttp1.Object'Class(T1_Fix_Ptr.all),
                      Ttp1.Callbacks_Class(Cb1));
   Ttp1.Set_Callbacks(Ttp1.Object'Class(T1_Exp_Ptr.all),
                      Ttp1.Callbacks_Class(Cb1));
   Ttp1.Set_Callbacks(Ttp1.Object'Class(T2_Dyn_Ptr.all),
                      Ttp1.Callbacks_Class(Cb2));
   Ttp1.Set_Callbacks(Ttp1.Object'Class(T2_Fix_Ptr.all),
                      Ttp1.Callbacks_Class(Cb2));
   Ttp1.Set_Callbacks(Ttp1.Object'Class(T2_Exp_Ptr.all),
                      Ttp1.Callbacks_Class(Cb2));

   -- Testing Dynamic Trees
   Test_Tree(Ttp1_Tree.Object_Class(T1_Dyn_Ptr),
             Ttp1_Tree.Object_Class(T2_Dyn_Ptr),
             Ttp1_Tree.Iterator_Class(I1_Dyn_Ptr));
   -- Testing Fixed Trees
   Test_Tree(Ttp1_Tree.Object_Class(T1_Fix_Ptr),
             Ttp1_Tree.Object_Class(T2_Fix_Ptr),
             Ttp1_Tree.Iterator_Class(I1_Fix_Ptr));
   -- Testing Expandable Trees
   Test_Tree(Ttp1_Tree.Object_Class(T1_Exp_Ptr),
             Ttp1_Tree.Object_Class(T2_Exp_Ptr),
             Ttp1_Tree.Iterator_Class(I1_Exp_Ptr));
   Test_P.Verify_Empty;

   -- Freeing all the data
   Ttp1.Free(I1_Dyn_Ptr);
   Ttp1.Free(I1_Fix_Ptr);
   Ttp1.Free(I1_Exp_Ptr);
   Test_P.Tree.Free(T1_Dyn_Ptr);
   Test_P.Verify_Delete_List((37, 49, 50, 78, 100, 109,
                              150, 175, 400, 500));
   Test_P.Tree.Free(T2_Dyn_Ptr);
   Test_P.Verify_Delete_List((37, 49, 50, 78, 100, 109,
                              150, 175, 400, 500));
   Test_P.Tree.Free(T1_Fix_Ptr);
   Test_P.Verify_Delete_List((37, 49, 50, 78, 100, 109,
                              150, 175, 400, 500));
   Test_P.Tree.Free(T2_Fix_Ptr);
   Test_P.Verify_Delete_List((37, 49, 50, 78, 100, 109,
                              150, 175, 400, 500));
   Test_P.Tree.Free(T1_Exp_Ptr);
   Test_P.Verify_Delete_List((37, 49, 50, 78, 100, 109,
                              150, 175, 400, 500));
   Test_P.Tree.Free(T2_Exp_Ptr);
   Test_P.Verify_Delete_List((37, 49, 50, 78, 100, 109,
                              150, 175, 400, 500));

   Test_P.Verify_Empty;

   -- Checking for leaks
   Test_P.Check_Leaks;

   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Tree_Balanced;
