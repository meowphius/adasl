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
with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;

with Test_P.Tree; use type Test_P.Tree.Test1_Tree.Iterator;
with Test_P.Ordered;


procedure Test_Tree_Balanced_Random is
   package Ttp1 renames Test_P.Test1_Con;
   package Ttp1_Tree renames Test_P.Tree.Test1_Tree;
   package Ttp1_Tree_Dyn renames Test_P.Tree.Test1_Tree_Dyn;
   package Ttp1_Tree_Fix renames Test_P.Tree.Test1_Tree_Fix;
   package Ttp1_Tree_Exp renames Test_P.Tree.Test1_Tree_Exp;
   package Integer_Vector renames Test_P.Ordered.Test1_Vec_Exp;

   package Random_Integer is new Ada.Numerics.Discrete_Random(Integer);


   Random_Seed  : Integer;
   Tree_Entries : Positive;

   Entries : Integer_Vector.Object_Ptr;
   Entry_It : Integer_Vector.Iterator_Class;

   Random_Generator : Random_Integer.Generator;

   Double_Entry, No_Entry : exception;

   procedure Test_Tree (T1 : in Ttp1_Tree.Object_Class;
                        I1 : in Ttp1_Tree.Iterator_Class) is
      Delete_Pos    : Integer;
      Current_Entry : Integer;
      Found         : Boolean;
      New_Val       : Integer;
      Is_End        : Ttp1.End_Marker;
   begin
      Current_Entry := 0;
      Ttp1_Tree.Verify_Integrity(T1.all);
      Integer_Vector.First(Entry_It.all, Is_End);
      while (Current_Entry < Tree_Entries) loop
         New_Val := Random_Integer.Random(Random_Generator);
         begin
            Ttp1_Tree.Add(T1.all, New_Val);

            -- Better not find the entry in the vector
            for I in 1 .. Current_Entry loop
               if (Integer_Vector.Get_At(Entries.all, I) = New_Val) then
                  raise Double_Entry;
               end if;
            end loop;

            Integer_Vector.Add_After(Entry_It.all, New_Val);
            Current_Entry := Current_Entry + 1;
         exception
            when Ttp1.Item_Already_Exists =>
               -- Better find the entry in the vector
               Found := False;
               for I in 1 .. Current_Entry loop
                  if (Integer_Vector.Get_At(Entries.all, I) = New_Val) then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if (not Found) then
                  raise No_Entry;
               end if;
         end;
         Ttp1_Tree.Verify_Integrity(T1.all);
      end loop;

      -- Now delete all the elements
      while (Current_Entry > 0) loop
         Delete_Pos := Random_Integer.Random(Random_Generator);
         Delete_Pos := (Delete_Pos mod Current_Entry) + 1;
         Ttp1_Tree.Delete(T1.all,
                          Integer_Vector.Get_At(Entries.all, Delete_Pos));
         Ttp1_Tree.Verify_Integrity(T1.all);
         Integer_Vector.Delete_At(Entries.all, Delete_Pos);
         Current_Entry := Current_Entry - 1;
      end loop;
   end Test_Tree;

   T1_Dyn_Ptr : Ttp1_Tree_Dyn.Object_Ptr
     := new Ttp1_Tree_Dyn.Object(Balanced => True);
   I1_Dyn_Ptr : Ttp1.Iterator_Class := Ttp1_Tree_Dyn.New_Iterator(T1_Dyn_Ptr);

   T1_Fix_Ptr : Ttp1_Tree_Fix.Object_Ptr;
   I1_Fix_Ptr : Ttp1.Iterator_Class;

   T1_Exp_Ptr : Ttp1_Tree_Exp.Object_Ptr
     := new Ttp1_Tree_Exp.Object(Balanced     => True,
                                 Initial_Size => 10,
                                 Increment    => 10);
   I1_Exp_Ptr : Ttp1.Iterator_Class := Ttp1_Tree_Exp.New_Iterator(T1_Exp_Ptr);

begin

   if (Ada.Command_Line.Argument_Count = 0) then
      -- Generate my own seed and count.
      Random_Integer.Reset(Random_Generator);
      Random_Seed := Random_Integer.Random(Random_Generator);
      Tree_Entries
        := ((abs Random_Integer.Random(Random_Generator)) mod 500) + 1;
   elsif (Ada.Command_Line.Argument_Count = 2) then
      Random_Seed := Integer'Value(Ada.Command_Line.Argument(1));
      Tree_Entries := Positive'Value(Ada.Command_Line.Argument(2));
   else
      Put_Line("Random seed and tree size not supplied");
      return;
   end if;

   Random_Integer.Reset(Random_Generator, Random_Seed);

   Entries := new Integer_Vector.Object(Tree_Entries, 0);
   Entry_It := new Integer_Vector.Iterator'
     (Integer_Vector.New_Iterator(Integer_Vector.Object_Class(Entries)));

   T1_Fix_Ptr := new Ttp1_Tree_Fix.Object(Balanced => True,
                                          Size     => Tree_Entries * 2);
   I1_Fix_Ptr := Ttp1_Tree_Fix.New_Iterator(T1_Fix_Ptr);

   -- Testing Dynamic Trees
   Test_Tree(Ttp1_Tree.Object_Class(T1_Dyn_Ptr),
             Ttp1_Tree.Iterator_Class(I1_Dyn_Ptr));
   -- Testing Fixed Trees
   Test_Tree(Ttp1_Tree.Object_Class(T1_Fix_Ptr),
             Ttp1_Tree.Iterator_Class(I1_Fix_Ptr));
   -- Testing Expandable Trees
   Test_Tree(Ttp1_Tree.Object_Class(T1_Exp_Ptr),
             Ttp1_Tree.Iterator_Class(I1_Exp_Ptr));

   -- Freeing all the data
   Ttp1.Free(I1_Dyn_Ptr);
   Ttp1.Free(I1_Fix_Ptr);
   Ttp1.Free(I1_Exp_Ptr);
   Test_P.Tree.Free(T1_Dyn_Ptr);
   Test_P.Tree.Free(T1_Fix_Ptr);
   Test_P.Tree.Free(T1_Exp_Ptr);

   Test_P.Ordered.Free(Entries);
   Integer_Vector.Free(Entry_It);

   -- Checking for leaks
   Test_P.Check_Leaks;

   Put_Line("Tests passed with seed="
            & Integer'Image(Random_Seed)
            & ", size="
            & Integer'Image(Tree_Entries));
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed with seed="
               & Integer'Image(Random_Seed)
               & ", size="
               & Integer'Image(Tree_Entries));
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Tree_Balanced_Random;
