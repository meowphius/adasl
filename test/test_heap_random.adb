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

with Test_P;
with Test_P.Ordered;
with Test_P.Heap;

procedure Test_Heap_Random is
   package Thp1 renames Test_P.Test1_Con;
   package Thp1_Heap renames Test_P.Heap.Test1_Heap;
   package Thp1_Heap_Dyn renames Test_P.Heap.Test1_Heap_Dyn;
   package Thp1_Heap_Fix renames Test_P.Heap.Test1_Heap_Fix;
   package Thp1_Heap_Exp renames Test_P.Heap.Test1_Heap_Exp;
   package Integer_Vector renames Test_P.Ordered.Test1_Vec_Exp;

   package Random_Integer is new Ada.Numerics.Discrete_Random(Integer);


   Random_Seed  : Integer;
   Heap_Entries : Positive;

   Entries : Integer_Vector.Object_Ptr;

   Random_Generator : Random_Integer.Generator;

   Double_Entry, No_Entry : exception;
   Heap_Error : exception;

   procedure Test_Heap (H1 : in Thp1_Heap.Object_Class) is
      Delete_Pos    : Integer;
      Current_Entry : Integer;
      Found         : Boolean;
      New_Val       : Integer;
      Tval          : Integer;
      Is_End        : Thp1.End_Marker;
      It            : Thp1_Heap.Iterator_Class
        := Thp1_Heap.Iterator_Class(Thp1_Heap.New_Iterator(H1));
   begin
      Current_Entry := 0;
      Thp1_Heap.Verify_Integrity(H1.all);
      while (Current_Entry < Heap_Entries) loop
         New_Val := Random_Integer.Random(Random_Generator);
         begin
            Thp1_Heap.Add(H1.all, New_Val);

            -- Better not find the entry in the vector
            for I in 1 .. Current_Entry loop
               if (Integer_Vector.Get_At(Entries.all, I) = New_Val) then
                  raise Double_Entry;
               end if;
            end loop;

            Current_Entry := Current_Entry + 1;
            Integer_Vector.Add_At(Entries.all, Current_Entry, New_Val);
         exception
            when Thp1.Item_Already_Exists =>
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
         Thp1_Heap.Verify_Integrity(H1.all);
      end loop;

      for I in 1 .. Heap_Entries * 10 loop
         Tval := Random_Integer.Random(Random_Generator);
         if (((Tval mod 2) = 0)
             and (Thp1_Heap.Member_Count(H1.all) > 0))
         then
            -- Delete an element if the count > 0.
            Delete_Pos := Random_Integer.Random(Random_Generator);
            Delete_Pos := (Delete_Pos mod Current_Entry) + 1;
            Thp1_Heap.Search(It.all,
                             Integer_Vector.Get_At(Entries.all, Delete_Pos),
                             Found);
            if (not Found) then
               raise Heap_Error;
            end if;
            Thp1_Heap.Delete(It.all, Is_End);

            Thp1_Heap.Verify_Integrity(H1.all);
            Integer_Vector.Delete_At(Entries.all, Delete_Pos);
            Current_Entry := Current_Entry - 1;
         else
            begin
               New_Val := Random_Integer.Random(Random_Generator);
               Thp1_Heap.Add(H1.all, New_Val);

               -- Better not find the entry in the vector
               for I in 1 .. Current_Entry loop
                  if (Integer_Vector.Get_At(Entries.all, I) = New_Val) then
                     raise Double_Entry;
                  end if;
               end loop;

               Current_Entry := Current_Entry + 1;
               Integer_Vector.Add_At(Entries.all, Current_Entry, New_Val);
            exception
               when Thp1.Item_Already_Exists =>
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
            Thp1_Heap.Verify_Integrity(H1.all);
         end if;
      end loop;

      -- Now delete all the elements

      while (Current_Entry > 0) loop
         Delete_Pos := Random_Integer.Random(Random_Generator);
         Delete_Pos := (Delete_Pos mod Current_Entry) + 1;
         Thp1_Heap.Search(It.all,
                          Integer_Vector.Get_At(Entries.all, Delete_Pos),
                          Found);
         if (not Found) then
            raise Heap_Error;
         end if;
         Thp1_Heap.Delete(It.all, Is_End);
         Thp1_Heap.Verify_Integrity(H1.all);
         Integer_Vector.Delete_At(Entries.all, Delete_Pos);
         Current_Entry := Current_Entry - 1;
      end loop;

      Thp1_Heap.Free(It);
   end Test_Heap;

   H1_Dyn_Ptr : Thp1_Heap_Dyn.Object_Ptr;
   H1_Fix_Ptr : Thp1_Heap_Fix.Object_Ptr;
   H1_Exp_Ptr : Thp1_Heap_Exp.Object_Ptr;

begin
   if (Ada.Command_Line.Argument_Count = 0) then
      -- Generate my own seed and count.
      Random_Integer.Reset(Random_Generator);
      Random_Seed := Random_Integer.Random(Random_Generator);
      Heap_Entries
        := ((abs Random_Integer.Random(Random_Generator)) mod 500) + 2;
   elsif (Ada.Command_Line.Argument_Count = 2) then
      Random_Seed := Integer'Value(Ada.Command_Line.Argument(1));
      Heap_Entries := Positive'Value(Ada.Command_Line.Argument(2));
   else
      Put_Line("Random seed and heap size not supplied");
      return;
   end if;

   Random_Integer.Reset(Random_Generator, Random_Seed);

   Entries := new Integer_Vector.Object(Heap_Entries, Heap_Entries);

   H1_Fix_Ptr := new Thp1_Heap_Fix.Object(Size => Heap_Entries * 11);

   H1_Dyn_Ptr := new Thp1_Heap_Dyn.Object;

   H1_Exp_Ptr := new Thp1_Heap_Exp.Object(Initial_Size => Heap_Entries / 2,
                                          Increment    => Heap_Entries);

   Test_Heap(Thp1_Heap.Object_Class(H1_Dyn_Ptr));
   Test_Heap(Thp1_Heap.Object_Class(H1_Fix_Ptr));
   Test_Heap(Thp1_Heap.Object_Class(H1_Exp_Ptr));

   Test_P.Heap.Free(H1_Dyn_Ptr);
   Test_P.Heap.Free(H1_Fix_Ptr);
   Test_P.Heap.Free(H1_Exp_Ptr);

   Test_P.Ordered.Free(Entries);

   -- Checking for leaks
   Test_P.Check_Leaks;

   Put_Line("Tests passed with seed="
            & Integer'Image(Random_Seed)
            & ", size="
            & Integer'Image(Heap_Entries));
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed with seed="
               & Integer'Image(Random_Seed)
               & ", size="
               & Integer'Image(Heap_Entries));
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Heap_Random;
