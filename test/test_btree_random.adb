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
with Test_P.Btree;

procedure Test_Btree_Random is
   package Thp1 renames Test_P.Test1_Con;
   use type Thp1.End_Marker;
   package Thp1_Btree renames Test_P.Btree.Test1_Btree;
   use type Thp1_Btree.Iterator;
   package Thp1_Btree_Dyn renames Test_P.Btree.Test1_Btree_Dyn;
   package Integer_Vector renames Test_P.Ordered.Test1_Vec_Exp;

   package Random_Integer is new Ada.Numerics.Discrete_Random(Integer);

   Random_Seed  : Integer;
   Btree_Entries : Positive;

   Entries : Integer_Vector.Object_Ptr;

   Random_Generator : Random_Integer.Generator;

   Double_Entry, No_Entry : exception;
   Btree_Error : exception;

   procedure Test_Btree (H1 : in Thp1_Btree.Object_Class) is
      Delete_Pos    : Integer;
      Current_Entry : Integer;
      Found         : Boolean;
      New_Val       : Integer;
      Tval          : Integer;
      Is_End        : Thp1.End_Marker;
      It            : Thp1_Btree.Iterator_Class
        := Thp1_Btree.Iterator_Class(Thp1_Btree.New_Iterator(H1));
      Next_It       : Thp1_Btree.Iterator_Class
        := Thp1_Btree.Iterator_Class(Thp1_Btree.New_Iterator(H1));
      Next_Val      : Integer;
   begin
      Current_Entry := 0;
      Thp1_Btree.Verify_Integrity(H1.all);
      while (Current_Entry < Btree_Entries) loop
         New_Val := Random_Integer.Random(Random_Generator);
         begin
            Thp1_Btree.Add(H1.all, New_Val);

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
         Thp1_Btree.Verify_Integrity(H1.all);
      end loop;

      for I in 1 .. Btree_Entries * 10 loop
         Tval := Random_Integer.Random(Random_Generator);
         if (((Tval mod 2) = 0)
             and (Thp1_Btree.Member_Count(H1.all) > 0))
         then
            -- Delete an element if the count > 0.
            Delete_Pos := Random_Integer.Random(Random_Generator);
            Delete_Pos := (Delete_Pos mod Current_Entry) + 1;
            Thp1_Btree.Search(It.all,
                             Integer_Vector.Get_At(Entries.all, Delete_Pos),
                             Found);
            if (not Found) then
               raise Btree_Error;
            end if;
            Next_It.all := It.all;
            Thp1_Btree.Next(Next_It.all, Is_End);
            if (Is_End = Thp1.Past_End) then
               Thp1_Btree.Delete(It.all, Is_End);
               if (Is_End /= Thp1.Past_End) then
                  raise Btree_Error;
               end if;
            else
               Next_Val := Thp1_Btree.Get(Next_It.all);
               Thp1_Btree.Delete(It.all, Is_End);
               if (Is_End = Thp1.Past_End) then
                  raise Btree_Error;
               end if;
               if (Next_Val /= It.all) then
                  raise Btree_Error;
               end if;
            end if;

            Thp1_Btree.Verify_Integrity(H1.all);
            Integer_Vector.Delete_At(Entries.all, Delete_Pos);
            Current_Entry := Current_Entry - 1;
         else
            begin
               New_Val := Random_Integer.Random(Random_Generator);
               Thp1_Btree.Add(H1.all, New_Val);

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
            Thp1_Btree.Verify_Integrity(H1.all);
         end if;
      end loop;

      -- Now delete all the elements
      while (Current_Entry > 0) loop
         Delete_Pos := Random_Integer.Random(Random_Generator);
         Delete_Pos := (Delete_Pos mod Current_Entry) + 1;
         Thp1_Btree.Search(It.all,
                          Integer_Vector.Get_At(Entries.all, Delete_Pos),
                          Found);
         if (not Found) then
            raise Btree_Error;
         end if;

         Next_It.all := It.all;
         Thp1_Btree.Next(Next_It.all, Is_End);
         if (Is_End = Thp1.Past_End) then
            Thp1_Btree.Delete(It.all, Is_End);
            if (Is_End /= Thp1.Past_End) then
               raise Btree_Error;
            end if;
         else
            Next_Val := Thp1_Btree.Get(Next_It.all);
            Thp1_Btree.Delete(It.all, Is_End);
            if (Is_End = Thp1.Past_End) then
               raise Btree_Error;
            end if;
            if (Next_Val /= It.all) then
               raise Btree_Error;
            end if;
         end if;

         Thp1_Btree.Verify_Integrity(H1.all);
         Integer_Vector.Delete_At(Entries.all, Delete_Pos);
         Current_Entry := Current_Entry - 1;
      end loop;

      Thp1_Btree.Free(It);
      Thp1_Btree.Free(Next_It);
   end Test_Btree;

   H1_Dyn_Ptr : Thp1_Btree_Dyn.Object_Ptr;

begin
   if (Ada.Command_Line.Argument_Count = 0) then
      -- Generate my own seed and count.
      Random_Integer.Reset(Random_Generator);
      Random_Seed := Random_Integer.Random(Random_Generator);
      Btree_Entries
        := ((abs Random_Integer.Random(Random_Generator)) mod 500) + 1;
   elsif (Ada.Command_Line.Argument_Count = 2) then
      Random_Seed := Integer'Value(Ada.Command_Line.Argument(1));
      Btree_Entries := Positive'Value(Ada.Command_Line.Argument(2));
   else
      Put_Line("Random seed and btree size not supplied");
      return;
   end if;

   Random_Integer.Reset(Random_Generator, Random_Seed);

   Entries := new Integer_Vector.Object(Btree_Entries, Btree_Entries);

   H1_Dyn_Ptr := new Thp1_Btree_Dyn.Object(Allow_Duplicates => False,
                                           Node_Size        => 4);

   Test_Btree(Thp1_Btree.Object_Class(H1_Dyn_Ptr));

   Test_P.Btree.Free(H1_Dyn_Ptr);

   Test_P.Ordered.Free(Entries);

   -- Checking for leaks
   Test_P.Check_Leaks;

   Put_Line("Tests passed with seed="
            & Integer'Image(Random_Seed)
            & ", size="
            & Integer'Image(Btree_Entries));
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed with seed="
               & Integer'Image(Random_Seed)
               & ", size="
               & Integer'Image(Btree_Entries));
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Btree_Random;
