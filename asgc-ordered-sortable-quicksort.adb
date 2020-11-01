-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This library is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--

with Ada.Numerics.Discrete_Random;

package body Asgc.Ordered.Sortable.Quicksort is

   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);

   Random_Generator : Random_Natural.Generator;

   function Random (Min, Max : in Positive)
                    return Positive is
   begin
      return ((Random_Natural.Random(Random_Generator) mod (Max - Min + 1))
              + Min);
   end Random;

   procedure Bubble (Left  : in out Iterator'Class;
                     Right : in out Iterator'Class) is
      Left_Pos    : Positive := Get_Loc(Left);
      Right_Pos   : Positive := Get_Loc(Right);
      Value_Moved : Boolean := True;
      Is_End      : End_Marker;
   begin
      while (Value_Moved) loop
         Value_Moved := False;
         Set_Loc(Left, Left_Pos);
         Set_Loc(Right, Left_Pos + 1);
         for I in Left_Pos .. Right_Pos - 1 loop
            if (Left > Right) then
               Value_Moved := True;
               Swap(Left, Right);
            end if;
            Next(Left, Is_End);
            Next(Right, Is_End);
         end loop;
      end loop;
   end Bubble;

   procedure Recurse (O     : access Object'Class;
                      Left  : in out Iterator'Class;
                      Right : in out Iterator'Class) is
      Left_Idx  : Natural  := Get_Loc(Left);
      Right_Idx : Natural  := Get_Loc(Right);
      Left_Pos  : Positive := Get_Loc(Left);
      Right_Pos : Positive := Get_Loc(Right);
      Is_End    : End_Marker;
   begin
      if (Left_Idx < Right_Idx) then
         -- Get the key from the random position and swap it with the first
         -- value.
         Swap_At(O.all, Left_Idx, Random(Left_Pos, Right_Pos));

         -- From here on down, the key swaps between the left and right
         -- location, starting with the left location.
         Main_Loop: loop
            -- Move the right index to the left while the right value >
            -- key.
            while (Right >= Left) loop
               Right_Idx := Right_Idx - 1;
               Prev(Right, Is_End);
               exit Main_Loop when (Left_Idx = Right_Idx);
            end loop;

            -- Put the key into the right position and advance the left
            -- position (since it is < the key now).
            Swap(Left, Right);
            Next(Left, Is_End);
            Left_Idx := Left_Idx + 1;
            exit Main_Loop when (Left_Idx = Right_Idx);

            -- Move the left index to the right while the left value < key.
            while (Left <= Right) loop
               Left_Idx := Left_Idx + 1;
               Next(Left, Is_End);
               exit Main_Loop when (Left_Idx = Right_Idx);
            end loop;

            -- The right value has been pulled out, so replace it with the
            -- left value.
            Swap(Right, Left);
            Prev(Right, Is_End);
            Right_Idx := Right_Idx - 1;
            exit Main_Loop when (Left_Idx = Right_Idx);
         end loop Main_Loop;

         if ((Left_Pos + 11) < Left_Idx) then
            -- We have 10 or more values, do another quick sort.
            Set_Loc(Left, Left_Pos);
            Set_Loc(Right, Left_Idx - 1);
            Recurse(O, Left, Right);

         elsif ((Left_Pos + 1) < Left_Idx) then
            -- Less than 10 values to do, but at least two, do a bubble
            -- sort.
            Set_Loc(Left, Left_Pos);
            Set_Loc(Right, Left_Idx - 1);
            Bubble(Left, Right);
         end if;

         if (Right_Pos > (Right_Idx + 11)) then
            -- We have 10 or more values, do another quick sort.
            Set_Loc(Left, Right_Idx + 1);
            Set_Loc(Right, Right_Pos);
            Recurse(O, Left, Right);

         elsif (Right_Pos > (Right_Idx + 1)) then
            -- Less than 20 values to do, but at least two, do a bubble
            -- sort.
            Set_Loc(Left, Right_Idx + 1);
            Set_Loc(Right, Right_Pos);
            Bubble(Left, Right);
         end if;
      end if;
   end Recurse;

   procedure Sort (O    : access Object'Class;
                   Pos1 : in out Iterator'Class;
                   Pos2 : in out Iterator'Class) is
      Is_End : End_Marker;
      Done   : Boolean := False;
   begin
      First(Pos1, Is_End);
      if (Is_End = Not_Past_End) then
         Last(Pos2, Is_End);
         Recurse(O, Pos1, Pos2);
      end if;
   end Sort;

   procedure Sort (O : access Object'Class) is
      Pos1   : Iterator_Class
        := Iterator_Class(New_Iterator(Asgc.Object_Class(O)));
      Pos2   : Iterator_Class
        := Iterator_Class(New_Iterator(Asgc.Object_Class(O)));
   begin
      Sort(O, Pos1.all, Pos2.all);
      Free(Pos1);
      Free(Pos2);
   end Sort;

begin
   Random_Natural.Reset(Random_Generator);
end Asgc.Ordered.Sortable.Quicksort;
