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

-- This program has two functions:
--
--   Print all the states in the United States and what states are adjacent
--   to them.
--
--   Find the path between two states that goes through the least number
--   of other states.
--
-- See the usage statement in Print_Usage for how to use this program.

with State_Graph; use type State_Graph.End_Marker;
  use type State_Graph.Iterator;
with State_Iterator_Vector;
with State_Stack;
with Ada.Command_Line;
with Text_IO; use Text_IO;

procedure State_Path is

   package Vector renames State_Iterator_Vector.The_Vector;
   package Stack renames State_Stack.Stack;

   procedure Print_Usage is
   begin
      Put_Line("Usage:");
      Put_Line("  state_path --print-states");
      Put_Line(" or");
      Put_Line("  state_path state1 state2");
      New_Line;
      Put_Line(" Print a list of all the states and what other states they");
      Put_Line(" are connected to (-print_states) or print a path with the");
      Put_Line(" least number of states from state1 to state2.  State names");
      Put_Line(" must be capitalized and names with multiple words have to");
      Put_Line(" be delimited properly.  For example:");
      New_Line;
      Put_Line("  state_path Alabama 'New York'");
      New_Line;
   end Print_Usage;

   -- Print all the states in the United States.  For each state, print the
   -- states that are adjacent.
   procedure Print_States is
      Print_Vector : aliased Vector.Object
                               (Size => State_Graph.Number_Of_States);
      Curr         : State_Graph.Iterator;
      Is_End       : State_Graph.End_Marker;
      V_Info       : State_Iterator_Vector.State_Vector_Info;
   begin
      -- Add in iterator that references each state to the vector.
      State_Graph.First(Curr, Is_End);
      while (Is_End /= State_Graph.Past_End) loop
         V_Info.Iter := Curr;
         Vector.Add(Print_Vector, V_Info);
         State_Graph.Next(Curr, Is_End);
      end loop;

      -- Sort the vector.
      State_Iterator_Vector.The_Sort.Sort(Print_Vector'Unchecked_Access);

      -- Now go through the sorted vector in order.
      for I in 1 .. Vector.Member_Count(Print_Vector) loop

         -- Get the state and print it out.
         V_Info := Vector.Get_At(Print_Vector, I);
         Put(State_Graph.Get_Name(V_Info.Iter)  & ":");

         -- Now go through every connection in the state
         State_Graph.First_Connection(V_Info.Iter, Is_End);
         while (Is_End /= State_Graph.Past_End) loop

            -- Print the state's name.
            Put(" " & State_Graph.
                Get_Name(State_Graph.Follow_Connection(V_Info.Iter)));
            State_Graph.Next_Connection(V_Info.Iter, Is_End);

            -- If there is another connection, print a ","
            if (Is_End = State_Graph.Not_Past_End) then
               Put(",");
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_States;


   Search_Vector : aliased Vector.Object(Size => 50);
   State_Holder  : aliased Stack.Object(Initial_Size => 5, Increment => 5);
   Curr_Pos      : Positive;
   Source        : State_Graph.Iterator;
   Dest          : State_Graph.Iterator;
   Curr          : State_Graph.Iterator;
   Pos           : Positive := 1;
   Count         : Positive := 1;
   Is_End        : State_Graph.End_Marker;
   Found         : Boolean;
   Last          : Positive;
   First         : Positive;
   V_Info        : State_Iterator_Vector.State_Vector_Info;
   V_Info_Tmp    : State_Iterator_Vector.State_Vector_Info;
begin
   -- Go through the command line arguments.
   while (Count <= Ada.Command_Line.Argument_Count) loop

      if (Ada.Command_Line.Argument(Count)(1) = '-') then

         -- It is a flag.  Find the flag it references.  Remove the initial
         -- '-' from the comparisons.
         First := Ada.Command_Line.Argument(Count)'First + 1;
         Last := Ada.Command_Line.Argument(Count)'Last;

         if (Ada.Command_Line.Argument(Count)(First .. Last)
             = "-print-states")
         then
            -- Print all the states in the union.
            Print_States;
            return;
         else
            Put_Line("Invalid argument: '"
                     & Ada.Command_Line.Argument(Count)
                     & "'");
            Print_Usage;
            return;
         end if;

      elsif (Pos = 1) then
         -- The first state.  Get an iterator that references it.
         State_Graph.Find(Source, Ada.Command_Line.Argument(Count), Found);
         if (not Found) then
            Put_Line("State '"
                     & Ada.Command_Line.Argument(Count)
                     & "' not found");
            Print_Usage;
            return;
         end if;
         Pos := Pos + 1;

      elsif (Pos = 2) then
         -- The second state.  Get an iterator that references it.
         State_Graph.Find(Dest, Ada.Command_Line.Argument(Count), Found);
         if (not Found) then
            Put_Line("State '"
                     & Ada.Command_Line.Argument(Count)
                     & "' not found");
            Print_Usage;
            return;
         end if;
         Pos := Pos + 1;

      else
         Put_Line("Too many states");
         Print_Usage;
         return;
      end if;

      Count := Count + 1;
   end loop;

   if (Pos /= 3) then
      Put_Line("Not enough states specified");
      Print_Usage;
      return;
   end if;

   -- Add the source state as the first item in the search vector.
   V_Info.Iter := Source;
   V_Info.Level := 1;
   Vector.Add(Search_Vector, V_Info);

   -- Now go through the search vector.  For every connection that the
   -- current position has, add it to the search vector if it is not
   -- already there.  This occurs in "levels".  The source state is level
   -- one.  Every state that is directly adjacent to the source state is
   -- level two.  Every state that is two states away is level three.  And
   -- so on.
   Curr_Pos := 1;
   Search_Loop: loop
      V_Info := Vector.Get_At(Search_Vector, Curr_Pos);

      -- Iterate through all the connection in the current position.
      State_Graph.First_Connection(V_Info.Iter, Is_End);
      while (Is_End /= State_Graph.Past_End) loop
         -- Get an iterator to the it references.
         V_Info_Tmp.Iter := State_Graph.Follow_Connection(V_Info.Iter);

         if (V_Info_Tmp.Iter = Dest) then
            -- We've found the destination, push the two states we know
            -- about and leave to start searching backwards.
            Stack.Push(State_Holder, Dest);
            Stack.Push(State_Holder, V_Info.Iter);
            exit Search_Loop;
         end if;

         if (not Vector.Value_Exists(Search_Vector, V_Info_Tmp)) then
            -- If the state is not in the vector already, then add it.  The
            -- level is one greater than the current level because it is
            -- one state farther away.
            V_Info_Tmp.Level := V_Info.Level + 1;
            Vector.Add(Search_Vector, V_Info_Tmp);
         end if;

         State_Graph.Next_Connection(V_Info.Iter, Is_End);
      end loop;

      if (Curr_Pos = Vector.Member_Count(Search_Vector)) then
         -- This is the last position in the vector, which means we haven't
         -- found a path between the two states.
         Put_Line("No path from "
                  & State_Graph.Get_Name(Source)
                  & " to "
                  & State_Graph.Get_Name(Dest));
         return;
      else
         -- We've finished with the current state, move to the next one.
         Curr_Pos := Curr_Pos + 1;
      end if;
   end loop Search_Loop;

   -- Ok, we have found that there is a patch and we can get it from the
   -- search vector, but we don't have it yet.  Search backwards from the
   -- current position.  We need a connection at each level from the state
   -- we currently have to one at the previous level.
   while (V_Info.Level /= 1) loop

      -- Search backwards until we get to the previous level.
      Curr_Pos := Curr_Pos - 1;
      V_Info_Tmp := Vector.Get_At(Search_Vector, Curr_Pos);
      while (V_Info.Level = V_Info_Tmp.Level) loop
         Curr_Pos := Curr_Pos - 1;
         V_Info_Tmp := Vector.Get_At(Search_Vector, Curr_Pos);
      end loop;

      -- Now we are at the previous level, search for a connection.  One
      -- has to be there for the current iterator to be there!
      while (not State_Graph.Is_Connected(V_Info.Iter, V_Info_Tmp.Iter)) loop
         Curr_Pos := Curr_Pos - 1;
         V_Info_Tmp := Vector.Get_At(Search_Vector, Curr_Pos);
      end loop;

      -- We have the next connection back, so put it onto the stack.
      V_Info := V_Info_Tmp;
      Stack.Push(State_Holder, V_Info.Iter);
   end loop;

   -- Everything should be in the stack, so just print it.
   while (Stack.Member_Count(State_Holder) /= 0) loop
      Stack.Pop(State_Holder, Curr);
      Put_Line(State_Graph.Get_Name(Curr));
   end loop;

end State_Path;
