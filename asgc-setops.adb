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

package body Asgc.Setops is

   function Entry_Count (O   : in Object_Class;
                         Val : in Contained_Type)
                         return Natural is
      Retval : Natural := 0;
      Iter   : Iterator_Class := New_Iterator(O);
      Found  : Boolean;
   begin
      Search(Iter.all, Val, Found);
      while (Found = True) loop
         Retval := Retval + 1;
         Search_Again(Iter.all, Found);
      end loop;
      Free(Iter);

      return Retval;
   end Entry_Count;

   -- Initialize the destination container.  If the destination and a
   -- source object are the same object, special work must be done.
   -- Otherwise, the destination container is emptied and the contents
   -- of the first object are put into it.
   procedure Init_Set_Dest (Dest   : in Object_Class;
                            O1, O2 : in Object_Class;
                            IDest  : in Iterator_Class;
                            I1, I2 : in out Iterator_Class) is
      I_Tmp  : Iterator_Class;
      Is_End : End_Marker;
   begin
      if (Dest = O1) then
         -- The destination an the first object are the same container, so
         -- there is nothing to do.
         null;
      elsif (Dest = O2) then
         -- The destination and the second object are the same container.
         -- Switch the first and second object so we will add the first
         -- object to the second.
         I_Tmp := I1;
         I1 := I2;
         I2 := I_Tmp;
      else
         -- Clear out the destination.
         First(IDest.all, Is_End);
         while (Is_End = Not_Past_End) loop
            Delete(IDest.all, Is_End);
         end loop;

         -- Copy all the things from the first container to the destination.
         First(I1.all, Is_End);
         while (Is_End = Not_Past_End) loop
            Add(Dest.all, Get(I1.all));
            Next(I1.all, Is_End);
         end loop;
      end if;
   end Init_Set_Dest;

   type Object_Ptr is access all Object;

   procedure Union (Dest   : in Object_Class;
                    O1, O2 : in Object_Class) is
      I1     : Iterator_Class := New_Iterator(O1);
      I2     : Iterator_Class := New_Iterator(O2);
      IDest  : Iterator_Class := New_Iterator(Dest);
      Is_End : End_Marker;
      Val    : Contained_Type;
      Found  : Boolean;
   begin
      Init_Set_Dest(Dest, O1, O2, IDest, I1, I2);

      -- Now move the second object into the first object.
      First(I2.all, Is_End);
      while (Is_End = Not_Past_End) loop
         Val := Get(I2.all);
         Search(Idest.all, Val, Found);
         if (not Found) then
            Add(Dest.all, Val);
         end if;
         Next(I2.all, Is_End);
      end loop;

      Free(I1);
      Free(I2);
      Free(IDest);
   end Union;

   procedure Intersection (Dest   : in Object_Class;
                           O1, O2 : in Object_Class) is
      I1     : Iterator_Class := New_Iterator(O1);
      I2     : Iterator_Class := New_Iterator(O2);
      IDest  : Iterator_Class := New_Iterator(Dest);
      Is_End : End_Marker;
      Val    : Contained_Type;
      Found  : Boolean;
   begin
      Init_Set_Dest(Dest, O1, O2, IDest, I1, I2);

      -- Now that we have everything in the first container into the
      -- destinaion, scan the destination container, deleting all items
      -- that are not in the second container.
      First(IDest.all, Is_End);
      while (Is_End = Not_Past_End) loop
         Val := Get(IDest.all);
         Search(I2.all, Val, Found);
         if (Found) then
            Next(IDest.all, Is_End);
         else
            Delete(IDest.all, Is_End);
         end if;
      end loop;

      Free(I1);
      Free(I2);
      Free(IDest);
   end Intersection;

   procedure Bag_Union (Dest   : in Object_Class;
                        O1, O2 : in Object_Class) is
      I1     : Iterator_Class := New_Iterator(O1);
      I2     : Iterator_Class := New_Iterator(O2);
      IDest  : Iterator_Class := New_Iterator(Dest);
      Is_End : End_Marker;
      Val    : Contained_Type;
   begin
      Init_Set_Dest(Dest, O1, O2, IDest, I1, I2);

      -- Now add everything in the second container into the destination.
      First(I2.all, Is_End);
      while (Is_End = Not_Past_End) loop
         Val := Get(I2.all);
         Add(Dest.all, Val);
         Next(I2.all, Is_End);
      end loop;

      Free(I1);
      Free(I2);
      Free(IDest);
   end Bag_Union;

   procedure Bag_Intersection (Dest   : in Object_Class;
                               O1, O2 : in Object_Class) is
      I1     : Iterator_Class := New_Iterator(O1);
      I2     : Iterator_Class := New_Iterator(O2);
      IDest  : Iterator_Class := New_Iterator(Dest);
      Is_End   : End_Marker;
      Val      : Contained_Type;
      Count1   : Natural;
      Count2   : Natural;
      Found    : Boolean;
      Next_Val : Contained_Type;
   begin
      Init_Set_Dest(Dest, O1, O2, IDest, I1, I2);

      -- If an entry is in both containers, add it to the destination.
      First(IDest.all, Is_End);
      while (Is_End = Not_Past_End) loop
         Val := Get(IDest.all);

         -- Get the next value, since we may lose this value from the
         -- container.
         Next(IDest.all, Is_End);
         while ((Is_End = Not_Past_End) and then (IDest.all = Val)) loop
            Next(IDest.all, Is_End);
         end loop;
         if (Is_End = Not_Past_End) then
            Next_Val := Get(IDest.all);
         end if;

         -- Find the number of entries in the destination and second source
         -- right now.
         Count1 := Entry_Count(Dest, Val);
         Count2 := Entry_Count(O2, Val);

         -- Now delete from the destination to make the count correct,
         -- if that is necessary.
         if (Count1 > Count2) then
            for I in Count2+1 .. Count1 loop
               Delete(Dest.all, Val);
            end loop;

            -- If we deleted values, the iterator will be invalid, so
            -- initialize it to reference the saved next value.
            if (Is_End = Not_Past_End) then
               Search(IDest.all, Next_Val, Found);
            end if;
         end if;
      end loop;

      Free(I1);
      Free(I2);
      Free(IDest);
   end Bag_Intersection;

   function Is_Subset (O1, O2 : in Object_Class) return Boolean is
      I1     : Iterator_Class := New_Iterator(O1);
      I2     : Iterator_Class := New_Iterator(O2);
      Is_End : End_Marker;
      Found  : Boolean;
   begin
      -- For all the members in O1, if any is not found in O2 then return
      -- False.  If we get through the loop, then O1 is a subset of O2.
      First (I1.all, Is_End);
      while (Is_End = Not_Past_End) loop
         Search(I2.all, Get(I1.all), Found);
         if (not Found) then
            return False;
         end if;
         Next(I1.all, Is_End);
      end loop;

      return True;
   end Is_Subset;

   function Is_Bag_Subset (O1, O2 : in Object_Class) return Boolean is
      I1       : Iterator_Class := New_Iterator(O1);
      I2       : Iterator_Class := New_Iterator(O2);
      Is_End   : End_Marker;
      Val      : Contained_Type;
      Last_Val : Contained_Type;
   begin
      -- For all the members in O1, if the count is O2 is less than the
      -- count in O1 return False.  If we get through the loop, then O1 is
      -- a bag subset of O2.  Since bag hash tables generally keep the same
      -- value in sequential locations, we do an optimization to skip a
      -- value if it is the same as the last value.
      First (I1.all, Is_End);
      if (Is_End = Not_Past_End) then
         Val := Get(I1.all);
         if (Entry_Count(O1, Val) > Entry_Count(O2, Val)) then
            return False;
         end if;
         Last_Val := Val;

         while (Is_End = Not_Past_End) loop
            Val := Get(I1.all);
            if ((Val /= Last_Val)
                and then (Entry_Count(O1, Val) > Entry_Count(O2, Val)))
            then
               return False;
            end if;
            Next(I1.all, Is_End);
            Last_Val := Val;
         end loop;
      end if;

      return True;
   end Is_Bag_Subset;

end Asgc.Setops;
