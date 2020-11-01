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

package body Asgc.Ordered.Sortable.Bubble_Sort is

   procedure Sort (O    : access Object'Class;
                   Pos1 : in out Iterator'Class;
                   Pos2 : in out Iterator'Class) is
      Is_End : End_Marker;
      Done   : Boolean := False;
   begin
      -- If the object is empty or has one member, we are sorted.
      First(Pos1, Is_End);
      if (Is_End = Not_Past_End) then

         -- This is a bubble sort, the simplest of all sorts.  We'll
         -- replace it later.
         while (not Done) loop
            First(Pos1, Is_End);
            Pos2 := Pos1;
            Done := True;
            Next(Pos2, Is_End);
            while (Is_End = Not_Past_End) loop
               if (Pos1 > Pos2) then
                  Swap(Pos1, Pos2);
                  Done := False;
               end if;
               Next(Pos1, Is_End);
               Next(Pos2, Is_End);
            end loop;
         end loop;
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

end Asgc.Ordered.Sortable.Bubble_Sort;
