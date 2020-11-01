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

package body Asgc is

   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean) is
      Is_End : End_Marker;
   begin
      First(Iterator'Class(Iter), Is_End);
      while (Is_End = Not_Past_End) loop
         if (Iterator'Class(Iter) = Val) then
            Found := True;
            return;
         end if;
         Next(Iterator'Class(Iter), Is_End);
      end loop;
      Found := False;
   end Search;

   procedure Search_Again (Iter  : in out Iterator;
                           Found : out Boolean) is
      Is_End : End_Marker;
      Val    : Contained_Type;
   begin
      Val := Get(Iterator'Class(Iter));
      Next(Iterator'Class(Iter), Is_End);
      while (Is_End = Not_Past_End) loop
         if (Iterator'Class(Iter) = Val) then
            Found := True;
            return;
         end if;
         Next(Iterator'Class(Iter), Is_End);
      end loop;

      Iter.Update := Invalid_Update;
      Found := False;
   end Search_Again;

   procedure Set_Callbacks (O  : in out Object;
                            Cb : in Callbacks_Class) is
   begin
      O.Cb := Cb;
   end Set_Callbacks;

   procedure Generic_For_All (O : in Object_Class) is
      Iter   : Iterator_Class := New_Iterator(O);
      Is_End : End_Marker;
      Val    : Contained_Type;
   begin
      First(Iter.all, Is_End);
      while (Is_End = Not_Past_End) loop
         Get_Incr(Iter.all, Val, Is_End);
         Operate(Val);
      end loop;
      Free(Iter);
   end Generic_For_All;

end Asgc;
