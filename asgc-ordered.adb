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

package body Asgc.Ordered is

   ------------------------------------------------------------------------
   function "-" (Iter1, Iter2 : in Iterator) return Integer is
   begin
      return (Integer(Get_Loc(Iterator'Class(Iter1)))
              - Integer(Get_Loc(Iterator'Class(Iter2))));
   end "-";

   ------------------------------------------------------------------------
   procedure Add (O : in out Object; Val : in Contained_Type) is
   begin
      Enqueue(Object'Class(O), Val);
   end Add;


   ------------------------------------------------------------------------
   procedure Add (Iter : in out Iterator;
                  Val  : in Contained_Type) is
      Is_End : End_Marker;
   begin
      Last(Iterator'Class(Iter), Is_End);
      Add_After(Iterator'Class(Iter), Val);
   end Add;

end Asgc.Ordered;
