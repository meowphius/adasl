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

-- All list containers implement the following interface.  Lists are
-- defined to have a tail and a head, so we can add to those positions.

generic
package Asgc.List is

   type Object is abstract new Asgc.Object with private;
   type Object_Class is access all Object'Class;

   function "=" (O1, O2 : in Object) return Boolean is abstract;

   procedure Add (O : in out Object; Val : in Contained_Type);

   -- Add an item to the first position in the list.
   procedure Add_Head (O : in out Object; Val : in Contained_Type)
     is abstract;

   -- Add an item to the last position in the list.
   procedure Add_Tail (O : in out Object; Val : in Contained_Type)
     is abstract;

   type Iterator is abstract new Asgc.Iterator with private;
   type Iterator_Class is access all Iterator'Class;

   function "=" (Iter1, Iter2 : in Iterator) return Boolean is abstract;

   -- Add an item before the position the iterator references.
   procedure Add_After (Iter : in out Iterator; Val : in Contained_Type)
      is abstract;

   -- Add an item after the position the iterator references.
   procedure Add_Before (Iter : in out Iterator; Val : in Contained_Type)
      is abstract;

   -- Set the value of the item the iterator references.
   procedure Set (Iter : in Iterator; Val : in Contained_Type)
      is abstract;

private

   Internal_List_Error : exception;

   type Object is abstract new Asgc.Object with null record;

   type Iterator is abstract new Asgc.Iterator with null record;

end Asgc.List;
