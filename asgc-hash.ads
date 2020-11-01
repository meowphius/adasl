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

-- The hash table base.  All hash tables implement the interface defined
-- here.  Actually, there is not much because hash tables only implement
-- the standard functionality defined in Asgc.  The do need a hash
-- function, and the quality of the hash table has a direct correlation
-- with the hash function chosen and the size of the hash table.  Read
-- your data structures book for more info.

generic
   with function Do_Hash (Val : in Contained_Type) return Natural;
package Asgc.Hash is

   ------------------------------------------------------------------------
   -- This is the hash container.
   type Object is abstract new Asgc.Object with private;
   type Object_Class is access all Object'Class;

   function "=" (O1, O2 : in Object) return Boolean is abstract;

   ------------------------------------------------------------------------
   -- An iterator for a hash container.
   type Iterator is abstract new Asgc.Iterator with private;
   type Iterator_Class is access all Iterator'Class;

   function "=" (Iter1, Iter2 : in Iterator) return Boolean is abstract;

   Internal_Hash_Error : exception;

private

   type Object is abstract new Asgc.Object with null record;

   type Iterator is abstract new Asgc.Iterator with null record;

end Asgc.Hash;
