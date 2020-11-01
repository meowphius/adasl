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

with Ada.Finalization;

generic
   with function ">" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
   with function "<" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
   with function ">=" (V1 : in Contained_Type; V2 : in Contained_Type)
                       return Boolean is <>;
   with function "<=" (V1 : in Contained_Type; V2 : in Contained_Type)
                       return Boolean is <>;
   -- "=" is defined in Asgc
package Asgc.Tree is

   ------------------------------------------------------------------------
   subtype Parent_Object is Asgc.Object;
   type Object is abstract new Parent_Object with private;
   type Object_Class is access all Object'Class;
   type Object_Ptr is access all Object;

   function "=" (O1, O2 : in Object) return Boolean is abstract;

   ------------------------------------------------------------------------
   subtype Parent_Iterator is Asgc.Iterator;
   type Iterator is abstract new Parent_Iterator with private;
   type Iterator_Class is access all Iterator'Class;
   type Iterator_Ptr is access all Iterator;

   function "=" (Iter1, Iter2 : in Iterator) return Boolean is abstract;

   -- Move the iterator to the root of the tree.  If the tree is empty,
   -- Is_End will be set to Past_End and the iterator will be invalid.
   -- Otherwise, the
   procedure Root (Iter   : in out Iterator;
                   Is_End : out End_Marker)
      is abstract;

   -- Move the iterator down the left branch of the current place in the
   -- tree.  If it is at the end, Is_End will be set to Past_End and the
   -- iterator will not be changed.
   procedure Left (Iter   : in out Iterator;
                   Is_End : out End_Marker)
      is abstract;

   -- Move the iterator down the right branch of the current place in the
   -- tree.  If it is at the end, Is_End will be set to Past_End and the
   -- iterator will not be changed.
   procedure Right (Iter   : in out Iterator;
                    Is_End : out End_Marker)
      is abstract;

   -- Move up the tree one level.  If the iterator is at the root of the
   -- tree then Is_End will be Past_End.  Otherwise, it will be
   -- Not_Past_End.
   procedure Up (Iter   : in out Iterator;
                 Is_End : out End_Marker)
      is abstract;

   -- Move to the last value in the tree.
   procedure Last (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- Move to the previous value in the tree.  If the current position is
   -- the first value, Is_End will be set to Past_End and the iterator will
   -- not be moved.
   procedure Prev (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- Get an item and move to the previous value in the tree.
   procedure Get_Decr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker)
      is abstract;

   -- Various comparison operators that compare what the iterators point
   -- to.
   function ">" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   function ">" (Iter : in Iterator; Val : in Contained_Type) return Boolean
      is abstract;

   function ">" (Val : in Contained_Type; Iter : in Iterator) return Boolean
      is abstract;

   function "<" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   function "<" (Iter : in Iterator; Val : in Contained_Type) return Boolean
      is abstract;

   function "<" (Val : in Contained_Type; Iter : in Iterator) return Boolean
      is abstract;

   function ">=" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   function ">=" (Iter : in Iterator; Val : in Contained_Type) return Boolean
      is abstract;

   function ">=" (Val : in Contained_Type; Iter : in Iterator) return Boolean
      is abstract;

   function "<=" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   function "<=" (Iter : in Iterator; Val : in Contained_Type) return Boolean
      is abstract;

   function "<=" (Val : in Contained_Type; Iter : in Iterator) return Boolean
      is abstract;

   -- The code raises this exception if it detects some internal
   -- inconsistency.
   Internal_Tree_Error : exception;

private

   type Object is abstract new Parent_Object with null record;

   type Iterator is abstract new Parent_Iterator with null record;

end Asgc.Tree;
