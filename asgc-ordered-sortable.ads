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

generic
   with function ">" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
   with function "<" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
   with function ">=" (V1 : in Contained_Type; V2 : in Contained_Type)
                       return Boolean is <>;
   with function "<=" (V1 : in Contained_Type; V2 : in Contained_Type)
                       return Boolean is <>;
package Asgc.Ordered.Sortable is

   ------------------------------------------------------------------------
   -- An ordered sortable container objects.  These objects have members
   -- that have a specific numeric position and their values can be
   -- ordinally compared.
   subtype Parent_Object is Asgc.Ordered.Object;
   type Object is abstract new Parent_Object with private;
   type Object_Class is access all Object'Class;

   -- GNAT makes us add this, but I don't think it is required.
   function "=" (O1, O2 : in Object) return Boolean is abstract;

   ------------------------------------------------------------------------
   -- An iterator for an ordered container.

   subtype Parent_Iterator is Asgc.Ordered.Iterator;
   type Iterator is abstract new Parent_Iterator with private;
   type Iterator_Class is access all Iterator'Class;

   -- Search a sorted container for the value.
   procedure Binary_Search (O     : access Object;
                            Val   : in Contained_Type;
                            Found : out Boolean;
                            Iter  : in out Iterator'Class);

   -- For some reason, GNAT makes me add this to get it to compile.  I'm
   -- not sure if Ada requires it specifically.  We shall see.
   function "=" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   ------------------------------------------------------------------------
   -- Comparison operators for iterators.
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

private

   type Object is abstract new Parent_Object with null record;

   type Iterator is abstract new Parent_Iterator with null record;

end Asgc.Ordered.Sortable;
