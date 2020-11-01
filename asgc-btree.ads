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

-- The Btree base.  All Btrees implement the interface defined here.
-- Actually, there is not much because Btrees only implement the standard
-- functionality defined in Asgc.  The do need comparison functions just
-- like a tree.

generic
   with function ">" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
   with function "<" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
   with function ">=" (V1 : in Contained_Type; V2 : in Contained_Type)
                       return Boolean is <>;
   with function "<=" (V1 : in Contained_Type; V2 : in Contained_Type)
                       return Boolean is <>;
package Asgc.Btree is

   subtype Btree_Node_Size is Positive range 4 .. Positive'Last;

   ------------------------------------------------------------------------
   -- This is the btree container.
   type Object is abstract new Asgc.Object with private;
   type Object_Class is access all Object'Class;

   function "=" (O1, O2 : in Object) return Boolean is abstract;

   ------------------------------------------------------------------------
   -- An iterator for a btree container.
   type Iterator is abstract new Asgc.Iterator with private;
   type Iterator_Class is access all Iterator'Class;

   function "=" (Iter1, Iter2 : in Iterator) return Boolean is abstract;

   procedure Last (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean)
      is abstract;

   procedure Search_Again (Iter  : in out Iterator;
                           Found : out Boolean)
      is abstract;

   procedure Prev (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

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


   Internal_Btree_Error : exception;

private

   type Object is abstract new Asgc.Object with null record;

   type Iterator is abstract new Asgc.Iterator with null record;

end Asgc.Btree;
