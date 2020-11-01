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

-- This package create a vector where each element holds a State_Graph
-- iterator and a level number.  The vector is sortable and uses the name
-- of the state the iterator references for comparison.

with Asgc.Ordered.Sortable.Vector.Fixed;
with Asgc.Ordered.Sortable.Quicksort;
with State_Graph; use type State_Graph.Iterator;

package State_Iterator_Vector is

   type State_Vector_Info is record
      Iter  : State_Graph.Iterator;
      Level : Positive;
   end record;

   -- Do comparisons, but we use the name of the state the iterator
   -- references for the comparison.
   function "=" (Val1, Val2 : in State_Vector_Info) return Boolean;
   function ">" (Val1, Val2 : in State_Vector_Info) return Boolean;
   function "<" (Val1, Val2 : in State_Vector_Info) return Boolean;
   function ">=" (Val1, Val2 : in State_Vector_Info) return Boolean;
   function "<=" (Val1, Val2 : in State_Vector_Info) return Boolean;

   -- Instantiate the hierarchy of packages we need to get a sortable
   -- vector.
   package Base is new Asgc(Contained_Type => State_Vector_Info);
   package Ordered_Base is new Base.Ordered;
   package Sortable_Base is new Ordered_Base.Sortable;
   package Vector_Base is new Sortable_Base.Vector;
   package The_Vector is new Vector_Base.Fixed;
   package The_Sort is new Sortable_Base.Quicksort;

end State_Iterator_Vector;
