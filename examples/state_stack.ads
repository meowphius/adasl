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

-- Instantiate a stack of iterators for the state graph.

with Asgc.Ordered.Alist.Expandable;
with State_Graph; use type State_Graph.Iterator;

package State_Stack is

   package Base is new Asgc(Contained_Type => State_Graph.Iterator);
   package Ordered_Base is new Base.Ordered;
   package AList_Base is new Ordered_Base.Alist;
   package Stack is new Alist_Base.Expandable;

end State_Stack;
