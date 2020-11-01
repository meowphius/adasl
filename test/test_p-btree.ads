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

with Asgc.Btree.Dynamic_Managed;
with Ada.Unchecked_Deallocation;

package Test_P.Btree is

   package Test1_Btree is new Test1_Con.Btree;

   package Test1_Btree_Dyn is new Test1_Btree.Dynamic_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Btree_Dyn.Object, Test1_Btree_Dyn.Object_Ptr);

end Test_P.Btree;
