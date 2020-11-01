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


with Asgc.Tree;
with Asgc.Tree.Dynamic_Managed;
with Asgc.Tree.Fixed_Managed;
with Asgc.Tree.Expandable_Managed;
with Ada.Unchecked_Deallocation;

package Test_P.Tree is

   package Test1_Tree is new Test1_Con.Tree;

   package Test1_Tree_Dyn is new Test1_Tree.Dynamic_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Tree_Fix is new Test1_Tree.Fixed_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Tree_Exp is new Test1_Tree.Expandable_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Tree_Dyn.Object, Test1_Tree_Dyn.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Tree_Fix.Object, Test1_Tree_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Tree_Exp.Object, Test1_Tree_Exp.Object_Ptr);

end Test_P.Tree;
