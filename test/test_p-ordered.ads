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

with Asgc.Ordered.Dlist.Dynamic_Managed;
with Asgc.Ordered.Dlist.Fixed_Managed;
with Asgc.Ordered.Dlist.Expandable_Managed;
with Asgc.Ordered.Alist.Fixed_Managed;
with Asgc.Ordered.Alist.Expandable_Managed;
with Asgc.Ordered.Vector.Expandable_Managed;
with Asgc.Ordered.Vector.Fixed_Managed;
with Ada.Unchecked_Deallocation;

package Test_P.Ordered is

   package Test1_Ord is new Test1_Con.Ordered;

   package Test1_Vec is new Test1_Ord.Vector;

   package Test1_Vec_Exp is new Test1_Vec.Expandable_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Vec_Fix is new Test1_Vec.Fixed_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Dlist is new Test1_Ord.Dlist;

   package Test1_Dlist_Dyn is new Test1_Dlist.Dynamic_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Dlist_Fix is new Test1_Dlist.Fixed_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Dlist_Exp is new Test1_Dlist.Expandable_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Alist is new Test1_Ord.Alist;

   package Test1_Alist_Fix is new Test1_Alist.Fixed_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Alist_Exp is new Test1_Alist.Expandable_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Dlist_Dyn.Object, Test1_Dlist_Dyn.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Dlist_Fix.Object, Test1_Dlist_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Dlist_Exp.Object, Test1_Dlist_Exp.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Alist_Fix.Object, Test1_Alist_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Alist_Exp.Object, Test1_Alist_Exp.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Vec_Fix.Object, Test1_Vec_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Vec_Exp.Object, Test1_Vec_Exp.Object_Ptr);

end Test_P.Ordered;
