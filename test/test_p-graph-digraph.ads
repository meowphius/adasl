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

with Asgc.Graph.Dynamic.DiGraph_Managed;
with Asgc.Graph.Fixed.DiGraph_Managed;
with Asgc.Graph.Expandable.DiGraph_Managed;
with Ada.Unchecked_Deallocation;

package Test_P.Graph.DiGraph is

   package Test1_Graph_Dyn_Dyn is new Test1_Graph_Dyn.DiGraph_Managed
     (Test1_Dyn_Graph_Dyn.Graph_Link,
      Test1_Dyn_Graph_Dyn.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   package Test1_Graph_Dyn_Fix is new Test1_Graph_Dyn.DiGraph_Managed
     (Test1_Dyn_Graph_Fix.Graph_Link,
      Test1_Dyn_Graph_Fix.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   package Test1_Graph_Dyn_Exp is new Test1_Graph_Dyn.DiGraph_Managed
     (Test1_Dyn_Graph_Exp.Graph_Link,
      Test1_Dyn_Graph_Exp.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);


   package Test1_Graph_Fix_Dyn is new Test1_Graph_Fix.DiGraph_Managed
     (Test1_Fix_Graph_Dyn.Graph_Link,
      Test1_Fix_Graph_Dyn.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   package Test1_Graph_Fix_Fix is new Test1_Graph_Fix.DiGraph_Managed
     (Test1_Fix_Graph_Fix.Graph_Link,
      Test1_Fix_Graph_Fix.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   package Test1_Graph_Fix_Exp is new Test1_Graph_Fix.DiGraph_Managed
     (Test1_Fix_Graph_Exp.Graph_Link,
      Test1_Fix_Graph_Exp.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);


   package Test1_Graph_Exp_Dyn is new Test1_Graph_Exp.DiGraph_Managed
     (Test1_Exp_Graph_Dyn.Graph_Link,
      Test1_Exp_Graph_Dyn.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   package Test1_Graph_Exp_Fix is new Test1_Graph_Exp.DiGraph_Managed
     (Test1_Exp_Graph_Fix.Graph_Link,
      Test1_Exp_Graph_Fix.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   package Test1_Graph_Exp_Exp is new Test1_Graph_Exp.DiGraph_Managed
     (Test1_Exp_Graph_Exp.Graph_Link,
      Test1_Exp_Graph_Exp.Graph_Link_It,
      Allow_Duplicate_Links => False,
      Pool_Type             => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool               => My_Pool);

   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Dyn_Dyn.Object, Test1_Graph_Dyn_Dyn.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Dyn_Fix.Object, Test1_Graph_Dyn_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Dyn_Exp.Object, Test1_Graph_Dyn_Exp.Object_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Fix_Dyn.Object, Test1_Graph_Fix_Dyn.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Fix_Fix.Object, Test1_Graph_Fix_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Fix_Exp.Object, Test1_Graph_Fix_Exp.Object_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Exp_Dyn.Object, Test1_Graph_Exp_Dyn.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Exp_Fix.Object, Test1_Graph_Exp_Fix.Object_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Test1_Graph_Exp_Exp.Object, Test1_Graph_Exp_Exp.Object_Ptr);

end Test_P.Graph.DiGraph;
