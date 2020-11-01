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

with Asgc.Graph.Dynamic.DiGraph;
with Asgc.Graph.Fixed.DiGraph;
with Asgc.Graph.Expandable.DiGraph;
with Asgc.Graph.Links.Dynamic;
with Asgc.Graph.Links.Fixed;
with Asgc.Graph.Links.Expandable;

package Test_P.DiGraph is

   function Hash_Integer (Val : Integer) return Natural;

   package Test1_Graph is new Test1_Con.Graph
     (Do_Hash             => Hash_Integer,
      Link_Contained_Type => Integer);

   package Test1_Graph_Dyn is new Test1_Graph.Dynamic;

   package Test1_Dyn_Graph_Dyn is new Test1_Graph_Dyn.Links.Dynamic;
   package Test1_DiGraph_Dyn_Dyn is new Test1_Graph_Dyn.DiGraph
     (Test1_Dyn_Graph_Dyn.Graph_Link,
      Test1_Dyn_Graph_Dyn.Graph_Link_It);

   package Test1_Dyn_Graph_Fix is
     new Test1_Graph_Dyn.Links.Fixed(Size => 100);
   package Test1_DiGraph_Dyn_Fix is new Test1_Graph_Dyn.DiGraph
     (Test1_Dyn_Graph_Fix.Graph_Link,
      Test1_Dyn_Graph_Fix.Graph_Link_It);

   package Test1_Dyn_Graph_Exp is
     new Test1_Graph_Dyn.Links.Expandable(Initial_Size => 10,
                                          Increment    => 10);
   package Test1_DiGraph_Dyn_Exp is new Test1_Graph_Dyn.DiGraph
     (Test1_Dyn_Graph_Exp.Graph_Link,
      Test1_Dyn_Graph_Exp.Graph_Link_It);


   package Test1_Graph_Fix is new Test1_Graph.Fixed;

   package Test1_Fix_Graph_Dyn is new Test1_Graph_Fix.Links.Dynamic;
   package Test1_DiGraph_Fix_Dyn is new Test1_Graph_Fix.DiGraph
     (Test1_Fix_Graph_Dyn.Graph_Link,
      Test1_Fix_Graph_Dyn.Graph_Link_It);

   package Test1_Fix_Graph_Fix is
     new Test1_Graph_Fix.Links.Fixed(Size => 100);
   package Test1_DiGraph_Fix_Fix is new Test1_Graph_Fix.DiGraph
     (Test1_Fix_Graph_Fix.Graph_Link,
      Test1_Fix_Graph_Fix.Graph_Link_It);

   package Test1_Fix_Graph_Exp is
     new Test1_Graph_Fix.Links.Expandable(Initial_Size => 10,
                                          Increment    => 10);
   package Test1_DiGraph_Fix_Exp is new Test1_Graph_Fix.DiGraph
     (Test1_Fix_Graph_Exp.Graph_Link,
      Test1_Fix_Graph_Exp.Graph_Link_It);


   package Test1_Graph_Exp is new Test1_Graph.Expandable;

   package Test1_Exp_Graph_Dyn is new Test1_Graph_Exp.Links.Dynamic;
   package Test1_DiGraph_Exp_Dyn is new Test1_Graph_Exp.DiGraph
     (Test1_Exp_Graph_Dyn.Graph_Link,
      Test1_Exp_Graph_Dyn.Graph_Link_It);

   package Test1_Exp_Graph_Fix is
     new Test1_Graph_Exp.Links.Fixed(Size => 100);
   package Test1_DiGraph_Exp_Fix is new Test1_Graph_Exp.DiGraph
     (Test1_Exp_Graph_Fix.Graph_Link,
      Test1_Exp_Graph_Fix.Graph_Link_It);

   package Test1_Exp_Graph_Exp is
     new Test1_Graph_Exp.Links.Expandable(Initial_Size => 10,
                                          Increment    => 10);
   package Test1_DiGraph_Exp_Exp is new Test1_Graph_Exp.DiGraph
     (Test1_Exp_Graph_Exp.Graph_Link,
      Test1_Exp_Graph_Exp.Graph_Link_It);

end Test_P.DiGraph;
