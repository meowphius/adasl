--     The Ada Structured Library - A set of container classes and general
--     tools for use with Ada95.
--     Copyright (C) 1998  Corey Minyard (minyard@acm.org)
--
--     This library is free software; you can redistribute it and/or
--     modify it under the terms of the GNU Library General Public
--     License as published by the Free Software Foundation; either
--     version 2 of the License, or (at your option) any later version.
--
--     This library is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--     Library General Public License for more details.
--
--     You should have received a copy of the GNU Library General Public
--     License along with this library; if not, write to the Free
--     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
--     MA 02111-1307, USA
--

with Asgc.Graph.Dynamic.DiGraph;
with Asgc.Graph.Links.Dynamic;

package Demo_Graph1_Package is

   --          EXAMPLE OF GRAPH USE
   --|Here are the NODES definitions which are (in this example)
   --|the Chemistry teacher's names in the school I am teaching
   --| and some chemicals used during the year
   type Graph_Node is (Heu, Melina, Vergnole,
                       Cuso4, Nicl2, Niso4,Hcl,H2so4, Hno3, Nacn, Cucn, Kcn);

   function Hash_Node (Node : in Graph_Node) return Natural;

   -- here, in this example,the teachers will consume these chemicals
   --|and the graph
   -- is going to keep track of the consumption of chemicals during the year
   -- the arc FROM THE TEACHER (NODE OF THE ARC)  -- T O --  THE CHEMICAL
   -- (ANOTHER NODE OF THE GRAPH)
   -- has an   A T T R I B U T E   which represents
   --a widhtdrawal of chemicals from the laboratory stockroom
   -- we will use the graph to keep track of the date and amount
   -- the graph representation is thus based on the following :
   -- any use will be represented by and ARC
   --whose ATTRIBUTE will hold the amount and date
   -- FROM a node whose vertex value will hold the teacher's name
   -- TO a node whose vertex value will hold the chemical's name
   type Attribute_Of_The_Arc is
      record
         Weight : Float   := 0.0;
         Date   : Integer := 0;
      end record;

   package Base is new Asgc(Contained_Type => Graph_Node);
   package Graph_Base is new Base.Graph
     (Do_Hash             => Hash_Node,
      Link_Contained_Type => Attribute_Of_The_Arc);

   package Graph_D is new Graph_Base.Dynamic;

   package Graph_Links is new Graph_D.Links.Dynamic;

   package Graph is new Graph_D.DiGraph(Graph_Links.Graph_Link,
                                        Graph_Links.Graph_Link_It,
                                        Allow_Duplicate_Links => True);

end Demo_Graph1_Package;
