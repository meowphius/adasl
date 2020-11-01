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

-- This package create a graph that holds all the states in the United
-- States.  States that are directly adjacent have connections between them

with Baseclass;
with Asgc.Graph.Fixed.Graph;
with Asgc.Graph.Links.Dynamic;

package State_Graph is

   -- How many states are there?
   Number_Of_States : constant := 50;

   -- These should be private, but I need them here for visibility of the
   -- End_Marker type.
   type State is new Baseclass.Object with private;
   type State_Class is access all State'Class;

   -- "=" must be defined here so that State_Base picks it up.
   function "=" (Val1, Val2 : in State_Class) return Boolean;

   package State_Base is new Asgc(Contained_Type => State_Class);
   subtype End_Marker is State_Base.End_Marker;
   Past_End     : constant End_Marker := State_Base.Past_End;
   Not_Past_End : constant End_Marker := State_Base.Not_Past_End;


   subtype Iterator_Parent is Baseclass.Object;
   type Iterator is new Iterator_Parent with private;
   type Iterator_Class is access all Iterator'Class;
   type Iterator_Ptr is access all Iterator;

   -- Return the name of the state the iterator references.
   function Get_Name (Iter : in Iterator) return String;

   -- Find a state in the graph.
   procedure Find (Iter  : in out Iterator;
                   Name  : in String;
                   Found : out Boolean);


   -- The first state in the graph.
   procedure First (Iter   : in out Iterator;
                    Is_End : out End_Marker);

   -- The next state in the graph.
   procedure Next (Iter   : in out Iterator;
                   Is_End : out End_Marker);

   ------------------------------------------------------------------------
   -- The graph of states is a list of the states that are physically
   -- connected to each other in the US.  The graph has a connection
   -- between states for every state that is adjacent.  The following
   -- methods traverse and test that map.  The state connections have no
   -- distinct order.

   -- Move the the first connection for the state.  If Is_End returns as
   -- Past_End, the state has no adjacent states.
   procedure First_Connection (Iter   : in out Iterator;
                               Is_End : out End_Marker);

   -- Move to the next connection for the state.  If Is_End returns as
   -- Past_End, the current connection is the last connection in the state.
   procedure Next_Connection (Iter   : in out Iterator;
                              Is_End : out End_Marker);

   -- Find if the states the two iterators reference are connected and move
   -- the first iterator (Iter) to reference that connection.  If the
   -- connection does not exist, Found is set to False and Iter is not
   -- modified.
   procedure Find_Connection (Iter  : in out Iterator;
                              Dest  : in Iterator;
                              Found : out Boolean);

   -- Find if the state the iterator references is connected and move the
   -- iterator (Iter) to reference that connection.  If the connection does
   -- not exist, Found is set to False and Iter is not modified.
   procedure Find_Connection (Iter  : in out Iterator;
                              Name  : in String;
                              Found : out Boolean);

   -- Return True if the states the two iterators reference are adjacent,
   -- False if not.
   function Is_Connected (Iter1, Iter2 : in Iterator) return Boolean;

   -- Return True if the two states are adjacent, False if not.
   function Is_Connected (Name1, Name2 : in String) return Boolean;

   -- Follow the current connection the iterator references and return an
   -- iterator that references that state.
   function Follow_Connection (Iter : in Iterator)
                               return Iterator;

   -- Follow the current connection the iterator references and sets the
   -- iterator that references that state.
   procedure Follow_Connection (Iter : in out Iterator);

private

   -- This is the state node data structure.
   type String_Ptr is access all String;
   type State is new Baseclass.Object with record
      Name : String_Ptr;
   end record;
   type State_Ptr is access all State;

   -- Hash function for the state.  This will just add up the positions of
   -- the characters for the hash value.
   function Hash_State (Val : in State_Class) return Natural;

   -- Instantiate a graph of states.  The link type is not used, we just
   -- set it to Boolean.  First the graph base.
   package State_Graph_Base is new State_Base.Graph
     (Do_Hash             => Hash_State,
      Link_Contained_Type => Boolean);

   -- The state graph is fixed (since there are a fixed number of states in
   -- the US).
   package State_Graph_F is new State_Graph_Base.Fixed;

   -- We use dynamic link entries for each node.
   package State_Graph_Links is new State_Graph_F.Links.Dynamic;

   -- We use dynamic links in the state graph because the number of
   -- adjacent states varies from state to state.
   package The_State_Graph is new State_Graph_F.Graph
     (State_Graph_Links.Graph_Link,
      State_Graph_Links.Graph_Link_It);

   -- Here is the state graph.  It is initialized in the packages
   -- initialization code.
   Graph : aliased The_State_Graph.Object(Hash_Size => 30,
                                          Size      => Number_Of_States);

   -- The iterator for the state graph, which just holds an iterator for
   -- the graph type we have just created.  These always reference the one
   -- graph we create, so just initialize it here.
   type Iterator is new Iterator_Parent with record
      Iter : The_State_Graph.Iterator
        := The_State_Graph.New_Iterator(Graph'Access);
   end record;

end State_Graph;
