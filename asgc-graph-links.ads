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

-- This package contains the links (or edges, or arcs) between nodes in a
-- graph.  It implements it using two generic parameters:
--  Link_Type - the way to reference another node in a graph.
--  Link_Contained_Type - A user data value that is stored and return
--      but not operated on.
--
-- This package instantiates Graph_Link and Graph_Link iterators as
-- abstract types.

-- Note that the "Value" operated on in all these functions is a node
-- pointer or index (depending on Link_Type).
--
-- Users can create their own link types.  If they have lots of links on a
-- node, a hash table of links might be nice, for instance.  If they do
-- this they must instantiate this generic with the proper types and then
-- subclass Graph_Link and Graph_Link_It.  In addition, three different
-- type of link are already defined in subpackages:
--
-- Dynamic -  keeps a linked list of graph links, each allocated
-- with new.
--
-- Fixed - keeps a fixed-sized array of graph links.  Since the
-- size (number of array elements) can't be specified here, this type
-- itself is in its own generic that takes a size parameter.
--
-- Expandable - are like fixed links, but its array is
-- dynamically allocated and will grow as necessary (by reallocation).
-- Like fixed links, it is declared in its own generic but it takes two
-- parmeters: an initial size and an amount it increase the array on every
-- reallocation.
--

generic
   type Link_Type is private;
   type Link_Contained_Type is private;
package Asgc.Graph.Links is

   -- A node in a graph, which holds links.  Note that a node can hold more
   -- than one distinct link to the same other node.  Graph nodes have to
   -- have the property that if we add the same value to the same node more
   -- than one time, A(1), A(2), and A(3), and we add another value to
   -- another node the same number of times, B(1), B(2), B(3), the A and B
   -- values will occur in the same order (If A(3) is first in one node,
   -- then B(3) is first in the second node) through other deletions and
   -- additions.  Since we support multiple links between two nodes, we
   -- must be able to find the right link back for deletion, this property
   -- says that if we are the Nth instance in our node, then the link back
   -- to us will be the Nth instance of the link back in the remote node.

   -- The graph data structures will never copy a link, so there is never a
   -- call to Adjust or the likes.

   type Graph_Link is abstract tagged private;
   type Graph_Link_Class is access all Graph_Link'Class;

   -- Called when the user is done with the graph link and is about to
   -- destroy it.  This can be used to free data and clean up.
   procedure Free_Graph_Link (Node : in out Graph_Link)
      is abstract;

   -- Called when a link is copied.  This is so the graph links can track
   -- their data properly and generate new copies if necessary.
   procedure Copied_Graph_Link (Node : in out Graph_Link)
      is abstract;


   type Graph_Link_It is abstract tagged private;

   -- Set the node that the iterator references.
   procedure Set_Node (Iter : in out Graph_Link_It;
                       Node : in Graph_Link_Class)
      is abstract;

   -- Add a link to a node.  The Contents is associated data that can be
   -- held in the link.
   procedure Add (Node     : in out Graph_Link;
                  Val      : in Link_Type;
                  Contents : in Link_Contained_Type)
      is abstract;

   -- Delete the first link from the node.
   procedure Delete_First (Node : in out Graph_Link)
      is abstract;

   -- Find the first link holding "Val" and delete it.
   procedure Delete_Val (Node     : in out Graph_Link;
                         Val      : in Link_Type;
                         Instance : in Positive := 1)
      is abstract;

   -- Return the value at the given numeric position.
   function Get_Pos (Node : in Graph_Link;
                     Pos  : in Positive)
                     return Link_Type
      is abstract;

   -- Return True if the value is in one of the links in the node and False
   -- if it does not exist.
   function Val_Exists (Node     : in Graph_Link;
                        Val      : in Link_Type;
                        Instance : in Positive := 1)
                        return Boolean
      is abstract;

   -- Delete the link the iterator currently references.
   procedure Delete (Iter   : in out Graph_Link_It;
                     Is_End : out End_Marker)
      is abstract;

   -- Move the iterator to the first link in the node.
   procedure First (Iter   : in out Graph_Link_It;
                    Is_End : out End_Marker)
      is abstract;

   -- Move the iterator to the Next link in the node.
   procedure Next (Iter   : in out Graph_Link_It;
                   Is_End : out End_Marker)
      is abstract;

   -- Find "Val" in the current node and move the iterator to if.  If Val
   -- is not a link in the current node, Found will be False and the
   -- iterator will not be modified.
   procedure Find (Iter     : in out Graph_Link_It;
                   Val      : in Link_Type;
                   Found    : out Boolean;
                   Instance : in Positive := 1)
      is abstract;

   -- Search forward in the iterator for the same value the iterator
   -- currently references.
   procedure Find_Again (Iter  : in out Graph_Link_It;
                         Found : out Boolean)
      is abstract;

   -- Return the instance number of the value the iterator currently
   -- references.  A value may occur more than once in the node and they
   -- will have a specific order.  The first will return 1, the second 2,
   -- and so on.
   function Get_Instance_Number (Iter : in Graph_Link_It)
                                 return Positive
      is abstract;

   -- Same as above, but works on absolute position.
   function Get_Instance_Number (Node : in Graph_Link;
                                 Pos  : in Positive)
                                 return Positive
      is abstract;

   -- Get the value of the link the iterator references.
   function Get (Iter : in Graph_Link_It) return Link_Type
      is abstract;

   -- Get the contents of the link the iterator references.
   function Get_Contents (Iter : in Graph_Link_It)
                          return Link_Contained_Type
      is abstract;

   -- Get the contents of the first link holding that specified value.
   function Get_Contents (Node     : in Graph_Link;
                          Val      : in Link_Type;
                          Instance : in Positive := 1)
                          return Link_Contained_Type
      is abstract;

   -- Get the contents of the value at the beginning of the list of
   -- links.
   function Get_First_Contents (Node : in Graph_Link)
                                return Link_Contained_Type
      is abstract;

   -- Set the contents of the link the iterator references.
   procedure Set_Contents (Iter     : in out Graph_Link_It;
                           Contents : in Link_Contained_Type)
      is abstract;

   -- Return the number of links in the current node.
   function Link_Count (Node : in Graph_Link) return Natural
      is abstract;

private

   type Update_Count is mod 2 ** 32;

   Invalid_Update : constant Update_Count := 0 - 1;

   type Graph_Link is abstract tagged record
      Update     : Update_Count := 0;
   end record;

   type Graph_Link_It is abstract tagged record
      Update : Update_Count := Invalid_Update;
   end record;

end Asgc.Graph.Links;
