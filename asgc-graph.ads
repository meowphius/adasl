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

-- This package implements a graph container.  A graph is a set of nodes
-- that are connected with links.  The nodes contain a user value and the
-- links may also contain a (different) user value.  The value for a graph
-- node is the base value for the container classes (defined in the Asgc
-- package), the value for a graph link is specified in Link_Contained_Type
-- below.  Note that internally, a graph uses a hash table to keep track of
-- values.  Unfortunately, this must be exposed because a hash function is
-- needed to keep track of the values and a hash table size must also be
-- specified.
--

generic
   type Link_Contained_Type is private;
   with function Do_Hash (Val : in Contained_Type) return Natural;
package Asgc.Graph is

   ------------------------------------------------------------------------
   -- This is the graph container.
   type Object is abstract new Asgc.Object with private;
   type Object_Class is access all Object'Class;

   function "=" (O1, O2 : in Object) return Boolean is abstract;

   -- Add a link between the given values.
   procedure Add_Link (O          : in out Object;
                       From       : in Contained_Type;
                       To         : in Contained_Type;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True)
      is abstract;

   function Link_Exists (O     : in Object;
                         From  : in Contained_Type;
                         To    : in Contained_Type)
                         return Boolean
      is abstract;

   -- Callbacks for adding and removing links.  If Set_Link_Callbacks is
   -- called with a non-null value, the callbacks for that object will be
   -- called accordingly.
   -- IMPORTANT - With regular graphs, these functions will be called
   --             TWICE for every link added, deleted, or copied, since
   --             the link goes both directions.  The methods must account
   --             for this properly, especially if they generate a new
   --             copy, since each link direction will have a different
   --             copy in that case.  The methods should not assume any
   --             calling order, so doing something like "Odd adds are
   --             real and even one just use the last value" is dangerous.
   --             This does NOT apply to directed graphs.
   type Link_Callbacks is abstract new Baseclass.Object with private;
   type Link_Callbacks_Class is access all Link_Callbacks'Class;

   -- A link was added to the container.
   procedure Added (Cb  : access Link_Callbacks;
                    O   : in Object'Class;
                    Val : in out Link_Contained_Type)
      is abstract;

   -- A link was copied to a new container.
   procedure Copied (Cb  : access Link_Callbacks;
                     O   : in Object'Class;
                     Val : in out Link_Contained_Type)
      is abstract;

   -- A link was deleted from a container.
   procedure Deleted (Cb  : access Link_Callbacks;
                      O   : in Object'Class;
                      Val : in out Link_Contained_Type)
      is abstract;


   -- Set the link callbacks for an object.  Setting it to null will turn
   -- the callbacks off.
   procedure Set_Link_Callbacks (O  : in out Object;
                                 Cb : in Link_Callbacks_Class);


   ------------------------------------------------------------------------
   -- An iterator for a graph container.
   type Iterator is abstract new Asgc.Iterator with private;
   type Iterator_Class is access all Iterator'Class;

   function "=" (Iter1, Iter2 : in Iterator) return Boolean is abstract;

   ------------------------------------------------------------------------
   -- A single element in a graph has "links" to other graph nodes, thus it
   -- needs a way to move through these.  Each element in a graph contains
   -- a set of links to other elements in the graph.  The iterator, in
   -- addition to being positioned on a graph element, has a subposition
   -- that iterates through the links for a graph element.

   -- Add a link from the "From" iterator's element to the "To" iterators
   -- element.  If Ignore_Dup is True, the call will not perform an action
   -- if the link already exists.  If Ignore_Dup is False, the call will
   -- raise Link_Already_Exists if the link already exists.
   procedure Add_Link (From       : in out Iterator;
                       To         : in out Iterator;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True)
      is abstract;

   -- Two more add_link routines that reference a contained type and one
   -- iterator.
   procedure Add_Link (From       : in out Iterator;
                       To         : in Contained_Type;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True)
      is abstract;

   procedure Add_Link (From       : in Contained_Type;
                       To         : in out Iterator;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True)
      is abstract;

   -- Delete the link the the current iterator references.  Raises
   -- Invalid_Link if the iterator has no link position.
   procedure Delete_Link (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- Find a link from the "From" iterator's current position to the "To"
   -- iterator.  Raises Item_Not_Found if the link does not exist.
   function Find_Link (From : in Iterator;
                       To   : in Iterator)
                       return Iterator
      is abstract;

   function Find_Link (From : in Iterator;
                       To   : in Contained_Type)
                       return Iterator
      is abstract;

   function Find_Link (From : in Contained_Type;
                       To   : in Iterator)
                       return Iterator
      is abstract;

   -- Search the "From" iterator's current position for a link to the "To"
   -- iterator.  If Found returns True, the link was found and "From" will
   -- be positioned on that link.  If Found returns False, the link was not
   -- found and the "From" iterator is not modified.
   procedure Find_Link (From  : in out Iterator;
                        To    : in Iterator;
                        Found : out Boolean)
      is abstract;

   procedure Find_Link (From  : in out Iterator;
                        To    : in Contained_Type;
                        Found : out Boolean)
      is abstract;


   -- Search from the "From" iterator's next position for another link with
   -- the same value as the current value of "From".  If Found returns
   -- True, the link was found and "From" will be positioned on that link.
   -- If Found returns False, the link was not found and the "From"
   -- iterator is not modified.
   procedure Find_Link_Again (From  : in out Iterator;
                              Found : out Boolean)
      is abstract;

   -- Returns True if there is a link from From to To, False if not.
   function Link_Exists (From  : in Iterator;
                         To    : in Iterator)
                         return Boolean
      is abstract;

   function Link_Exists (From  : in Iterator;
                         To    : in Contained_Type)
                         return Boolean
      is abstract;

   function Link_Exists (From  : in Contained_Type;
                         To    : in Iterator)
                         return Boolean
      is abstract;


   -- Move the iterator to the first link in the iterator's current
   -- location.  Is_End will be set to Past_End if the member has no links.
   procedure First_Link (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- Move the iterator to the next link in the iterator's current
   -- location.  Raises Invalid_Iterator if the iterator has no link
   -- position.  Is_End will be set to Past_End and the iterator will not
   -- be modified if the iterator is at the last link in the member.
   procedure Next_Link (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- Follow the current link in the iterators current location, returning
   -- an iterator that is positioned on the element the current link
   -- references.  Raises Invalid_Iterator if the iterator has no link
   -- position.
   function Follow_Link (Iter : in Iterator) return Iterator
      is abstract;

   -- Follow a link, but move the iterator itself to the position where it
   -- points.  Raises Invalid_Iterator if the iterator has no link
   -- position.
   procedure Follow_Link (Iter : in out Iterator)
      is abstract;

   -- Return the number of links in the current element of the iterator.
   function Link_Count (Iter : in Iterator) return Natural
      is abstract;

   -- Return the user value contained in the link
   function Get_Link (Iter : in Iterator) return Link_Contained_Type
      is abstract;

   -- Set the user value contained in the link.
   procedure Set_Link (Iter : in out Iterator;
                       Val  : in Link_Contained_Type)
      is abstract;


   ------------------------------------------------------------------------
   -- Nodes in the graph will descend from this type.  Nodes must be based
   -- upon a tagged type for polymorphism with links, but don't use
   -- Baseclass.Object because of the overhead of it being controlled.  The
   -- graph container handles calling the proper routines for adjusting and
   -- finalizing graph links.
   type Node_Base is abstract tagged private;
   type Node_Base_Class is access all Node_Base'Class;

   ------------------------------------------------------------------------
   -- Graphs can be dynamic, fixed, or expandable.  As well, the individual
   -- elements may also be in those categories, and it is independent of
   -- the graph type.  A dynamic element uses dynamic allocation for each
   -- link.  A fixed element has a fixed maximum number of links in an
   -- array.  An expandable uses an array like a fixed element, but the
   -- array will resize to grow as it fills up.  These work much like
   -- iterators.


   Internal_Graph_Error : exception;

private

   type Object is abstract new Asgc.Object with record
      Link_Cb : Link_Callbacks_Class := null;
   end record;

   type Link_Callbacks is abstract new Baseclass.Object with null record;

   type Iterator is abstract new Asgc.Iterator with null record;

   type Node_Base is abstract tagged null record;

end Asgc.Graph;
