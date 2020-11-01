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

--$START STORAGE_POOL
with System.Storage_Pools;
--$END STORAGE_POOL

generic
   type Link_List is new Graph_Link with private;
   type Link_List_It is new Graph_Link_It with private;
   Allow_Duplicate_Links : Boolean := False;
--$START STORAGE_POOL
   type Pool_Type is new System.Storage_Pools.Root_Storage_Pool with private;
   My_Pool : in out Pool_Type;
--$END STORAGE_POOL
package Asgc.Graph.CTYPE.GRAPHTYPEMANAGED is
   ------------------------------------------------------------------------
   -- This is the graph container.
   subtype Object_Parent is Asgc.Graph.Object;
--$START DYNAMIC
   type Object (Hash_Size : Positive) is new Object_Parent with private;
--$END DYNAMIC
--$START FIXED
   type Object (Hash_Size : Positive;
                Size      : Positive)
     is new Object_Parent with private;
--$END FIXED
--$START EXPANDABLE
   type Object (Hash_Size    : Positive;
                Initial_Size : Positive;
                Increment    : Natural)
     is new Object_Parent with private;
--$END EXPANDABLE
   type Object_Class is access all Object'Class;
   type Object_Ptr is access all Object;
--$START STORAGE_POOL
   for Object_Ptr'Storage_Pool use My_Pool;
   for Object_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL

   procedure Add (O   : in out Object;
                  Val : in Contained_Type);

   procedure Delete (O   : in out Object;
                     Val : in Contained_Type);

   function Value_Exists (O   : in Object;
                          Val : in Contained_Type)
                          return Boolean;

   function Member_Count (O : in Object)
                          return Natural;

   procedure Add_Link (O          : in out object;
                       From       : in Contained_Type;
                       To         : in Contained_Type;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True);

   function Link_Exists (O     : in Object;
                         From  : in Contained_Type;
                         To    : in Contained_Type)
                         return Boolean;

   function "=" (O1, O2 : in Object) return Boolean;

   procedure Verify_Integrity (O : in Object);

   function Copy (O : in Object) return Asgc.Object_Class;

   procedure Initialize (O : in out Object);
   procedure Adjust (O : in out Object);
   procedure Finalize (O : in out Object);

   ------------------------------------------------------------------------
   -- An iterator for a graph container.
   subtype Iterator_Parent is Asgc.Graph.Iterator;
   type Iterator is new Iterator_Parent with private;
   type Iterator_Class is access all Iterator'Class;
   type Iterator_Ptr is access all Iterator;
--$START STORAGE_POOL
   for Iterator_Ptr'Storage_Pool use My_Pool;
   for Iterator_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL

   function New_Iterator (O : access Object) return Asgc.Iterator_Class;

   function New_Iterator (O : in Object_Class) return Iterator;

   procedure Free (Iter : access Iterator);

   procedure Set_Container(Iter : in out Iterator;
                           O    : in Asgc.Object_Class);

   procedure Add (Iter : in out Iterator;
                  Val  : in Contained_Type);

   procedure First (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Next (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Delete (Iter : in out Iterator; Is_End : out End_Marker);

   function Is_Same (Iter1, Iter2 : in Iterator) return Boolean;

   function Get (Iter : in Iterator) return Contained_Type;

   procedure Get_Incr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker);

   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean);

   procedure Search_Again (Iter  : in out Iterator;
                           Found : out Boolean);

   function "=" (Iter1, Iter2 : in Iterator) return Boolean;

   function "=" (Iter : in Iterator;
                 Val : in Contained_Type)
                 return Boolean;
   function "=" (Val : in Contained_Type;
                 Iter : in Iterator)
                 return Boolean;

   procedure Add_Link (From       : in out Iterator;
                       To         : in out Iterator;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True);

   procedure Add_Link (From       : in out Iterator;
                       To         : in Contained_Type;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True);

   procedure Add_Link (From       : in Contained_Type;
                       To         : in out Iterator;
                       Contents   : in Link_Contained_Type;
                       Ignore_Dup : in Boolean      := True);

   procedure Delete_Link (Iter : in out Iterator; Is_End : out End_Marker);

   function Find_Link (From : in Iterator;
                       To   : in Iterator)
                       return Iterator;

   function Find_Link (From : in Iterator;
                       To   : in Contained_Type)
                       return Iterator;

   function Find_Link (From : in Contained_Type;
                       To   : in Iterator)
                       return Iterator;

   procedure Find_Link (From  : in out Iterator;
                        To    : in Iterator;
                        Found : out Boolean);

   procedure Find_Link (From  : in out Iterator;
                        To    : in Contained_Type;
                        Found : out Boolean);

   procedure Find_Link_Again (From  : in out Iterator;
                              Found : out Boolean);

   function Link_Exists (From  : in Iterator;
                         To    : in Iterator)
                         return Boolean;

   function Link_Exists (From  : in Iterator;
                         To    : in Contained_Type)
                         return Boolean;

   function Link_Exists (From  : in Contained_Type;
                         To    : in Iterator)
                         return Boolean;

   procedure First_Link (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Next_Link (Iter : in out Iterator; Is_End : out End_Marker);

   function Follow_Link (Iter : in Iterator) return Iterator;

   procedure Follow_Link (Iter : in out Iterator);

   function Get_Link (Iter : in Iterator) return Link_Contained_Type;

   procedure Set_Link (Iter : in out Iterator;
                       Val  : in Link_Contained_Type);

   function Link_Count (Iter : in Iterator) return Natural;

   procedure Finalize (Iter : in out Iterator);

private

   -- The graph nodes are implemented as a hash table.  We can't use the
   -- existing hash table because we can't instantiate it (we are in the
   -- same hierarchy) and because we need a two-way linked list to handle a
   -- graph properly.  Since we can visit a node using a link, we won't
   -- have the previous pointer the iterator needs to be able to delete
   -- links, so we need a two-way linked list.

--$START FIXED
   subtype Node_Ref is Integer;
   Null_Node : constant Node_Ref := 0;
--$END FIXED
--$START EXPANDABLE
   subtype Node_Ref is Integer;
   Null_Node : constant Node_Ref := 0;
--$END EXPANDABLE

--$START DYNAMIC
   type Node;
   type Node_Ptr is access all Node;
--$START STORAGE_POOL
   for Node_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL
   type Node is new Node_Base with record
      Val    : Contained_Type;
      Next   : REF_VAL        := NULL_REF;
      Prev   : REF_VAL        := NULL_REF;

      Links      : aliased Link_List;
--$START DIGRAPH
      -- Links back to things that have links to us.
      From_Links : aliased Link_List;
--$END DIGRAPH
   end record;

   type Hash_Array is array (Positive range <>) of REF_VAL;

   type Object (Hash_Size : Positive) is new Object_Parent with record
      Nodes : Hash_Array(1 .. Hash_Size) := (1 .. Hash_Size => NULL_REF);
      Count : Natural                    := 0;
   end record;
--$END DYNAMIC
--$START FIXED
   type Node is record
      Val    : Contained_Type;
      Next   : REF_VAL        := NULL_REF;
      Prev   : REF_VAL        := NULL_REF;

      Links      : aliased Link_List;
--$START DIGRAPH
      -- Links back to things that have links to us.
      From_Links : aliased Link_List;
--$END DIGRAPH
   end record;

   type Hash_Array is array (Positive range <>) of REF_VAL;

   type Node_Array is array (Positive range <>) of Node;

   type Object (Hash_Size : Positive;
                Size      : Positive)
     is new Object_Parent with record
      Nodes     : Hash_Array(1 .. Hash_Size)  := (1 .. Hash_Size => NULL_REF);
      Count     : Natural                     := 0;
      Data      : Node_Array(1 .. Size);
      Free_List : REF_VAL                     := NULL_REF;
   end record;
--$END FIXED
--$START EXPANDABLE
   type Node is record
      Val    : Contained_Type;
      Next   : REF_VAL        := NULL_REF;
      Prev   : REF_VAL        := NULL_REF;

      Links      : aliased Link_List;
--$START DIGRAPH
      -- Links back to things that have links to us.
      From_Links : aliased Link_List;
--$END DIGRAPH
   end record;

   type Hash_Array is array (Positive range <>) of REF_VAL;

   type Node_Array is array (Positive range <>) of Node;
   type Node_Array_Ptr is access all Node_Array;
--$START STORAGE_POOL
   for Node_Array_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL

   type Object (Hash_Size    : Positive;
                Initial_Size : Positive;
                Increment    : Natural)
     is new Object_Parent with record
      Nodes     : Hash_Array(1 .. Hash_Size) := (1 .. Hash_Size => NULL_REF);
      Count     : Natural                    := 0;
      Data      : Node_Array_Ptr         := new Node_Array(1 .. Initial_Size);
      Free_List : REF_VAL                    := NULL_REF;
   end record;
--$END EXPANDABLE

   type Iterator is new Iterator_Parent with record
      Robj     : Object_Class := null;
      Curr     : REF_VAL      := NULL_REF;

      -- The iterator that references the links for the current node.
      Links_It : Link_List_It;
   end record;

end Asgc.Graph.CTYPE.GRAPHTYPEMANAGED;
