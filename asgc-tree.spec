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
--$START STORAGE_POOL
   type Pool_Type is new System.Storage_Pools.Root_Storage_Pool with private;
   My_Pool : in out Pool_Type;
--$END STORAGE_POOL
package Asgc.Tree.CTYPEMANAGED is

   subtype Parent_Object is Asgc.Tree.Object;
--$START DYNAMIC
   type Object (Balanced : Boolean) is new Parent_Object with private;
--$END DYNAMIC
--$START FIXED
   type Object (Balanced : Boolean;
                Size     : Positive)
     is new Parent_Object with private;
--$END FIXED
--$START EXPANDABLE
   type Object (Balanced     : Boolean;
                Initial_Size : Positive;
                Increment    : Natural)
     is new Parent_Object with private;
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

   function "=" (O1, O2 : in Object) return Boolean;

   procedure Verify_Integrity (O : in Object);

   function Copy (O : in Object) return Asgc.Object_Class;

   procedure Initialize (O : in out Object);

   procedure Adjust (O : in out Object);

   procedure Finalize (O : in out Object);


   ------------------------------------------------------------------------
   subtype Parent_Iterator is Asgc.Tree.Iterator;
   type Iterator is new Parent_Iterator with private;
   type Iterator_Class is access all Iterator'Class;
   type Iterator_Ptr is access all Iterator;
--$START STORAGE_POOL
   for Iterator_Ptr'Storage_Pool use My_Pool;
   for Iterator_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL


   procedure Add (Iter : in out Iterator;
                  Val  : in Contained_Type);

   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean);

   procedure Root (Iter   : in out Iterator;
                   Is_End : out End_Marker);

   procedure Left (Iter   : in out Iterator;
                   Is_End : out End_Marker);

   procedure Right (Iter   : in out Iterator;
                    Is_End : out End_Marker);

   procedure Up (Iter   : in out Iterator;
                 Is_End : out End_Marker);

   procedure Last (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Prev (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Get_Decr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker);

   procedure Finalize (Iter : in out Iterator);

   function ">" (Iter1, Iter2 : in Iterator) return Boolean;

   function ">" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function ">" (Val : in Contained_Type; Iter : in Iterator) return Boolean;

   function "<" (Iter1, Iter2 : in Iterator) return Boolean;

   function "<" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function "<" (Val : in Contained_Type; Iter : in Iterator) return Boolean;

   function ">=" (Iter1, Iter2 : in Iterator) return Boolean;

   function ">=" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function ">=" (Val : in Contained_Type; Iter : in Iterator) return Boolean;

   function "<=" (Iter1, Iter2 : in Iterator) return Boolean;

   function "<=" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function "<=" (Val : in Contained_Type; Iter : in Iterator) return Boolean;


   -- Abstract functions this must implement
   function New_Iterator (O : access Object) return Asgc.Iterator_Class;

   function New_Iterator (O : in Object_Class) return Iterator;

   procedure Free (Iter : access Iterator);

   procedure Set_Container (Iter : in out Iterator;
                            O    : in Asgc.Object_Class);

   procedure First (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Next (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Delete (Iter : in out Iterator; Is_End : out End_Marker);

   function Is_Same (Iter1, Iter2 : in Iterator) return Boolean;

   function Get (Iter : in Iterator) return Contained_Type;

   procedure Get_Incr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker);

   function "=" (Iter1, Iter2 : in Iterator) return Boolean;

   function "=" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function "=" (Val : in Contained_Type; Iter : in Iterator) return Boolean;

private

   -- A tree data structure is pretty straighforward, each node has a left,
   -- right, and up reference, an AVL balance value, and the contained
   -- value.

   -- The balance types of an AVL tree node.
   type Node_Balance is ('+', '=', '-');

   -- The position of a node with respect to the node above it.  If the
   -- current node comes from the left branch above, it is Left.  If the
   -- right branch then Right.  If the current node is the root, then None.
   type Last_Direction is (Left, Right, None);

--$START DYNAMIC
   type Node;
   type Node_Ptr is access all Node;
--$START STORAGE_POOL
   for Node_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL
   type Node is record
      Left, Right, Up : Node_Ptr        := null;
      Balance         : Node_Balance    := '=';
      Val             : Contained_Type;
   end record;

   type Object (Balanced : Boolean) is new Parent_Object with record
      Root  : Node_Ptr := null;
      Count : Natural  := 0;
   end record;
--$END DYNAMIC

--$START FIXED
   subtype Node_Ref is Integer;
   Null_Node : constant Node_Ref := 0;

   type Node is tagged record
      Left, Right, Up : Node_Ref       := Null_Node;
      Balance         : Node_Balance   := '=';
      Val             : Contained_Type;
   end record;
   type Node_Array is array (Node_Ref range <>) of Node;

   type Object (Balanced : Boolean;
                Size     : Positive) is new Parent_Object with record
      Data  : Node_Array(1 .. Size);
      Root  : Node_Ref         := Null_Node;
      Count : Natural          := 0;

      Free_List : Node_Ref := Null_Node;
   end record;
--$END FIXED

--$START EXPANDABLE
   subtype Node_Ref is Integer;
   Null_Node : constant Node_Ref := 0;

   type Node is tagged record
      Left, Right, Up : Node_Ref       := Null_Node;
      Balance         : Node_Balance   := '=';
      Val             : Contained_Type;
   end record;
   type Node_Array is array (Node_Ref range <>) of Node;
   type Node_Array_Ptr is access all Node_Array;
--$START STORAGE_POOL
   for Node_Array_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL

   type Object (Balanced     : Boolean;
                Initial_Size : Positive;
                Increment    : Natural)
   is new Parent_Object with record
      Data  : Node_Array_Ptr := new Node_Array(1 .. Initial_Size);
      Root  : Node_Ref       := Null_Node;
      Count : Natural        := 0;

      Free_List : Node_Ref := Null_Node;
   end record;
--$END EXPANDABLE

   type Iterator is new Parent_Iterator with record
      Robj : Object_Class := null;
      Pos  : REF_VAL      := NULL_REF;
   end record;

end Asgc.Tree.CTYPEMANAGED;
