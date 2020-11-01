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
package Asgc.Btree.DynamicMANAGED is

   ------------------------------------------------------------------------
   type Object (Allow_Duplicates : Boolean;
                Node_Size        : Btree_Node_Size)
     is new Asgc.Btree.Object with private;
   type Object_Class is access all Object'Class;
   type Object_Ptr is access all Object;
--$START STORAGE_POOL
   for Object_Ptr'Storage_Pool use My_Pool;
   for Object_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL

   procedure Add (O : in out Object; Val : in Contained_Type);

   function "=" (O1, O2 : in Object) return Boolean;

   procedure Delete (O   : in out Object;
                     Val : in Contained_Type);

   function Value_Exists (O   : in Object;
                          Val : in Contained_Type)
                          return Boolean;

   function Member_Count (O : in Object)
                          return Natural;

   procedure Verify_Integrity (O : in Object);

   function Copy (O : in Object) return Asgc.Object_Class;

   procedure Initialize (O : in out Object);
   procedure Adjust (O : in out Object);
   procedure Finalize (O : in out Object);

   ------------------------------------------------------------------------
   type Iterator is new Asgc.Btree.Iterator with private;
   type Iterator_Class is access all Iterator'Class;
   type Iterator_Ptr is access all Iterator;
--$START STORAGE_POOL
   for Iterator_Ptr'Storage_Pool use My_Pool;
   for Iterator_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL


   function New_Iterator (O : access Object) return Asgc.Iterator_Class;

   function New_Iterator (O : in Object_Class) return Iterator;

   procedure Free (Iter : access Iterator);

   procedure Set_Container (Iter : in out Iterator;
                            O    : in Asgc.Object_Class);

   procedure Add (Iter : in out Iterator;
                  Val  : in Contained_Type);

   procedure First (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Last (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean);

   procedure Search_Again (Iter  : in out Iterator;
                           Found : out Boolean);

   procedure Next (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Prev (Iter : in out Iterator; Is_End : out End_Marker);

   procedure Delete (Iter : in out Iterator; Is_End : out End_Marker);

   function Is_Same (Iter1, Iter2 : in Iterator) return Boolean;

   function Get (Iter : in Iterator) return Contained_Type;

   procedure Get_Incr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker);

   procedure Get_Decr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker);

   function "=" (Iter1, Iter2 : in Iterator) return Boolean;

   function "=" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function "=" (Val : in Contained_Type; Iter : in Iterator) return Boolean;

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

   procedure Finalize (Iter : in out Iterator);


   Internal_Btree_Error : exception;

private

   type Contained_Array is array (Positive range <>) of Contained_Type;

   -- A node is a circular list in an array.  This makes doing the shifts
   -- due to an insertion or removal faster.
   type Node;
   type Node_Ptr is access all Node;
--$START STORAGE_POOL
   for Node_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL
   type Node_Ptr_Array is array (Positive range <>) of Node_Ptr;
   type Node (Size : Btree_Node_Size;
              Leaf : Boolean) is record
      First        : Positive; -- The index of the first item in the node.
      Last         : Positive; -- The index of the last item in the node
      Parent       : Node_Ptr := null; -- My parent.
      -- If Parent_Index is past my parents size, then I am a right child.
      -- The index of my parent's reference to me.
      Parent_Index : Positive := 1;
      Vals         : Contained_Array(1 .. Size);
      case Leaf is
         when True  => null;
         when False =>
           Children    : Node_Ptr_Array(1 .. Size);
           Right_Child : Node_Ptr;  -- Holds the rightmost child.
      end case;
   end record;

   type Object (Allow_Duplicates : Boolean;
                Node_Size        : Btree_Node_Size)
     is new Asgc.Btree.Object with record
      Root  : Node_Ptr := new Node(Size => Node_Size,
                                   Leaf => True);
      Count : Natural  := 0;
   end record;

   type Iterator is new Asgc.Btree.Iterator with record
        Robj  : Object_Class := null;
        Pos   : Node_Ptr     := null;
        Index : Positive     := 1;
   end record;

end Asgc.Btree.DynamicMANAGED;
