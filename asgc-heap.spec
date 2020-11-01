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
package Asgc.Heap.CTYPEMANAGED is

   ------------------------------------------------------------------------
--$START DYNAMIC
   type Object is new Asgc.Heap.Object with private;
--$END DYNAMIC
--$START FIXED
   type Object (Size : Positive) is new Asgc.Heap.Object with private;
--$END FIXED
--$START EXPANDABLE
   type Object (Initial_Size : Positive;
                Increment    : Natural)
     is new Asgc.Heap.Object with private;
--$END EXPANDABLE
   type Object_Class is access all Object'Class;
   type Object_Ptr is access all Object;
--$START STORAGE_POOL
   for Object_Ptr'Storage_Pool use My_Pool;
   for Object_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL


   procedure Add (O : in out Object;
                  Val : in Contained_Type);

   procedure Delete (O : in out Object;
                     Val : in Contained_Type);

   function Value_Exists (O   : in Object;
                          Val : in Contained_Type)
                          return Boolean;

   function Member_Count (O : in Object)
                          return Natural;

   function Get_Head (O : in Object)
                      return Contained_Type;

   procedure Remove_Head (O   : in out Object;
                          Val : out Contained_Type);

   function "=" (O1, O2 : in Object) return Boolean;

   procedure Verify_Integrity (O : in Object);

   function Copy (O : in Object) return Asgc.Object_Class;

   procedure Initialize (O : in out Object);
   procedure Adjust (O : in out Object);
   procedure Finalize (O : in out Object);

   ------------------------------------------------------------------------
   type Iterator is new Asgc.Heap.Iterator with private;
   type Iterator_Class is access all Iterator'Class;
   type Iterator_Ptr is access all Iterator;
--$START STORAGE_POOL
   for Iterator_Ptr'Storage_Pool use My_Pool;
   for Iterator_Class'Storage_Pool use My_Pool;
--$END STORAGE_POOL


   function New_Iterator (O : access Object) return Asgc.Iterator_Class;

   function New_Iterator (O : in Object_class) return Iterator;

   procedure Free (Iter : access Iterator);

   procedure Set_Container (Iter : in out Iterator;
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

   function "=" (Iter1, Iter2 : in Iterator) return Boolean;

   function "=" (Iter : in Iterator; Val : in Contained_Type) return Boolean;

   function "=" (Val : in Contained_Type; Iter : in Iterator) return Boolean;

   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean);

   procedure Finalize (Iter : in out Iterator);

private

--$START DYNAMIC
   -- Dynamic heaps are implemented as a tree.
   type Node;
   type Node_Ptr is access all Node;
--$START STORAGE_POOL
   for Node_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL
   type Node is record
      Left, Right, Up : Node_Ptr       := null;
      Val             : Contained_Type;
   end record;

   type Object is new Asgc.Heap.Object with record
      Head, Tail : Node_Ptr   := null;
      Count      : Natural    := 0;
   end record;
--$END DYNAMIC

--$START FIXED
   -- Fixed heaps are simply an array.
   subtype Node_Ref is Integer;
   Null_Node : constant Node_Ref := 0;

   type Node is record
      Val : Contained_Type;
   end record;

   type Heap_Array is array (Integer range <>) of Node;

   type Object (Size : Positive) is new Asgc.Heap.Object with record
      Data  : Heap_Array(1 .. Size);
      Head  : Node_Ref              := Null_Node;
      Tail  : Node_Ref              := Null_Node;
      Count : Natural               := 0;
   end record;
--$END FIXED

--$START EXPANDABLE
   -- Expandable heaps are simply an array.
   subtype Node_Ref is Integer;
   Null_Node : constant Node_Ref := 0;

   type Node is record
      Val : Contained_Type;
   end record;

   type Heap_Array is array (Integer range <>) of Node;
   type Heap_Array_Ptr is access all Heap_Array;
--$START STORAGE_POOL
   for Heap_Array_Ptr'Storage_Pool use My_Pool;
--$END STORAGE_POOL

   type Object (Initial_Size : Positive;
                Increment    : Natural)
   is new Asgc.Heap.Object with record
      Data  : Heap_Array_Ptr := new Heap_Array(1 .. Initial_Size);
      Head  : Node_Ref       := Null_Node;
      Tail  : Node_Ref       := Null_Node;
      Count : Natural        := 0;
   end record;
--$END EXPANDABLE

   type Iterator is new Asgc.Heap.Iterator with record
        Robj : Object_Class := null;
        Pos  : REF_VAL      := NULL_REF;
   end record;

end Asgc.Heap.CTYPEMANAGED;
