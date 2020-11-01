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

with System.Storage_Pools;
with System.Storage_Elements;

package Asl.Leak_Detect_Pool is

   type Leak_Pool is new System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate
     (Pool         : in out Leak_Pool;
      Address      : out System.Address;
      Storage_Size : in System.Storage_Elements.Storage_Count;
      Alignment    : in System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool         : in out Leak_Pool;
      Address      : in System.Address;
      Storage_Size : in System.Storage_Elements.Storage_Count;
      Alignment    : in System.Storage_Elements.Storage_Count);

   function Storage_Size (Pool : Leak_Pool)
                          return System.Storage_Elements.Storage_Count;

   Deallocate_Of_Invalid_Data : exception;

   type End_Marker is (Past_End, Not_Past_End);

   type Iterator is private;

   -- Go to the first element in the pool.  Iter will be set to Past_End if
   -- the pool is empty.  Otherwise, the address and size will be returned.
   procedure First (Pool    : in Leak_Pool;
                    Iter    : in out Iterator;
                    Is_End  : out End_Marker;
                    Address : out System.Address;
                    Size    : out System.Storage_Elements.Storage_Count);

   -- Go to the next element in the pool.  Iter will be set to Past_End if
   -- at the pool end.  Otherwise, the address and size will be returned.
   procedure Next (Iter    : in out Iterator;
                   Is_End  : out End_Marker;
                   Address : out System.Address;
                   Size    : out System.Storage_Elements.Storage_Count);

private

   use type System.Storage_Elements.Storage_Count;

   type Magic_Number is mod 2 ** 32;
   for Magic_Number'Size use 32;

   Leak_Pool_Magic : constant Magic_Number := 16#80fdb14c#;

   type Alloc_Data is array (System.Storage_Elements.Storage_Count range <>) of
     aliased System.Storage_Elements.Storage_Element;
   for Alloc_Data'Alignment use 4;
   type Alloc_Data_Ptr is access all Alloc_Data;

   type Pool_Element;
   type Pool_Element_Ptr is access all Pool_Element;
   type Pool_Element is record
      Magic     : Magic_Number;
      Size      : System.Storage_Elements.Storage_Count;
      Real_Size : System.Storage_Elements.Storage_Count;
      Next      : Pool_Element_Ptr;
      Prev      : Pool_Element_Ptr;
   end record;

   type Leak_Pool is new System.Storage_Pools.Root_Storage_Pool with record
      Elements : Pool_Element_Ptr := null;
   end record;

   type Iterator is record
      Curr : Pool_Element_Ptr;
   end record;

end Asl.Leak_Detect_Pool;
