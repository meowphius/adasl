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

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body Asl.Leak_Detect_Pool is

   package Conv is new System.Address_To_Access_Conversions(Pool_Element);

   -- The size of the header in storage elements.
   Header_Size : System.Storage_Elements.Storage_Count
     := Pool_Element'Size / System.Storage_Elements.Storage_Element'Size;

   -- The offset back from the data address we return of the header.
   Header_Offset : System.Storage_Elements.Storage_Count;


   ------------------------------------------------------------------------
   procedure Allocate
     (Pool         : in out Leak_Pool;
      Address      : out System.Address;
      Storage_Size : in System.Storage_Elements.Storage_Count;
      Alignment    : in System.Storage_Elements.Storage_Count)
   is
      My_Alignment  : System.Storage_Elements.Storage_Count := Alignment;
      Total_Header  : System.Storage_Elements.Storage_Count;
      Alloc_Size    : System.Storage_Elements.Storage_Count;
      Ret_Address   : System.Address;
   begin
      -- Make sure the alignment is at least 4 storage elements.
      if ((My_Alignment mod 4) /= 0) then
         My_Alignment := My_Alignment + (4 - (My_Alignment mod 4));
      end if;

      -- Now calculate the amount of data to allocate.  First we need the
      -- size for the header, being careful to allow for any alignment.
      if ((Header_Size mod My_Alignment) /= 0) then
         Total_Header := (Header_Size
                          + (My_Alignment - (Header_Size mod My_Alignment)));
      else
         Total_Header := Header_Size;
      end if;

      -- Now the size to allocate.
      Alloc_Size := Storage_Size + Total_Header;

      declare
         -- Create local fixed types to allocate the data with.  We can't
         -- use unconstrained pointers because the array sizes might be
         -- carried in the data pointed to.  Make sure it uses the standard
         -- global storage pool.
         subtype Real_Data is Alloc_Data(0 .. (Alloc_Size - 1));
         type Real_Data_Ptr is access all Real_Data;
         for Real_Data_Ptr'Storage_Pool use Alloc_Data_Ptr'Storage_Pool;

         -- Here is the data from the standard global storage pool
         Alloc_Block  : Real_Data_Ptr := new Real_Data;
         Header       : Conv.Object_Pointer
           := Conv.To_Pointer(Alloc_Block(Total_Header-Header_Offset)'Address);
      begin
         Header.Magic := Leak_Pool_Magic;
         Header.Size := Storage_Size;
         Header.Real_Size := Alloc_Size;

         -- put the element into the list
         if (Pool.Elements /= null) then
            Pool.Elements.Prev := Pool_Element_Ptr(Header);
         end if;
         Header.Next := Pool.Elements;
         Header.Prev := null;
         Pool.Elements := Pool_Element_Ptr(Header);
         Ret_Address := Alloc_Block(Total_Header)'Address;
         Address := Ret_Address;
      end;
   end Allocate;


   ------------------------------------------------------------------------
   procedure Deallocate
     (Pool         : in out Leak_Pool;
      Address      : in System.Address;
      Storage_Size : in System.Storage_Elements.Storage_Count;
      Alignment    : in System.Storage_Elements.Storage_Count)
   is
      Header      : Conv.Object_Pointer
        := Conv.To_Pointer(Address - Header_Offset);
   begin
      if (Header.Magic /= Leak_Pool_Magic) then
         raise Deallocate_Of_Invalid_Data;
      end if;

      -- Remove the element from the list.
      if (Header.Prev /= null) then
         Header.Prev.Next := Header.Next;
      else
         Pool.Elements := Header.Next;
      end if;
      if (Header.Next /= null) then
         Header.Next.Prev := Header.Prev;
      end if;

      -- Create a bunch of types to create a data pointer to free.
      declare
         subtype Real_Data is Alloc_Data(0 .. (Header.Real_Size - 1));
         type Real_Data_Ptr is access all Real_Data;
         for Real_Data_Ptr'Storage_Pool use Alloc_Data_Ptr'Storage_Pool;
         procedure Free is
           new Ada.Unchecked_Deallocation(Real_Data, Real_Data_Ptr);

         -- I'd like to use access to address conversions, but we can't
         -- define storage pools for those pointers.
         Alloc_Block  : aliased Real_Data;
         for Alloc_Block'Address
           use Address - (Header.Real_Size - Header.Size);
         To_Free : Real_Data_Ptr := Alloc_Block'Unchecked_Access;
      begin
         Free(To_Free);
      end;
   end Deallocate;


   ------------------------------------------------------------------------
   function Storage_Size (Pool : Leak_Pool)
                          return System.Storage_Elements.Storage_Count is
   begin
      -- We use the default pool's size.
      return System.Storage_Pools.Storage_Size
        (System.Storage_Pools.Root_Storage_Pool'Class
         (Alloc_Data_Ptr'Storage_Pool));
   end Storage_Size;


   ------------------------------------------------------------------------
   procedure First (Pool    : in Leak_Pool;
                    Iter    : in out Iterator;
                    Is_End  : out End_Marker;
                    Address : out System.Address;
                    Size    : out System.Storage_Elements.Storage_Count) is
   begin
      Iter.Curr := Pool.Elements;
      if (Iter.Curr = null) then
         Is_End := Past_End;
      else
         Is_End := Not_Past_End;
         Address := Iter.Curr.all'Address + Header_Offset;
         Size := Iter.Curr.Size;
      end if;
   end First;


   ------------------------------------------------------------------------
   procedure Next (Iter    : in out Iterator;
                   Is_End  : out End_Marker;
                   Address : out System.Address;
                   Size    : out System.Storage_Elements.Storage_Count) is
   begin
      Iter.Curr := Iter.Curr.Next;
      if (Iter.Curr = null) then
         Is_End := Past_End;
      else
         Is_End := Not_Past_End;
         Address := Iter.Curr.all'Address + Header_Offset;
         Size := Iter.Curr.Size;
      end if;
   end Next;

begin
   -- Calculate the actual starting place of the header relative to the
   -- data we are returning.
   if ((Header_Size mod 4) /= 0) then
      Header_Offset := Header_Size + (4 - (Header_Size mod 4));
   else
      Header_Offset := Header_Size;
   end if;
end Asl.Leak_Detect_Pool;
