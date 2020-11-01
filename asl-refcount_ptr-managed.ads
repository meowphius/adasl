-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 2001  Corey Minyard (minyard@acm.org)
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

-- Like Asl.Refcount_Ptr, but with a specified storage pool.  No differences
-- besides that.
generic
   type Pool_Type is new System.Storage_Pools.Root_Storage_Pool with private;
   My_Pool : in out Pool_Type;
package Asl.Refcount_Ptr.Managed is

   -- The is the reference counting pointer, put one of these in your
   -- declarations.
   type Managed_Counted_Ptr is new Counted_Ptr with private;

   procedure Allocate(Ret : in out Managed_Counted_Ptr;
                      Ptr : in Contained_Ptr);

   procedure Free(Ptr : in out Managed_Counted_Ptr);

private

   type Managed_Counted_Ptr is new Counted_Ptr with null record;
   type Managed_Record_Ptr is access all Counted_Record;
   for Managed_Record_Ptr'Storage_Pool use My_Pool;

end Asl.Refcount_Ptr.Managed;
