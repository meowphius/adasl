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

with Ada.Unchecked_Deallocation;
with Asl.Refcount_Ptr.Managed;
with System.Storage_Pools;

-- Like Asl.Refcount_String, but with a specified storage pool.  No differences
-- besides that.
generic
   type Pool_Type is new System.Storage_Pools.Root_Storage_Pool with private;
   My_Pool : in out Pool_Type;
package Asl.Refcount_String.Managed is

   type Managed_String_Ptr is access all String;
   for Managed_String_Ptr'Storage_Pool use My_Pool;

   procedure Free is new Ada.Unchecked_Deallocation(String,
                                                    Managed_String_Ptr);

   package Main_String_Inst is new Asl.Refcount_Ptr(String,
                                                    Managed_String_Ptr,
                                                    Free);
   package String_Inst is new Main_String_Inst.Managed(Pool_Type,
                                                       My_Pool);

   subtype Managed_Counted_String is String_Inst.Managed_Counted_Ptr;

   procedure Allocate(Ret : in out Managed_Counted_String; Str : in String);

   procedure Free(Str : in out Managed_Counted_String)
     renames String_Inst.Free;

   procedure Duplicate(Ret : in out Managed_Counted_String;
                       Str : in Managed_Counted_String)
     renames String_Inst.Duplicate;

   function Get(Str : in Managed_Counted_String) return String
     renames String_Inst.Get;

   function Is_Null(Str : in Managed_Counted_String) return Boolean
     renames String_Inst.Is_Null;

end Asl.Refcount_String.Managed;
