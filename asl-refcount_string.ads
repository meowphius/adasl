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
with Asl.Refcount_Ptr;

-- A reference counting string.  The string value is allocated and will
-- be held inside the pointer.  The string value is immutable  These
-- calls work like Asl.Refcount_Ptr, see that package for more details.
package Asl.Refcount_String is

   type String_Ptr is access all String;

   procedure Free is new Ada.Unchecked_Deallocation(String, String_Ptr);

   package String_Inst is new Asl.Refcount_Ptr(String, String_Ptr, Free);

   subtype Counted_String is String_Inst.Counted_Ptr;

   procedure Allocate(Ret : in out Counted_String; Str : in String);

   procedure Free(Str : in out Counted_String) renames String_Inst.Free;

   procedure Duplicate(Ret : in out Counted_String; Str : in Counted_String)
     renames String_Inst.Duplicate;

   function Get(Str : in Counted_String) return String renames String_Inst.Get;

   function Is_Null(Str : in Counted_String) return Boolean
     renames String_Inst.Is_Null;

end Asl.Refcount_String;
