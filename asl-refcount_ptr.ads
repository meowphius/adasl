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

with Ada.Finalization; use Ada.Finalization;

-- A reference counting pointer.  This creates a limited type that holds
-- a pointer to something.  The numbers of copies of the pointer is
-- referenced counted, when it reaches zero the pointer is freed.
-- The pointer cannot be copied with ":=" because it is limited, instead
-- use the "Duplicate" procedure.  If a pointer is duplicated, it must
-- be freed.
--
-- A pointer starts out as "null" (null can be tested with the Is_Null
-- function), and may be assigned a value with Allocate or Duplicate.
--
generic
   -- The type contained in the pointer.
   type Contained(<>) is limited private;

   -- The actual pointer type.
   type Contained_Ptr is access Contained;

   -- A routine that will free a pointer to the type.
   with procedure Free(Item : in out Contained_Ptr);

package Asl.Refcount_Ptr is

   -- The is the reference counting pointer, put one of these in your
   -- declarations.
   type Counted_Ptr is new Limited_Controlled with private;

   -- Allocate a counted pointer with Ptr as the value.  If Ptr is null,
   -- the value in the pointer is freed and the pointer will not exist
   -- any more (same as calling Free).  If Ret already holds a valid
   -- pointer,
   procedure Allocate(Ret : in out Counted_Ptr; Ptr : in Contained_Ptr);

   -- Clear the contents of a pointer and free it if it is the last user.
   -- This will set the pointer to null.
   procedure Free(Ptr : in out Counted_Ptr);

   -- Copy the value of a pointer from Ptr to Ret, and increment it's
   -- reference count.  If Ret already holds a value, it will be freed.
   procedure Duplicate(Ret : in out Counted_Ptr; Ptr : in Counted_Ptr);

   -- Get the pointer contained in the counted pointer.
   function Get(Ptr : in Counted_Ptr) return Contained_Ptr;

   -- Get the contents of the pointer contained in the counted pointer.
   function Get(Ptr : in Counted_Ptr) return Contained;

   -- Return true if the counted pointer is null, false if not.
   function Is_Null(Ptr : in Counted_Ptr) return Boolean;

private

   -- Make sure the pointer gets cleaned up properly in all circumstances
   -- we possible can.
   procedure Finalize(Ptr : in out Counted_Ptr);

   -- We wrap the thing in a protected type to handle multi-threaded
   -- situations.
   protected type Counted_Record is
      procedure Init(New_Item : in Contained_Ptr);
      procedure Incr;
      procedure Decr(Done : out Boolean);
      function Get return Contained_Ptr;
      function Get return Contained;
   private
      Refcount : Positive := 1;
      Item     : Contained_Ptr;
   end Counted_Record;
   type Counted_Record_Ptr is access all Counted_Record;

   type Counted_Ptr is new Limited_Controlled with record
      Holder : Counted_Record_Ptr := null;
   end record;

end Asl.Refcount_Ptr;
