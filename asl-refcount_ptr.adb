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

with Ada.Text_IO; use Ada.Text_IO;

package body Asl.Refcount_Ptr is

   procedure Free_It is new Ada.Unchecked_Deallocation(Counted_Record,
                                                       Counted_Record_Ptr);

   protected body Counted_Record is
      procedure Init(New_Item : in Contained_Ptr) is
      begin
         Item := New_Item;
      end Init;

      procedure Incr is
      begin
         Refcount := Refcount + 1;
      end Incr;

      procedure Decr(Done : out Boolean) is
      begin
         if (Refcount = 1) then
            Free(Item);
            Done := True;
         else
            Refcount := Refcount - 1;
            Done := False;
         end if;
      end Decr;

      function Get return Contained_Ptr is
      begin
         return Item;
      end Get;

      function Get return Contained is
      begin
         return Item.all;
      end Get;

   end Counted_Record;

   procedure Allocate(Ret : in out Counted_Ptr; Ptr : in Contained_Ptr) is
      Retval : Counted_Record_Ptr := new Counted_Record;
   begin
      if (Ret.Holder /= null) then
         Free(Counted_Ptr'Class(Ret));
      end if;
      if (Ptr /= null) then
         Retval.all.Init(Ptr);
         Ret.Holder := Retval;
      end if;
   end Allocate;

   procedure Free(Ptr : in out Counted_Ptr) is
      Done : Boolean;
   begin
      Ptr.Holder.all.Decr(Done);
      if (Done) then
         Free_It(Ptr.Holder);
      end if;
      Ptr.Holder := null;
   end Free;

   procedure Duplicate(Ret : in out Counted_Ptr; Ptr : in Counted_Ptr) is
   begin
      if (Ret.Holder /= null) then
         Free(Counted_Ptr'Class(Ret));
      end if;
      if (Ptr.Holder /= null) then
         Ptr.Holder.all.Incr;
      end if;
      Ret.Holder := Ptr.Holder;
   end Duplicate;

   function Get(Ptr : in Counted_Ptr) return Contained_Ptr is
   begin
      if (Ptr.Holder /= null) then
         return Ptr.Holder.all.Get;
      else
         return null;
      end if;
   end Get;

   function Get(Ptr : in Counted_Ptr) return Contained is
   begin
      return Ptr.Holder.all.Get.all;
   end Get;

   function Is_Null(Ptr : in Counted_Ptr) return Boolean is
   begin
      return Ptr.Holder = null;
   end Is_Null;

   procedure Finalize(Ptr : in out Counted_Ptr) is
   begin
      if (Ptr.Holder /= null) then
         Free(Counted_Ptr'Class(Ptr));
      end if;
   end Finalize;
end Asl.Refcount_Ptr;
