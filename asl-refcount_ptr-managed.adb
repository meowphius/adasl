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

package body Asl.Refcount_Ptr.Managed is

   procedure Free_It is new Ada.Unchecked_Deallocation(Counted_Record,
                                                       Managed_Record_Ptr);

   procedure Allocate(Ret : in out Managed_Counted_Ptr;
                      Ptr : in Contained_Ptr) is
      Retval : Managed_Record_Ptr := new Counted_Record;
   begin
      if (Ret.Holder /= null) then
         Free(Ret);
      end if;
      if (Ptr /= null) then
         Retval.all.Init(Ptr);
         Ret.Holder := Counted_Record_Ptr(Retval);
      end if;
   end Allocate;

   procedure Free(Ptr : in out Managed_Counted_Ptr) is
      Done    : Boolean;
      To_Free : Managed_Record_Ptr;
   begin
      Ptr.Holder.all.Decr(Done);
      if (Done) then
         To_Free := Managed_Record_Ptr(Ptr.Holder);
         Free_It(To_Free);
      end if;
      Ptr.Holder := null;
   end Free;

end Asl.Refcount_Ptr.Managed;
