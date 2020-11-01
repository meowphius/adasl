-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 2001  Corey Minyard (minyard@acm.org)
--
-- This code is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This code is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--

with Text_IO; use Text_IO;
with Asl.Leak_Detect_Pool; use Asl.Leak_Detect_Pool;
with Test_Refcount_Pack; use Test_Refcount_Pack;
use Test_Refcount_Pack.Strpack;
with System;
with System.Storage_Elements;

procedure Test_Refcount_Ptr is
   function Is_Empty(Pool : Leak_Pool) return Boolean is
      It     : Iterator;
      Is_End : End_Marker;
      Addr   : System.Address;
      Size   : System.Storage_Elements.Storage_Count;
   begin
      First(Pool, It, Is_End, Addr, Size);
      return Is_End = Past_End;
   end Is_Empty;
begin
   -- Test an empty usage.
   declare
      Str1 : Managed_Counted_String;
   begin
      null;
   end;

   declare
      Str1 : Managed_Counted_String;
   begin
      Allocate(Str1, "Hello");
      if (Get(Str1) /= "Hello") then
         Put_Line("str1 didn't return 'Hello', returned '"
                  & Get(Str1) & "' instead");
         raise Constraint_Error;
      end if;
   end;

   if (not Is_Empty(My_Pool)) then
      Put_Line("Pool not empty (1)");
      raise Constraint_Error;
   end if;

   declare
      Str1 : Managed_Counted_String;
      Str2 : Managed_Counted_String;
   begin
      if (not Is_Null(Str1)) then
         Put_Line("str1 was not null");
         raise Constraint_Error;
      end if;

      Allocate(Str1, "Goodbye");
      if (Get(Str1) /= "Goodbye") then
         Put_Line("str1 didn't return 'Goodbye', returned '"
                  & Get(Str1) & "' instead");
         raise Constraint_Error;
      end if;

      if (Is_Null(Str1)) then
         Put_Line("str1 was null");
         raise Constraint_Error;
      end if;

      Duplicate(Str2, Str1);
      if (Get(Str2) /= "Goodbye") then
         Put_Line("str1 didn't return 'Goodbye', returned '"
             & Get(Str2) & "' instead");
         raise Constraint_Error;
      end if;

      Free(Str1);
      Free(Str2);

      if (not Is_Empty(My_Pool)) then
         Put_Line("Pool not empty (2)");
         raise Constraint_Error;
      end if;
   end;

   Put_Line("Tests passed");
end Test_Refcount_Ptr;
