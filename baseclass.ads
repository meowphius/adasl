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

with Ada.Finalization;
with Ada.Tags;

package Baseclass is

   type Object is abstract new Ada.Finalization.Controlled with private;
   type Object_Class is access all Object'Class;
   type Object_Ptr is access all Object;

   function Cast (To_Cast  : access Object;
                  New_Type : in Ada.Tags.Tag)
                  return Object_Class;

   function To_String (To_Convert : access Object)
                       return String;

   function Get_Object_Name (Get_Name_For : access Object)
                             return String;

   type Limited_Object is abstract new Ada.Finalization.Limited_Controlled
     with private;
   type Limited_Object_Class is access all Limited_Object'Class;
   type Limited_Object_Ptr is access all Limited_Object;

   function Cast (To_Cast  : access Limited_Object;
                  New_Type : in Ada.Tags.Tag)
                  return Limited_Object_Class;

   function To_String (To_Convert : access Limited_Object)
                       return String;

   function Get_Object_Name (Get_Name_For : access Limited_Object)
                             return String;

private

   type Object is abstract new Ada.Finalization.Controlled with null record;

   type Limited_Object is abstract new Ada.Finalization.Limited_Controlled
     with null record;

end Baseclass;
