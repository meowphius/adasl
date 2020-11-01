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

with Asl.Abstract_IO;

package Asl.Security.Userpass is

   type Verify_User_Userpass is abstract new Verify_User with null record;
   type Verify_User_Userpass_Class is
      access all Verify_User_Userpass'Class;

   procedure Verify_Access(Verify   : in out Verify_User_Userpass;
                           User     : in String;
                           Password : in String;
                           Permit   : out Boolean)
      is abstract;

   function Prompt_For_User(Verify : in Verify_User_Userpass)
                            return Boolean;

   function Prompt_For_Password(Verify : in Verify_User_Userpass)
                                return Boolean;

   procedure Verify_Access(Verify : in out Verify_User_Userpass;
                           IO     : in out Asl.Abstract_IO.Abstract_File'Class;
                           Permit : out Boolean);

end Asl.Security.Userpass;
