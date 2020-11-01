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

with Asl.Abstract_IO; use Asl.Abstract_IO;

package body Asl.Security.Userpass is

   function Prompt_For_User(Verify : in Verify_User_Userpass)
                            return Boolean is
   begin
      return True;
   end Prompt_For_User;

   function Prompt_For_Password(Verify : in Verify_User_Userpass)
                                return Boolean is
   begin
      return True;
   end Prompt_For_Password;

   procedure Verify_Access(Verify : in out Verify_User_Userpass;
                           IO     : in out Asl.Abstract_IO.Abstract_File'Class;
                           Permit : out Boolean) is
      User     : String(1 .. 128);
      User_Pos : Integer := User'First;
      Pass     : String(1 .. 128);
      Pass_Pos : Integer := User'First;
      Ch       : Character;
   begin
      if Prompt_For_User(Verify_User_Userpass'Class(Verify)) then
         Put(IO, "Username: ");
         Get(IO, Ch);
         while not End_Of_Line(IO, Ch) loop
            Put(IO, Ch);
            exit when User_Pos > User'Last;
            User(User_Pos) := Ch;
            User_Pos := User_Pos + 1;
            Get(IO, Ch);
         end loop;
         New_Line(IO);
      end if;

      if Prompt_For_Password(Verify_User_Userpass'Class(Verify)) then
         Put(IO, "Password: ");
         Get(IO, Ch);
         while not End_Of_Line(IO, Ch) loop
            exit when Pass_Pos > User'Last;
            Pass(Pass_Pos) := Ch;
            Pass_Pos := Pass_Pos + 1;
            Get(IO, Ch);
         end loop;
         New_Line(IO);
      end if;

      Verify_Access(Verify_User_Userpass'Class(Verify),
                    User(1 .. User_Pos-1),
                    Pass(1 .. Pass_Pos-1),
                    Permit);

      -- Paranoia
      for I in Pass'Range loop
         Pass(I) := ' ';
      end loop;
      for I in User'Range loop
         User(I) := ' ';
      end loop;
   end Verify_Access;

end Asl.Security.Userpass;
