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

with Unchecked_Deallocation;

package body Asl.Debug_Out.Command is

   type String_Ptr is access all String;

   procedure Free is new Unchecked_Deallocation(String, String_Ptr);

   procedure Initialize(Cmd   : in out Level_Command;
                        Debug : in Debug_Processor_Class) is
   begin
      Cmd.Debug := Debug;
   end Initialize;

   type Commands is (On, Off, List);

   procedure Execute(Cmd  : in out Level_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class) is
      Tok_Size : Integer := Current_Token_Length(Tok);
      Command  : Commands;
   begin
      if (Tok_Size = 0) then
         Put_Line(IO, "No subcommand specified");
         return;
      end if;

      declare
         Subcmd : String := Current_Token(Tok);
      begin
         if (Subcmd = "on") then
            Command := On;
         elsif (Subcmd = "off") then
            Command := Off;
         elsif (Subcmd = "list") then
            Command := List;
         else
            Put_Line(IO, "Invalid subcommand");
            return;
         end if;
      end;

      Move_To_Next_Token(Tok);

      Tok_Size := Current_Token_Length(Tok);
      if (Tok_Size = 0) then
         Put_Line(IO, "No level specified");
         return;
      end if;

      declare
         Level     : aliased String := Current_Token(Tok);
         Level_Val : Debug_Level_Class;
      begin
         case Command is
            when On =>
               Enable_Debug_Level(Cmd.Debug.all, Level'Unchecked_Access);

            when Off =>
               Disable_Debug_Level(Cmd.Debug.all, Level'Unchecked_Access);

            when List =>
               Get_Debug_Level(Cmd.Debug.all, Level'Unchecked_Access,
                               Level_Val);
               if (Level_Val = null) then
                  raise Invalid_Debug_Level;
               end if;
               if (Level_Val.Enabled) then
                  Put_Line(IO, "Level '" & Level & "' is enabled");
               else
                  Put_Line(IO, "Level '" & Level & "' is disabled");
               end if;
         end case;
      exception
         when Invalid_Debug_Level =>
            Put_Line(IO, "Debug level was not valid");
      end;
   end Execute;

   procedure Help(Cmd  : in out Level_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class) is
   begin
      Put_Wrapped
        (IO,
         Name & " on <debug level> - Turn on the given debug level");
      New_Line(IO);
      Put_Wrapped
        (IO,
         Name & " off <debug level> - Turn off the given debug level");
      New_Line(IO);
      Put_Wrapped
        (IO,
         Name & " list <debug level> - List the state of the debug level");
      New_Line(IO);
   end Help;

end Asl.Debug_Out.Command;
