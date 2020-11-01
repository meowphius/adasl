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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Asl.Cmdproc; use Asl.Cmdproc;
with Asl.Cmdproc.Telnet; use Asl.Cmdproc.Telnet;
with Telnet_Cmdproc_Handlers; use Telnet_Cmdproc_Handlers;
with Asl.Security; use Asl.Security;
with Asl.Debug_Out; use Asl.Debug_Out;

procedure Telnet_Cmdproc is
   Commands : Command_Table_Ptr;
   Listener : Telnet_Cmd_Listener_Task;
   Sec      : Dummy_Verify_User_Ptr := new Dummy_Verify_User;
   Debug    : Debug_Processor_Class;
   Level1   : aliased Debug_Level;
   Level1_P : Debug_Level_Ptr := Level1'Unchecked_Access;
   Genlog   : aliased Gen_Debug_Command;
   Genlog_P : Gen_Debug_Command_Ptr := Genlog'Unchecked_Access;
begin
   if (Argument_Count /= 1) then
      Put_Line("No port identified");
      return;
   end if;

   Listener.Start
     ("Use 'dummy' for the user and 'login' for the password.",
      "Debug port, Use 'dummy' for the user and 'login' for the password.",
      Positive'Value(Argument(1)),
      Positive'Value(Argument(1)) + 1,
      Verify_User_Class(Sec),
      Verify_User_Class(Sec),
      Commands,
      Debug);

   Register_Debug_Level(Debug.all, "level1", Level1'Unchecked_Access);
   Genlog.Level := Debug_Level_Class(Level1_P);
   Genlog.Proc := Debug;
   Register_Command(Commands.all, "genlog", Command_Class(Genlog_P));
end Telnet_Cmdproc;
