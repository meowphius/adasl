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

with Asl.Cmdproc;
with Asl.Debug_Out;

-- A full telnet implementation of the command processor.  Just
-- instantiate one of these babies, provide the necessary information,
-- and it hands you back a command table that you can fill in with
-- your commands.

package Asl.Cmdproc.Telnet is

   task type Telnet_Cmd_Listener_Task is
      -- Start the listener.  It will open the given login port and wait for
      -- command connection, and the given debug port and wait for debug
      -- connections.  At connection time, the login/debug text is printed
      -- and if the Sec paramter is non-null, the specific security
      -- class is executed.
      --
      -- This class return the command table for the user to add command,
      -- and the debug processor for registering debug levels.
      entry Start(Login_Text : in String;
                  Debug_Text : in String;
                  Login_Port : in Integer;
                  Debug_Port : in Integer;
                  Login_Sec  : in Asl.Security.Verify_User_Class;
                  Debug_Sec  : in Asl.Security.Verify_User_Class;
                  Cmd_Table  : out Asl.Cmdproc.Command_Table_Ptr;
                  Debug_Proc : out Asl.Debug_Out.Debug_Processor_Class);
   end Telnet_Cmd_Listener_Task;

end Asl.Cmdproc.Telnet;
