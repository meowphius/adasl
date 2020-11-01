-- The Ada telnet processor - A set of packages for dealing with telnet
--   processing under Ada95
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Asl.Protocol.Telnet.Option.Terminal_Type;
use Asl.Protocol.Telnet.Option.Terminal_Type;
with Asl.Protocol.Telnet.Option.Window_Size;
use Asl.Protocol.Telnet.Option.Window_Size;
with Ada.Streams;
use type Ada.Streams.Stream_Element;

package body Telnet_Server_Handlers is

   procedure Remote_Params_Changed
     (H      : in out Termtype_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array) is
   begin
      Put_Line("Terminal type is "
               & Get_Remote_Terminal(Telnet_Termtype_Option(Option)));
   end Remote_Params_Changed;

   procedure Remote_Params_Changed
     (H      : in out Winsize_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array)
   is
      Width  : Window_Measure;
      Height : Window_Measure;
   begin
      Get_Remote_Winsize(Telnet_Winsize_Option(Option),
                         Width, Height);
      Put("Window size is ");
      Put(Width);
      Put(" by ");
      Put(Height);
      New_Line;
   end Remote_Params_Changed;

   procedure Handle_Telnet_Command
     (H   : in out Handle_Command;
      P   : in out Telnet_Processor'Class;
      Cmd : in Ada.Streams.Stream_Element_Array) is
   begin
      Put_Line("Got command");
      if (Cmd(1) = Asl.Protocol.Telnet.Break) then
         Put_Line("command was break");
      end if;
   end Handle_Telnet_Command;

end Telnet_Server_Handlers;
