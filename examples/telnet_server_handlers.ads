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

with Sockets.Stream_IO; use Sockets.Stream_IO;
with Asl.Protocol.Telnet; use Asl.Protocol.Telnet;
with Asl.Protocol.Telnet.Option; use Asl.Protocol.Telnet.Option;
with Ada.Streams;
with Sockets; use Sockets;

package Telnet_Server_Handlers is

   type Socket_Stream_Ptr is access all Socket_Stream_Type;

   type Termtype_Handler is new Telnet_Option_Change_Handler with null record;
   type Termtype_Handler_Class is access all Termtype_Handler'Class;

   procedure Remote_Params_Changed
     (H      : in out Termtype_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array);

   type Winsize_Handler is new Telnet_Option_Change_Handler with null record;
   type Winsize_Handler_Class is access all Winsize_Handler'Class;

   procedure Remote_Params_Changed
     (H      : in out Winsize_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array);

   type Handle_Command is new Telnet_Command_Handler with null record;
   type Handle_Command_Class is access all Handle_Command'Class;

   procedure Handle_Telnet_Command
     (H   : in out Handle_Command;
      P   : in out Telnet_Processor'Class;
      Cmd : in Ada.Streams.Stream_Element_Array);

end Telnet_Server_Handlers;
