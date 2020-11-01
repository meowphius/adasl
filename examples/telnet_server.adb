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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;

with Sockets; use Sockets;
with Sockets.Stream_IO; use Sockets.Stream_IO;

with Asl.Protocol.Telnet; use Asl.Protocol.Telnet;
with Asl.Protocol.Telnet.Stream_IO; use Asl.Protocol.Telnet.Stream_IO;
with Asl.Protocol.Telnet.Option; use Asl.Protocol.Telnet.Option;
with Asl.Protocol.Telnet.Option.Terminal_Type;
use Asl.Protocol.Telnet.Option.Terminal_Type;
with Asl.Protocol.Telnet.Option.Window_Size;
use Asl.Protocol.Telnet.Option.Window_Size;
with Asl.Protocol.Telnet.Option.Status; use Asl.Protocol.Telnet.Option.Status;
with Telnet_Server_Handlers; use Telnet_Server_Handlers;

procedure Telnet_Server is

   package ElementOff_IO is new Integer_IO(Ada.Streams.Stream_Element_Offset);
   use ElementOff_IO;
   package Element_IO is new Modular_IO(Ada.Streams.Stream_Element);
   use Element_IO;

   Accepting_Socket : Socket_FD;
   Data_Socket      : Socket_FD;
   Cmdproc          : Handle_Command_Class := new Handle_Command;

   task type Processor is
      entry Start(S : Socket_FD);
   end Processor;

   task body Processor is
      Socket        : Socket_FD;
      Sock_Stream   : Socket_Stream_Ptr;
      Telnet_Stream : Telnet_Stream_Type;
      Telnet_Proc   : Telnet_Processor_Class;
      Data          : Ada.Streams.Stream_Element_Array(1 .. 100);
      Last          : Ada.Streams.Stream_Element_Offset;
      S             : String(1 .. 100);
      Echo          : Telnet_Base_Option_Class := new Telnet_Base_Option;
      Go_Ahead      : Telnet_Base_Option_Class := new Telnet_Base_Option;
      Termtype      : Telnet_Termtype_Option_Class
        := new Telnet_Termtype_Option;
      Winsize       : Telnet_Winsize_Option_Class
        := new Telnet_Winsize_Option;
      Term_Hndl     : Termtype_Handler_Class := new Termtype_Handler;
      Win_Hndl      : Winsize_Handler_Class := new Winsize_Handler;
      Status_Hndl   : Telnet_Status_Option_Class := new Telnet_Status_Option;
   begin
      select
         accept Start(S : Socket_FD) do
            Socket := S;
         end Start;
      or
         terminate;
      end select;

      Sock_Stream := new Socket_Stream_Type;
      Initialize(Sock_Stream.all, Socket, False);
      Initialize(Telnet_Stream, Substream_Access(Sock_Stream),
                 Telnet_Command_Handler_Class(Cmdproc));

      Telnet_Proc := Get_Telnet_Processor(Telnet_Stream);
      Set_Local_Will(Echo.all, True);
      Set_Local_Will(Go_Ahead.all, True);
      Set_Local_Do(Termtype.all, True);
      Set_Local_Do(Winsize.all, True);
      Set_Local_Do(Status_Hndl.all, True);
      Set_Local_Will(Status_Hndl.all, True);
      Register_Option(Telnet_Proc.all, Echo_Option,
                      Telnet_Option_Class(Echo));
      Register_Option(Telnet_Proc.all, Suppress_Go_Ahead_Option,
                      Telnet_Option_Class(Go_Ahead));
      Register_Option(Telnet_Proc.all, Terminal_Type_Option,
                      Telnet_Option_Class(Termtype));
      Set_Change_Handler(Telnet_Base_Option(Termtype.all),
                         Telnet_Option_Change_Handler_Class(Term_Hndl));
      Register_Option(Telnet_Proc.all, Window_Size_Option,
                      Telnet_Option_Class(Winsize));
      Set_Change_Handler(Telnet_Base_Option(Winsize.all),
                         Telnet_Option_Change_Handler_Class(Win_Hndl));
      Register_Option(Telnet_Proc.all, Status_Option,
                      Telnet_Option_Class(Status_Hndl));

      loop
         Read(Telnet_Stream, Data, Last);
         for I in Data'First .. Last loop
            S(Integer(I)) := Character'Val(Data(I));
         end loop;
         Put(S(1 .. Integer(Last)));
         Write(Telnet_Stream, Data(1 .. Last));
      end loop;
   exception
      when Connection_Closed =>
         Put_Line("Connection closed");
         Destroy(Telnet_Stream);
         Shutdown(Socket);
   end Processor;

   type Processor_Ptr is access all Processor;

   Tmp_Proc : Processor_Ptr;

begin
   if (Argument_Count /= 1) then
      Put_Line("No port identified");
      return;
   end if;

   Socket(Accepting_Socket, AF_INET, SOCK_STREAM);
   Setsockopt(Accepting_Socket, SOL_SOCKET, SO_REUSEADDR, 0);
   Bind(Accepting_Socket, Positive'Value(Argument(1)));
   Listen(Accepting_Socket);
   loop
      Put_Line("Waiting for connection");
      Accept_Socket(Accepting_Socket, Data_Socket);
      Put_Line("Got connection");
      Tmp_Proc := new Processor;
      Tmp_Proc.Start(Data_Socket);
   end loop;
end Telnet_Server;
