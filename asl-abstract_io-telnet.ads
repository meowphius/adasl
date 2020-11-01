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

with Sockets; use Sockets;
with Sockets.Stream_IO; use Sockets.Stream_IO;
with Asl.Protocol.Telnet; use Asl.Protocol.Telnet;
with Asl.Protocol.Telnet.Stream_IO; use Asl.Protocol.Telnet.Stream_IO;
with Asl.Protocol.Telnet.Option; use Asl.Protocol.Telnet.Option;
with Asl.Protocol.Telnet.Option.Terminal_Type;
use Asl.Protocol.Telnet.Option.Terminal_Type;
with Asl.Protocol.Telnet.Option.Window_Size;
use Asl.Protocol.Telnet.Option.Window_Size;

-- This package provides abstract I/O on top of the telnet processor.

package Asl.Abstract_IO.Telnet is

   type Telnet_Abstract_File is new Abstract_File with private;
   type Telnet_Abstract_File_Ptr is access all Telnet_Abstract_File;
   type Telnet_Abstract_File_Class is access all Telnet_Abstract_File'Class;

   -- Create the telnet I/O from a telnet socket.
   procedure Initialize(File   : access Telnet_Abstract_File;
                        Socket : in Socket_FD);


   -- The rest are implementations of the abstract versions of
   -- Abstract_File.

   procedure Put(File : in out Telnet_Abstract_File;
                 Item : in Character);

   procedure Put(File : in out Telnet_Abstract_File;
                 Item : in String);

   procedure Put_Line(File : in out Telnet_Abstract_File;
                      Item : in String);

   procedure New_Line(File : in out Telnet_Abstract_File);


   procedure Get(File : in out Telnet_Abstract_File;
                 Item : out Character);

   procedure Get(File : in out Telnet_Abstract_File;
                 Item : out String);

   procedure Get_Line(File : in out Telnet_Abstract_File;
                      Item : out String;
                      Last : out Natural);

   function End_Of_Line(File : in Telnet_Abstract_File;
                        Ch   : in Character)
                        return Boolean;

   procedure Putback_Char(File : in out Telnet_Abstract_File;
                          Item : in Character);


   function Next_Column(File : in Telnet_Abstract_File)
                        return Positive;
   function IO_Width(File : in Telnet_Abstract_File)
                     return Positive;
   function IO_Height(File : in Telnet_Abstract_File)
                      return Positive;

private

   type Telnet_Abstract_File is new Abstract_File with record
      Socket        : Socket_FD;
      Sock_Stream   : aliased Socket_Stream_Type;
      Telnet_Stream : Telnet_Stream_Type;
      Telnet_Proc   : Telnet_Processor_Class;

      Putback_Valid : Boolean := False;
      Putback       : Character;

      Column        : Integer := 1;

      Echo          : Telnet_Base_Option_Class := new Telnet_Base_Option;
      Go_Ahead      : Telnet_Base_Option_Class := new Telnet_Base_Option;
      Termtype      : Telnet_Termtype_Option_Class
        := new Telnet_Termtype_Option;
      Winsize       : Telnet_Winsize_Option_Class
        := new Telnet_Winsize_Option;
   end record;

end Asl.Abstract_IO.Telnet;
