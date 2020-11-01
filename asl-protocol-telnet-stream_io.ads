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


with Ada.Streams;

package Asl.Protocol.Telnet.Stream_IO is

   -- A stream type that implements the telnet protocol over another
   -- stream.
   type Telnet_Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   type Substream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   procedure Initialize
     (Stream    : in out Telnet_Stream_Type;
      Substream : in Substream_Access;
      Handler   : in Telnet.Telnet_Command_Handler_Class);

   -- This will destroy the underlying telnet stream and free it.
   procedure Destroy(Stream : in out Telnet_Stream_Type);

   function Get_Telnet_Processor (Stream : in Telnet_Stream_Type)
                                  return Telnet.Telnet_Processor_Class;

   procedure Read
     (Stream : in out Telnet_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Telnet_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

private

   protected type Mutex is
      procedure Write(Stream : in out Telnet_Stream_Type'Class;
                      Item   : in Ada.Streams.Stream_Element_Array);
   end Mutex;

   type Telnet_Stream_Type_Ptr is access all Telnet_Stream_Type;

   type Stream_Output_Handler is new Telnet.Telnet_Output_Handler with record
      Substream : Substream_Access;
   end record;
   type Stream_Output_Handler_Ptr is access all Stream_Output_Handler;

   procedure Telnet_Output
     (H      : in out Stream_Output_Handler;
      P      : in out Telnet.Telnet_Processor'Class;
      Data   : in Ada.Streams.Stream_Element_Array);

   type Telnet_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Processor : Telnet.Telnet_Processor_Ptr;
      Substream : Substream_Access;
      Output    : Stream_Output_Handler_Ptr;

      -- Output is required to be atomic by the telnet processor, so we
      -- use a mutex to make sure it is atomic.
      Out_Mutex : Mutex;
   end record;

end Asl.Protocol.Telnet.Stream_IO;
