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

with Ada.Streams; use Ada.Streams;
with Unchecked_Deallocation;

with Ada.Text_IO; use Ada.Text_IO;

package body Asl.Protocol.Telnet.Stream_IO is

   procedure Free is new Unchecked_Deallocation(Telnet_Processor,
                                                Telnet_Processor_Ptr);

   procedure Free is new Unchecked_Deallocation(Stream_Output_Handler,
                                                Stream_Output_Handler_Ptr);

   package ElementOff_IO is new Integer_IO(Stream_Element_Offset);
   use ElementOff_IO;
   package Element_IO is new Modular_IO(Stream_Element);
   use Element_IO;

   protected body Mutex is
      procedure Write(Stream : in out Telnet_Stream_Type'Class;
                      Item   : in Ada.Streams.Stream_Element_Array) is
         -- Might need up to twice as many output characters plus one.
         Tmp_Item : Stream_Element_Array(0 .. (Item'Length * 2));
         Tmp_Last : Stream_Element_Offset;
         Tmp_Next : Stream_Element_Offset;
      begin
         Telnet.Process_Output(Stream.Processor.all,
                               Item, Tmp_Item, Tmp_Next, Tmp_Last);
         Write(Stream.Substream.all, Tmp_Item(0 .. Tmp_Last));
      end Write;
   end Mutex;


   procedure Initialize
     (Stream    : in out Telnet_Stream_Type;
      Substream : in Substream_Access;
      Handler   : in Telnet.Telnet_Command_Handler_Class) is
   begin
      Stream.Substream := Substream;
      Stream.Output := new Stream_Output_Handler;
      Stream.Output.Substream := Substream;
      Stream.Processor := new Telnet.Telnet_Processor;
      Telnet.Initialize(Telnet_Processor_Class(Stream.Processor),
                        Handler,
                        Telnet_Output_Handler_Class(Stream.Output));
   end Initialize;

   procedure Destroy(Stream : in out Telnet_Stream_Type) is
   begin
      Telnet.Destroy(Stream.Processor.all);
      Free(Stream.Processor);
   end Destroy;

   function Get_Telnet_Processor (Stream : in Telnet_Stream_Type)
                                  return Telnet_Processor_Class is
   begin
      return Telnet.Telnet_Processor_Class(Stream.Processor);
   end Get_Telnet_Processor;

   procedure Read
     (Stream : in out Telnet_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Tmp_Item : Stream_Element_Array(Item'Range);
      Tmp_Last : Stream_Element_Offset;
      Tmp_Next : Stream_Element_Offset;
   begin
      Read(Stream.Substream.all, Tmp_Item, Tmp_Last);
      Telnet.Process_Input(Stream.Processor.all,
                           Tmp_Item(Item'First .. Tmp_Last),
                           Item,
                           Tmp_Next,
                           Last);
   end Read;

   procedure Write(Stream : in out Telnet_Stream_Type;
                   Item   : in Stream_Element_Array) is
   begin
      Stream.Out_Mutex.Write(Stream, Item);
   end Write;

   procedure Telnet_Output
     (H      : in out Stream_Output_Handler;
      P      : in out Telnet.Telnet_Processor'Class;
      Data   : in Stream_Element_Array) is
   begin
      Write(H.Substream.all, Data);
   end Telnet_Output;

end Asl.Protocol.Telnet.Stream_IO;
