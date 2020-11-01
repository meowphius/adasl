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

with Ada.Characters.Latin_1;
with Ada.Streams; use Ada.Streams;

package body Asl.Abstract_IO.Telnet is

   -- Newline is a linefeed.
   Newline_Marker : constant Character := Ada.Characters.Latin_1.CR;

   Out_Newline : constant String := (Ada.Characters.Latin_1.CR
                                     & Ada.Characters.Latin_1.LF);

   type Socket_Stream_Ptr is access all Socket_Stream_Type;

   procedure Initialize(File   : access Telnet_Abstract_File;
                        Socket : in Socket_FD) is
      Stream_Ptr : Socket_Stream_Ptr;
   begin
      Initialize(File.Sock_Stream, Socket);
      Stream_Ptr := File.Sock_Stream'Access;
      Initialize(File.Telnet_Stream, Substream_Access(Stream_Ptr),
                 null);
      File.Telnet_Proc := Get_Telnet_Processor(File.Telnet_Stream);
      Set_Local_Will(File.Echo.all, True);
      Set_Local_Will(File.Go_Ahead.all, True);
      Set_Local_Do(File.Termtype.all, True);
      Set_Local_Do(File.Winsize.all, True);
      Register_Option(File.Telnet_Proc.all, Echo_Option,
                      Telnet_Option_Class(File.Echo));
      Register_Option(File.Telnet_Proc.all, Suppress_Go_Ahead_Option,
                      Telnet_Option_Class(File.Go_Ahead));
      Register_Option(File.Telnet_Proc.all, Terminal_Type_Option,
                      Telnet_Option_Class(File.Termtype));
      Register_Option(File.Telnet_Proc.all, Window_Size_Option,
                      Telnet_Option_Class(File.Winsize));
   end Initialize;

   procedure Put(File : in out Telnet_Abstract_File;
                 Item : in Character) is
   begin
      Write(File.Telnet_Stream, (1 => Character'Pos(Item)));
      File.Column := File.Column + 1;
   end Put;

   procedure Put(File : in out Telnet_Abstract_File;
                 Item : in String) is
      Data : Stream_Element_Array(1 .. Stream_Element_Offset(Item'Length));
      Item_Pos : Integer := Item'First;
      Data_Pos : Stream_Element_Offset := Data'First;
   begin
      while (Item_Pos <= Item'Last) loop
         Data(Data_Pos) := Character'Pos(Item(Item_Pos));
         Data_Pos := Data_Pos + 1;
         if (Data_Pos > Data'Last) then
            Write(File.Telnet_Stream, Data);
            Data_Pos := Data'First;
         end if;
         Item_Pos := Item_Pos + 1;
      end loop;

      if (Data_Pos /= Data'First) then
         Write(File.Telnet_Stream, Data(1 .. Data_Pos-1));
      end if;
   end Put;

   procedure Put_Line(File : in out Telnet_Abstract_File;
                      Item : in String) is
   begin
      Put(File, Item);
      Put(File, Out_Newline);
      File.Column := 1;
   end Put_Line;

   procedure New_Line(File : in out Telnet_Abstract_File) is
   begin
      Put(File, Out_Newline);
      File.Column := 1;
   end New_Line;



   procedure Get(File : in out Telnet_Abstract_File;
                 Item : out Character) is
      Data : Stream_Element_Array(1 .. 1);
      Last : Stream_Element_Offset := 0;
   begin
      if (File.Putback_Valid) then
         File.Putback_Valid := False;
         Item := File.Putback;
      else
         while (Last = 0) loop
            Read(Root_Stream_Type'Class(File.Telnet_Stream), Data, Last);
         end loop;

         Item := Character'Val(Data(1));
      end if;
   end Get;

   procedure Get(File : in out Telnet_Abstract_File;
                 Item : out String) is
      Curr : Integer := Item'First;
   begin
      while (Curr <= Item'Last) loop
         Get(File, Item(Curr));
      end loop;
   end Get;

   procedure Get_Line(File : in out Telnet_Abstract_File;
                      Item : out String;
                      Last : out Natural) is
      Curr : Integer := Item'First;
      Ch   : Character;
   begin
      while (Curr <= Item'Last) loop
         Get(File, Ch);
         exit when Ch = Newline_Marker;
         Item(Curr) := Ch;
      end loop;
      Last := Curr - 1;
   end Get_Line;

   function End_Of_Line(File : in Telnet_Abstract_File;
                        Ch   : in Character)
                        return Boolean is
   begin
      return Ch = Newline_Marker;
   end End_Of_Line;

   procedure Putback_Char(File : in out Telnet_Abstract_File;
                          Item : in Character) is
   begin
      File.Putback := Item;
      File.Putback_Valid := True;
   end Putback_Char;

   function Next_Column(File : in Telnet_Abstract_File)
                        return Positive is
   begin
      return File.Column;
   end Next_Column;

   function IO_Width(File : in Telnet_Abstract_File)
                     return Positive is
      Width, Height : Integer;
   begin
      Get_Remote_Winsize(File.Winsize.all, Width, Height);
      if (Width /= 0) then
         return Width;
      else
         return 80;
      end if;
   end IO_Width;

   function IO_Height(File : in Telnet_Abstract_File)
                      return Positive is
      Width, Height : Integer;
   begin
      Get_Remote_Winsize(File.Winsize.all, Width, Height);
      if (Height /= 0) then
         return Height;
      else
         return 24;
      end if;
   end IO_Height;

end Asl.Abstract_IO.Telnet;
