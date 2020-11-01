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
use type Ada.Streams.Stream_Element;
with Unchecked_Deallocation;

package body Asl.Protocol.Telnet.Option.Window_Size is

   procedure Free is new Unchecked_Deallocation(Telnet_Winsize_Option,
                                                Telnet_Winsize_Option_Ptr);

   procedure Send_Winsize(Option : in Telnet_Winsize_Option;
                          P      : in out Telnet_Processor'Class) is
      Params : Ada.Streams.Stream_Element_Array(1 .. 4);
      Win_Size : Window;
   begin
      Win_Size := Option.Local.Get;
      Params(1) := Ada.Streams.Stream_Element((Win_Size.Width / 256) mod 256);
      Params(2) := Ada.Streams.Stream_Element(Win_Size.Width mod 256);
      Params(3) := Ada.Streams.Stream_Element((Win_Size.Height / 256) mod 256);
      Params(4) := Ada.Streams.Stream_Element(Win_Size.Height mod 256);
      Send_Params(P, Option, Params);
   end Send_Winsize;

   procedure New_Remote_Params(Option : in out Telnet_Winsize_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array) is
      Width, Height : Window_Measure;
   begin
      Width := ((Window_Measure(Params(1)) * 256)
                + Window_Measure(Params(2)));
      Height := ((Window_Measure(Params(3)) * 256)
                 + Window_Measure(Params(4)));
      Option.Remote.Set(Width, Height);
      New_Remote_Params(Telnet_Base_Option(Option), P, Params);
   end New_Remote_Params;

   -- Used to send the terminal type request.
   procedure Handle_Do(Option : in out Telnet_Winsize_Option;
                       P      : in out Telnet_Processor'Class) is
   begin
      Handle_Do(Telnet_Base_Option(Option), P);
      if Will_Active(Option) then
         Send_Winsize(Option, P);
      end if;
   end Handle_Do;

   procedure Set_Winsize(Option : in out Telnet_Winsize_Option;
                         Width  : in Window_Measure;
                         Height : in Window_Measure) is
   begin
      Option.Local.Set(Width, Height);
      if Will_Active(Option) then
         Send_Winsize(Option, Get_Processor(Option).all);
      end if;
   end Set_Winsize;

   procedure Get_Remote_Winsize(Option : in Telnet_Winsize_Option;
                                Width  : out Window_Measure;
                                Height : out Window_Measure) is
      Win_Size : Window;
   begin
      Win_Size := Option.Remote.Get;
      Width := Win_Size.Width;
      Height := Win_Size.Height;
   end Get_Remote_Winsize;

   procedure Destroy(Option     : in out Telnet_Winsize_Option;
                     Option_Ptr : in out Telnet_Option_Class) is
      Ptr : Telnet_Winsize_Option_Ptr := Telnet_Winsize_Option_Ptr(Option_Ptr);
   begin
      Option_Ptr := null;
      Destroy(Telnet_Base_Option(Option), Option_Ptr);
      if (Ptr /= null) then
         Free(Ptr);
      end if;
   end Destroy;

   function Supports_Params(Option : in Telnet_Winsize_Option) return Boolean
   is
   begin
      return Will_Active(Option);
   end Supports_Params;

   protected body Winsize is
      procedure Set(New_Width, New_Height : in Window_Measure) is
      begin
         Width := New_Width;
         Height := New_Height;
      end Set;

      function Get return Window is
      begin
         return (Width, Height);
      end Get;
   end Winsize;

end Asl.Protocol.Telnet.Option.Window_Size;
