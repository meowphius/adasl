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

------------------------------------------------------------------------
-- Negotiate the window size for a telnet session.  A server should set
-- the local "do" to true, a client should set the local "will" to true
-- and set the window size.  For the server, when the change handler for
-- this calls the Remote_Params_Changed method, you can read the window
-- size.
package Asl.Protocol.Telnet.Option.Window_Size is

   type Telnet_Winsize_Option is new Telnet_Base_Option with private;
   type Telnet_Winsize_Option_Ptr is access all Telnet_Winsize_Option;
   type Telnet_Winsize_Option_Class is access all Telnet_Winsize_Option'Class;

   subtype Window_Measure is Integer range 0 .. 65535;

   procedure New_Remote_Params(Option : in out Telnet_Winsize_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array);

   -- Used to report the first window size.
   procedure Handle_Do(Option : in out Telnet_Winsize_Option;
                       P      : in out Telnet_Processor'Class);

   function Supports_Params(Option : in Telnet_Winsize_Option) return Boolean;

   procedure Destroy(Option     : in out Telnet_Winsize_Option;
                     Option_Ptr : in out Telnet_Option_Class);

   procedure Set_Winsize(Option : in out Telnet_Winsize_Option;
                         Width  : in Window_Measure;
                         Height : in Window_Measure);

   procedure Get_Remote_Winsize(Option : in Telnet_Winsize_Option;
                                Width  : out Window_Measure;
                                Height : out Window_Measure);

private

   type Window is record
      Width, Height : Window_Measure;
   end record;

   protected type Winsize is
      procedure Set(New_Width, New_Height : in Window_Measure);
      function Get return Window;
   private
      Width  : Window_Measure := 0;
      Height : Window_Measure := 0;
   end Winsize;

   type Telnet_Winsize_Option is new Telnet_Base_Option with record
      Remote : Winsize;
      Local  : Winsize;
   end record;

end Asl.Protocol.Telnet.Option.Window_Size;
