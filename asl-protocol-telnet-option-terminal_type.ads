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
-- Negotiate the terminal type for a telnet session.  A server should set
-- the local "do" to true, a client should set the local "will" to true
-- and set the terminal type.  For the server, when the change handler
-- get the Remote_Params_Changed method, you can read the terminal
-- type.
package Asl.Protocol.Telnet.Option.Terminal_Type is

   type Telnet_Termtype_Option is new Telnet_Base_Option with private;
   type Telnet_Termtype_Option_Ptr is
      access all Telnet_Termtype_Option;
   type Telnet_Termtype_Option_Class is
      access all Telnet_Termtype_Option'Class;

   procedure New_Remote_Params(Option : in out Telnet_Termtype_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array);

   -- We override this because when we get the "Will" we send the request
   -- to the other end to request the terminal type.
   procedure Handle_Will(Option : in out Telnet_Termtype_Option;
                         P      : in out Telnet_Processor'Class);

   procedure Destroy(Option     : in out Telnet_Termtype_Option;
                     Option_Ptr : in out Telnet_Option_Class);

   function Supports_Params(Option : in Telnet_Termtype_Option) return Boolean;

   procedure Set_Terminal(Option   : in out Telnet_Termtype_Option;
                          Termtype : in String);

   function Get_Remote_Terminal(Option : in Telnet_Termtype_Option)
                                return String;

private

   type String_Ptr is access all String;

   protected type String_Holder is
      procedure Set(New_String : String);
      function Get return String;
      procedure Destroy;
   private
      Val : String_Ptr := new String'("");
   end String_Holder;

   type Telnet_Termtype_Option is new Telnet_Base_Option with record
      Local  : String_Holder;
      Remote : String_Holder;
   end record;

end Asl.Protocol.Telnet.Option.Terminal_Type;
