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
use type Ada.Streams.Stream_Element_Offset;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;

package body Asl.Protocol.Telnet.Option.Terminal_Type is

   procedure Free is
     new Ada.Unchecked_Deallocation(Telnet_Termtype_option,
                                    Telnet_Termtype_Option_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation(String, String_Ptr);

   procedure Send_Terminal(P      : in out Telnet_Processor'Class;
                           Option : in out Telnet_Termtype_Option) is
      Terminal : String := Option.Local.Get;
      Params : Ada.Streams.Stream_Element_Array(1 .. Terminal'Length + 1);
      J : Integer;
   begin
      Params(1) := 0;
      J := Terminal'First;
      for I in 2 .. Params'Last loop
         Params(I) := Character'Pos(Terminal(J));
         J := J + 1;
      end loop;
      Send_Params(P, Telnet_Option'Class(Option), Params);
   end Send_Terminal;

   procedure New_Remote_Params(Option : in out Telnet_Termtype_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array) is
      Terminal : String(1 .. Params'Length-1);
      J        : Integer;
   begin
      if (Params(1) = 1) then
         -- This is a send request, send the response.
         Send_Terminal(P, Option);
      elsif (Params(1) = 0) then
         -- This is a response, just store it for now and report it.  We want
         -- to calculate it first so it is available from the user's callback
         -- handler.
         J := Terminal'First;
         for I in 2 .. Params'Last loop
            Terminal(J) := Ada.Characters.Handling.To_Lower
              (Character'Val(Params(I)));
            J := J + 1;
         end loop;
         Option.Remote.Set(Terminal);
         New_Remote_Params(Telnet_Base_Option(Option), P, Params);
      end if;
   end New_Remote_Params;

   procedure Handle_Will(Option : in out Telnet_Termtype_Option;
                         P      : in out Telnet_Processor'Class) is
   begin
      Handle_Will(Telnet_Base_Option(Option), P);
      if Do_Active(Option) then
         -- If the remote side is ready, send a "send" request.
         Send_Params(P, Telnet_Option'Class(Option), (1 => 1));
      end if;
   end Handle_Will;

   procedure Set_Terminal(Option   : in out Telnet_Termtype_Option;
                          Termtype : in String) is
      Xlat_Termtype : String := Termtype;
   begin
      for I in Xlat_Termtype'Range loop
         Xlat_Termtype(I)
           := Ada.Characters.Handling.To_Upper(Xlat_Termtype(I));
      end loop;
      Option.Local.Set(Xlat_Termtype);
      if Will_Active(Option) then
         Send_Terminal(Get_Processor(Option).all, Option);
      end if;
   end Set_Terminal;

   function Get_Remote_Terminal(Option : in Telnet_Termtype_Option)
                                return String is
   begin
      return Option.Remote.Get;
   end Get_Remote_Terminal;

   procedure Destroy(Option     : in out Telnet_Termtype_Option;
                     Option_Ptr : in out Telnet_Option_Class) is
      Ptr : Telnet_Termtype_Option_Ptr
        := Telnet_Termtype_Option_Ptr(Option_Ptr);
   begin
      Option_Ptr := null;
      Option.Remote.Destroy;
      Option.Local.Destroy;
      Destroy(Telnet_Base_Option(Option), Option_Ptr);
      if (Ptr /= null) then
         Free(Ptr);
      end if;
   end Destroy;

   function Supports_Params(Option : in Telnet_Termtype_Option) return Boolean
   is
   begin
      return Will_Active(Option);
   end Supports_Params;

   protected body String_Holder is
      procedure Set(New_String : String) is
         Old : String_Ptr := Val;
      begin
         Val := new String'(New_String);
         Free(Old);
      end Set;

      function Get return String is
      begin
         return Val.all;
      end Get;

      procedure Destroy is
      begin
         Free(Val);
      end Destroy;
   end String_Holder;

end Asl.Protocol.Telnet.Option.Terminal_Type;
