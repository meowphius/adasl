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

-- Implements a special telnet status handler, which sends the current
-- status of this side to the remote end.
package body Asl.Protocol.Telnet.Option.Status is

   procedure Free is
     new Ada.Unchecked_Deallocation(Ada.Streams.Stream_Element_Array,
                                    Telnet_Params_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation(Telnet_Status_Option,
                                                    Telnet_Status_Option_Ptr);

   procedure Append_Param(Params : in out Telnet_Params_Ptr;
                          Length : in out Ada.Streams.Stream_Element_Count;
                          Param  : in Ada.Streams.Stream_Element)
   is
      New_Params : Telnet_Params_Ptr;
   begin
      -- Create or extend the array if necessary.
      if (Params = null) then
         Length := 0;
         Params := new Ada.Streams.Stream_Element_Array(1 .. 16);
      elsif (Params'Length = Length) then
         New_Params
           := new Ada.Streams.Stream_Element_Array(1 .. Params'Last + 16);
         New_Params.all(1 .. Params'Last) := Params.all;
         Free(Params);
         Params := New_Params;
      end if;

      Length := Length + 1;
      Params.all(Length) := Param;
   end Append_Param;

   procedure Processed_Param(Params : in out Telnet_Params_Ptr;
                             Length : in out Ada.Streams.Stream_Element_Count;
                             Param  : in Ada.Streams.Stream_Element) is
   begin
      Append_Param(Params, Length, Param);
      if (Param = SE) then
         Append_Param(Params, Length, Param);
      end if;
   end Processed_Param;

   procedure Send_Param_Info(P      : in out Telnet_Processor'Class;
                             Option : in Telnet_Status_Option) is
      Options : Telnet_Option_Class := P.Options;
      Params  : Telnet_Params_Ptr := null;
      Last    : Ada.Streams.Stream_Element_Count := 0;
   begin
      -- Append the "is".
      Append_Param(Params, Last, 0);
      while (Options /= null) loop
         if (Will_Active(Options.all)) then
            Append_Param(Params, Last, Will_Option);
         else
            Append_Param(Params, Last, Wont_Option);
         end if;
         Append_Param(Params, Last, Options.Option_Code);

         if (Do_Active(Options.all)) then
            Append_Param(Params, Last, Do_Option);
         else
            Append_Param(Params, Last, Dont_Option);
         end if;
         Append_Param(Params, Last, Options.Option_Code);

         if (Supports_Params(Options.all)) then
            Append_Param(Params, Last, SB);
            declare
               Tparams : Ada.Streams.Stream_Element_Array
                 := Get_Local_Param_Array(Options.all);
            begin
               for I in Tparams'Range loop
                  Processed_Param(Params, Last, Tparams(I));
               end loop;
            end;
            Append_Param(Params, Last, SE);
         end if;

         Options := Options.Next;
      end loop;
      if (Last /= 0) then
         Send_Params(P,
                     Telnet_Option'Class(Option),
                     Params.all(1 .. Last));
      end if;
   end Send_Param_Info;

   procedure New_Remote_Params(Option : in out Telnet_Status_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array) is
   begin
      if (Params(1) = 1) then
         -- This is a send request, send the response.
         Send_Param_Info(P, Option);
      elsif (Params(1) = 0) then
         -- This is a response, just store it for now and report it.
         New_Remote_Params(Telnet_Base_Option(Option), P, Params);
      end if;
   end New_Remote_Params;

   procedure Request_Status(Option : in out Telnet_Status_Option)
   is
      P : Telnet_Processor_Class := Get_Processor(Option);
   begin
      Send_Params(P.all, Telnet_Option'Class(Option), (1 => 1));
   end Request_Status;

   procedure Destroy(Option     : in out Telnet_Status_Option;
                     Option_Ptr : in out Telnet_Option_Class) is
      Ptr : Telnet_Status_Option_Ptr := Telnet_Status_Option_Ptr(Option_Ptr);
   begin
      Option_Ptr := null;
      Destroy(Telnet_Base_Option(Option), Option_Ptr);
      if (Ptr /= null) then
         Free(Ptr);
      end if;
   end Destroy;

end Asl.Protocol.Telnet.Option.Status;
