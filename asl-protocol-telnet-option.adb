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

package body Asl.Protocol.Telnet.Option is

   procedure Free is new Unchecked_Deallocation(Telnet_Base_Option,
                                                Telnet_Base_Option_Ptr);

   procedure Initialize(Option : in out Telnet_Base_Option) is
   begin
      null;
   end Initialize;

   procedure Registered(Option : in out Telnet_Base_Option;
                        P      : in Telnet_Processor_Class) is
   begin
      if (Option.Local_Do = Do_Option) then
         Send_Command(P.all,
                      Option.Local_Do,
                      Option.Option_Code);
         Option.Waiting_Will := True;
      end if;
      if (Option.Local_Will = Will_Option) then
         Send_Command(P.all,
                      Option.Local_Will,
                      Option.Option_Code);
         Option.Waiting_Do := True;
      end if;
   end Registered;

   procedure Handle_Will(Option : in out Telnet_Base_Option;
                         P      : in out Telnet_Processor'Class) is
   begin
      Option.Remote_Will := True;
      if (Option.Waiting_Will) then
         Option.Waiting_Will := False;
         if (Option.Change_Handler /= null) then
            Remote_Will_Changed(Option.Change_Handler.all,
                                Option,
                                P,
                                True);
         end if;
      elsif (Option.Local_Do = Do_Option) then
         if (Option.Change_Handler /= null) then
            Remote_Will_Changed(Option.Change_Handler.all,
                                Option,
                                P,
                                True);
         end if;
         Send_Command(P, Do_Option, Option.Option_Code);
      else
         Send_Command(P, Dont_Option, Option.Option_Code);
      end if;
   end Handle_Will;

   procedure Handle_Wont(Option : in out Telnet_Base_Option;
                         P      : in out Telnet_Processor'Class) is
   begin
      Option.Remote_Will := False;
      if (Option.Waiting_Will) then
         Option.Waiting_Will := False;
         if (Option.Change_Handler /= null) then
            Remote_Will_Changed(Option.Change_Handler.all,
                                Option,
                                P,
                                True);
         end if;
      else
         Send_Command(P, Dont_Option, Option.Option_Code);
      end if;
   end Handle_Wont;

   procedure Handle_Do(Option : in out Telnet_Base_Option;
                       P      : in out Telnet_Processor'Class) is
   begin
      Option.Remote_Do := True;
      if (Option.Waiting_Do) then
         Option.Waiting_Do := False;
         if (Option.Change_Handler /= null) then
            Remote_Do_Changed(Option.Change_Handler.all,
                              Option,
                              P,
                              True);
         end if;
      elsif (Option.Local_Will = Will_Option) then
         if (Option.Change_Handler /= null) then
            Remote_Do_Changed(Option.Change_Handler.all,
                              Option,
                              P,
                              True);
         end if;
         Send_Command(P, Will_Option, Option.Option_Code);
      else
         Send_Command(P, Wont_Option, Option.Option_Code);
      end if;
   end Handle_Do;

   procedure Handle_Dont(Option : in out Telnet_Base_Option;
                         P      : in out Telnet_Processor'Class) is
   begin
      Option.Remote_Do := False;
      if (Option.Waiting_Do) then
         Option.Waiting_Do := False;
         if (Option.Change_Handler /= null) then
            Remote_Do_Changed(Option.Change_Handler.all,
                              Option,
                              P,
                              True);
         end if;
      else
         Send_Command(P, Wont_Option, Option.Option_Code);
      end if;
   end Handle_Dont;

   procedure New_Remote_Params(Option : in out Telnet_Base_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array) is
   begin
      if (Option.Change_Handler /= null) then
         Remote_Params_Changed(Option.Change_Handler.all,
                               Option,
                               P,
                               Get_Remote_Param_Array(Option),
                               Params);
      end if;
      Set_Remote_Param_Array(Option, Params);
   end New_Remote_Params;

   function Remote_Will(Option : in Telnet_Base_Option) return Boolean is
   begin
      return Option.Remote_Will;
   end Remote_Will;

   function Remote_Do(Option : in Telnet_Base_Option) return Boolean is
   begin
      return Option.Remote_Do;
   end Remote_Do;

   procedure Set_Local_Will(Option : in out Telnet_Base_Option;
                            Val    : in Boolean) is
   begin
      if (Val) then
         Option.Local_Will := Will_Option;
      else
         Option.Local_Will := Wont_Option;
      end if;

      if (Option.Registered) then
         Send_Command(Option.Processor.all,
                      Option.Local_Will,
                      Option.Option_Code);
         Option.Waiting_Do := True;
      end if;
   end Set_Local_Will;

   procedure Set_Local_Do(Option : in out Telnet_Base_Option;
                          Val    : in Boolean) is
   begin
      if (Val) then
         Option.Local_Do := Do_Option;
      else
         Option.Local_Do := Dont_Option;
      end if;

      if (Option.Registered) then
         Send_Command(Option.Processor.all,
                      Option.Local_Do,
                      Option.Option_Code);
         Option.Waiting_Will := True;
      end if;
   end Set_Local_Do;

   function Local_Will(Option : in Telnet_Base_Option) return Boolean is
   begin
      return Option.Local_Will = Will_Option;
   end Local_Will;

   function Local_Do(Option : in Telnet_Base_Option) return Boolean is
   begin
      return Option.Local_Do = Do_Option;
   end Local_Do;

   procedure Set_Change_Handler
     (Option  : in out Telnet_Base_Option;
      Handler : in Telnet_Option_Change_Handler_Class) is
   begin
      Option.Change_Handler := Handler;
   end Set_Change_Handler;

   function Supports_Params(Option : in Telnet_Base_Option) return Boolean is
   begin
      return False;
   end Supports_Params;

   function Will_Active(Option : in Telnet_Base_Option) return Boolean is
   begin
      return ((Option.Local_Will = Will_Option) and (Option.Remote_Do));
   end Will_Active;

   function Do_Active(Option : in Telnet_Base_Option) return Boolean is
   begin
      return ((Option.Local_Do = Do_Option) and (Option.Remote_Will));
   end Do_Active;

   procedure Destroy(Option     : in out Telnet_Base_Option;
                     Option_Ptr : in out Telnet_Option_Class) is
   begin
      -- Nothing is currently needed to clean up the option.
      if (Option_Ptr /= null) then
         Free(Telnet_Base_Option_Ptr(Option_Ptr));
      end if;
      Destroy(Telnet_Option(Option), Option_Ptr);
   end;

   procedure Remote_Do_Changed(H      : in out Telnet_Option_Change_Handler;
                               Option : in out Telnet_Base_Option'Class;
                               P      : in out Telnet_Processor'Class;
                               Val    : in Boolean) is
   begin
      null;
   end Remote_Do_Changed;

   procedure Remote_Will_Changed(H      : in out Telnet_Option_Change_Handler;
                                 Option : in out Telnet_Base_Option'Class;
                                 P      : in out Telnet_Processor'Class;
                                 Val    : in Boolean) is
   begin
      null;
   end Remote_Will_Changed;

   procedure Remote_Params_Changed
     (H      : in out Telnet_Option_Change_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array) is
   begin
      null;
   end Remote_Params_Changed;

end Asl.Protocol.Telnet.Option;
