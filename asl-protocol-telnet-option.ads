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
-- This package defines a basic telnet option processor.  It can handle
-- basic telnet options that don't require any special processing, such
-- as binary (rfc856), echo (rfc857), suppress go ahead (rfc858),
-- and timing mark (rfc860).  More complex options can descend from it
-- and let this do the basic work of negotiation.  See
-- Telnet.Option.Terminal_Type for a basic example of how to do this.
--
-- To use an option processor, allocate one, then initialize it it,
-- set the local will, do, and parameters (if necessary) then register
-- it with the telnet processor.
--
-- Anything that descends from this MUST override the "Destroy"
-- method with their own that frees the proper type and passes a
-- "null" in as the Option_Ptr parameter to Destroy.
package Asl.Protocol.Telnet.Option is

   type Telnet_Base_Option is new Telnet_Option with private;
   type Telnet_Base_Option_Ptr is access all Telnet_Base_Option;
   type Telnet_Base_Option_Class is access all Telnet_Base_Option'Class;

   -- This procedure must be called for every option created.  It may
   -- be overriden by subclasses, but the subclasses must call this.
   procedure Initialize(Option : in out Telnet_Base_Option);

   -- If you override any of these, you need to call these, anyway, since
   -- the are used to track the internal state of the will and won't.  You
   -- Should do any processing required (but don't modify the params in
   -- the New_Remote_Params call, that's done for you).  Note that if you
   -- don't want to modify the parameters, you don't have to call the
   -- New_Remote_Params parent, such as if you receive a command that
   -- is a "send" and you don't want the value saved.
   procedure Registered(Option : in out Telnet_Base_Option;
                        P      : in Telnet_Processor_Class);
   procedure Handle_Will(Option : in out Telnet_Base_Option;
                         P      : in out Telnet_Processor'Class);
   procedure Handle_Wont(Option : in out Telnet_Base_Option;
                         P      : in out Telnet_Processor'Class);
   procedure Handle_Do(Option : in out Telnet_Base_Option;
                       P      : in out Telnet_Processor'Class);
   procedure Handle_Dont(Option : in out Telnet_Base_Option;
                         P      : in out Telnet_Processor'Class);
   procedure New_Remote_Params(Option : in out Telnet_Base_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array);

   -- Does the option support params?  Set to false by default, an
   -- option that support params should override this and return the
   -- proper value if it is enabled.
   function Supports_Params(Option : in Telnet_Base_Option) return Boolean;

   -- If the remote end had informed us that it will do the option and
   -- the local user has set local will to true, then this will return
   -- true.
   function Will_Active(Option : in Telnet_Base_Option) return Boolean;

   -- If the remote end has informed us that it will do the option and
   -- the local user has set the local do to true, then this will return
   -- true.
   function Do_Active(Option : in Telnet_Base_Option) return Boolean;

   procedure Destroy(Option     : in out Telnet_Base_Option;
                     Option_Ptr : in out Telnet_Option_Class);

   -- Returns True if the remote end has reported that it will do something,
   -- False if not.  The initial value is False;
   function Remote_Will(Option : in Telnet_Base_Option) return Boolean;

   -- Returns True if the remote end has asked us to do something,
   -- False if not.  The initial value is False;
   function Remote_Do(Option : in Telnet_Base_Option) return Boolean;

   -- Set the local end so that we will perform an option.
   procedure Set_Local_Will(Option : in out Telnet_Base_Option;
                            Val    : in Boolean);

   -- Set the local end to request the other end to do an option.
   procedure Set_Local_Do(Option : in out Telnet_Base_Option;
                          Val    : in Boolean);

   -- Return the local set values.
   function Local_Will(Option : in Telnet_Base_Option) return Boolean;
   function Local_Do(Option : in Telnet_Base_Option) return Boolean;


   -- The user should descend from this type if they to be told when
   -- option information changes, such as we are told to do something,
   -- or the other end has told us it will do something, or if
   -- parameters change.
   type Telnet_Option_Change_Handler is abstract tagged null record;
   type Telnet_Option_Change_Handler_Class is
      access all Telnet_Option_Change_Handler'Class;

   -- The following have dummy implementations, override the ones you are
   -- interested in.
   procedure Remote_Do_Changed(H      : in out Telnet_Option_Change_Handler;
                               Option : in out Telnet_Base_Option'Class;
                               P      : in out Telnet_Processor'Class;
                               Val    : in Boolean);
   procedure Remote_Will_Changed(H      : in out Telnet_Option_Change_Handler;
                                 Option : in out Telnet_Base_Option'Class;
                                 P      : in out Telnet_Processor'Class;
                                 Val    : in Boolean);
   procedure Remote_Params_Changed
     (H      : in out Telnet_Option_Change_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array);

   -- Here is where you register the handler that gets called when
   -- the options change.
   procedure Set_Change_Handler
     (Option  : in out Telnet_Base_Option;
      Handler : in Telnet_Option_Change_Handler_Class);

   -- Codes for various options.
   Transmit_Binary_Option      : constant Ada.Streams.Stream_Element := 0;
   Echo_Option                 : constant Ada.Streams.Stream_Element := 1;
   Suppress_Go_Ahead_Option    : constant Ada.Streams.Stream_Element := 3;
   Status_Option               : constant Ada.Streams.Stream_Element := 5;
   Timing_Mark_Option          : constant Ada.Streams.Stream_Element := 6;
   Terminal_Type_Option        : constant Ada.Streams.Stream_Element := 24;
   Window_Size_Option          : constant Ada.Streams.Stream_Element := 31;
   Terminal_Speed_Option       : constant Ada.Streams.Stream_Element := 32;
   Remote_Flow_Control_Option  : constant Ada.Streams.Stream_Element := 33;
   Linemode_Option             : constant Ada.Streams.Stream_Element := 34;
   Environment_Variable_Option : constant Ada.Streams.Stream_Element := 36;
   Extended_Options_Option     : constant Ada.Streams.Stream_Element := 255;

private

   type Telnet_Base_Option is new Telnet_Option with record
      Change_Handler : Telnet_Option_Change_Handler_Class := null;
      Remote_Will    : Boolean := False;
      Remote_Do      : Boolean := False;
      Local_Will     : Ada.Streams.Stream_Element := Wont_Option;
      Local_Do       : Ada.Streams.Stream_Element := Dont_Option;
      Waiting_Will   : Boolean := False;
      Waiting_Do     : Boolean := False;
   end record;

end Asl.Protocol.Telnet.Option;
