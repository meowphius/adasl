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
-- This package defines a processor that implements the telnet protocol
-- (as defined in RFCs 854 and 855.  It handles command processing and
-- some basic option processing.  You can register options against if,
-- if option commands come in on a registered option it will call the
-- handler for that option.
--
-- To use this, allocate an telnet processor (or get one from the
-- Telnet.Stream_IO package).  At allocation, the processor must have
-- a valid way to do output.  However, don't read or process any data
-- yet.  Then allocate each option, configure it, and register it
-- with the telnet processor.  Once all the options have been registered,
-- start the normal I/O process and the telnet processor and options
-- will negotiate everything.
--
-- Note that you cannot reuse the same instance of an option processor
-- with different telnet processors.
--
-- Note that the option will free themselves when the telnet processor
-- is destroyed, but the command and output handlers will not, the
-- user is responsible for freeing them.

with Ada.Streams;

package Asl.Protocol.Telnet is

   -- This is the type that handles telnet command processing.  You must
   -- allocate one of these dynamically to use.
   type Telnet_Processor is tagged limited private;
   type Telnet_Processor_Ptr is access all Telnet_Processor;
   type Telnet_Processor_Class is access all Telnet_Processor'Class;

   -- If the user wishes to receive telnet commands as they happen, they
   -- should extend this type and define a handler.  Pass an instance of
   -- the type into the Initialize or Set_Handler calls, and the handler
   -- will be called on the type.
   type Telnet_Command_Handler is abstract tagged private;
   type Telnet_Command_Handler_Class is
      access all Telnet_Command_Handler'Class;

   -- Receives telnet commands as they happen from the processor.  The
   -- actual command is passes as an array of stream elements, the user
   -- must interpret them.
   procedure Handle_Telnet_Command
     (H   : in out Telnet_Command_Handler;
      P   : in out Telnet_Processor'Class;
      Cmd : in Ada.Streams.Stream_Element_Array)
     is abstract;

   -- This class must be registered with the telnet processor so it can
   -- do output on the stream.
   type Telnet_Output_Handler is abstract tagged private;
   type Telnet_Output_Handler_Class is access all Telnet_Output_Handler'Class;

   -- Called by the internal processor to output some data.  This must go
   -- out as soon as possible.  Also, the output MUST be atomic, all the
   -- elements written in this call must go out together, even in a
   -- multi-threaded situation.
   procedure Telnet_Output
     (H      : in out Telnet_Output_Handler;
      P      : in out Telnet_Processor'Class;
      Data   : in Ada.Streams.Stream_Element_Array)
     is abstract;


   -- Initialize an instance of the telnet processor.  Note that the
   -- command handler may be null, but the output handler may not.
   procedure Initialize(P       : access Telnet_Processor;
                        Handler : in Telnet_Command_Handler_Class;
                        Output  : in Telnet_Output_Handler_Class);

   -- Clean up the telnet processor.
   procedure Destroy(P : in out Telnet_Processor);

   -- Process some data to be output.  All output data must be processed
   -- because some substitutions must be done.  The data to process is passed
   -- in as Data, the processed output is put into Output.  The next input
   -- character to process is put into Next_In (in case not enough space
   -- was in the output array), the last output character put in the output
   -- array is placed in Last_Out.  Note that if Output is 1 greater than
   -- twice as large as Input, all the input data is guaranteed to be
   -- processed and Next_In can be ignored.
   procedure Process_Output
     (P        : in out Telnet_Processor;
      Data     : in Ada.Streams.Stream_Element_Array;
      Output   : out Ada.Streams.Stream_Element_Array;
      Next_In  : out Ada.Streams.Stream_Element_Offset;
      Last_Out : out Ada.Streams.Stream_Element_Offset);

   -- Process some data from the input.  All input data must be processed
   -- because commands must be extracted.  The raw data to process is passed
   -- in as Data, the processed input is put into Output.  The next raw input
   -- character to process is put into Next_In (in case not enough space
   -- was in the output array), the last character put in the output
   -- array is placed in Last_Out.  Note that if Output is as large
   -- as Input, all the input data is guaranteed to be processed and
   -- Next_In can be ignored.
   procedure Process_Input
     (P        : in out Telnet_Processor;
      Data     : in Ada.Streams.Stream_Element_Array;
      Output   : out Ada.Streams.Stream_Element_Array;
      Next_In  : out Ada.Streams.Stream_Element_Offset;
      Last_Out : out Ada.Streams.Stream_Element_Offset);

   -- Standard telnet character definitions from RFC 854.
   CR                : constant Ada.Streams.Stream_Element := 13;
   LF                : constant Ada.Streams.Stream_Element := 10;
   NUL               : constant Ada.Streams.Stream_Element := 0;

   -- The following is the list of telnet commands defined in
   -- RFC 854.  When a command handler is called, this will appear
   -- as the first element of the array.
   SE                : constant Ada.Streams.Stream_Element := 240;
   NOP               : constant Ada.Streams.Stream_Element := 241;
   Data_Mark         : constant Ada.Streams.Stream_Element := 242;
   Break             : constant Ada.Streams.Stream_Element := 243;
   Interrupt_Process : constant Ada.Streams.Stream_Element := 244;
   Abort_Output      : constant Ada.Streams.Stream_Element := 245;
   Are_You_There     : constant Ada.Streams.Stream_Element := 246;
   Erase_Character   : constant Ada.Streams.Stream_Element := 247;
   Erase_Line        : constant Ada.Streams.Stream_Element := 248;
   Go_Ahead          : constant Ada.Streams.Stream_Element := 249;
   SB                : constant Ada.Streams.Stream_Element := 250;
   Will_Option       : constant Ada.Streams.Stream_Element := 251;
   Wont_Option       : constant Ada.Streams.Stream_Element := 252;
   Do_Option         : constant Ada.Streams.Stream_Element := 253;
   Dont_Option       : constant Ada.Streams.Stream_Element := 254;
   IAC               : constant Ada.Streams.Stream_Element := 255;

   -- Send a one-byte command.  Two-byte commands are only sent by
   -- the option processors, so don't have that here.
   procedure Send_Command(P   : in out Telnet_Processor;
                          Cmd : Ada.Streams.Stream_Element);


   type Telnet_Option is abstract tagged limited private;
   type Telnet_Option_Class is access all Telnet_Option'Class;

   -- If you override any of these, you need to call these, anyway, since
   -- the are used to track the internal state of the will and won't.
   procedure Registered(Option : in out Telnet_Option;
                        P      : in Telnet_Processor_Class)
      is abstract;
   procedure Handle_Will(Option : in out Telnet_Option;
                         P      : in out Telnet_Processor'Class)
      is abstract;
   procedure Handle_Wont(Option : in out Telnet_Option;
                         P      : in out Telnet_Processor'Class)
      is abstract;
   procedure Handle_Do(Option : in out Telnet_Option;
                       P      : in out Telnet_Processor'Class)
      is abstract;
   procedure Handle_Dont(Option : in out Telnet_Option;
                         P      : in out Telnet_Processor'Class)
      is abstract;
   procedure New_Remote_Params(Option : in out Telnet_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array)
      is abstract;

   -- Returns True if the option supports params, False if not.
   function Supports_Params(Option : in Telnet_Option) return Boolean
      is abstract;
   -- Returns True if the option is ready to use on the local side.
   function Will_Active(Option : in Telnet_Option) return Boolean
      is abstract;
   -- Returns True if the option is ready to use on the remote side.
   function Do_Active(Option : in Telnet_Option) return Boolean
      is abstract;
   -- Called by the telnet processor to clean up the option and free
   -- itself.  Note that the call is responsible for freeing itself
   -- using the "option_ptr" value, if Option_Ptr is not null.  This
   -- version of the procedure should also be called, but the Option_Ptr
   -- is ignored in this one since the type is abstract.
   procedure Destroy(Option     : in out Telnet_Option;
                     Option_Ptr : in out Telnet_Option_Class);

   -- These should not be called by user programs, only from the option
   -- processor classes.
   function Get_Option_Code(Option : in Telnet_Option'Class)
                            return Ada.Streams.Stream_Element;
   procedure Set_Local_Param_Array(Option : in out Telnet_Option'Class;
                                   Params : Ada.Streams.Stream_Element_Array);
   function Get_Local_Param_Array(Option : in Telnet_Option'Class)
                                  return Ada.Streams.Stream_Element_Array;
   procedure Set_Remote_Param_Array(Option : in out Telnet_Option'Class;
                                    Params : Ada.Streams.Stream_Element_Array);
   function Get_Remote_Param_Array(Option : in Telnet_Option'Class)
                                   return Ada.Streams.Stream_Element_Array;
   procedure Send_Params(P      : in out Telnet_Processor;
                         Option : in Telnet_Option'Class;
                         Data   : in Ada.Streams.Stream_Element_Array);
   procedure Send_Command(P    : in out Telnet_Processor;
                          Cmd  : Ada.Streams.Stream_Element;
                          Parm : Ada.Streams.Stream_Element);
   function Get_Processor(Option : in Telnet_Option)
                          return Telnet_Processor_Class;



   -- Register an option with the telnet option processor.  If the option
   -- code is already registered with the telnet processor, or if the
   -- option is already registered with another telnet processor, a
   -- constraint error will be raised.  Note that once you have
   -- registered an
   procedure Register_Option
     (P           : in out Telnet_Processor;
      Option_Code : in Ada.Streams.Stream_Element;
      Option      : in Telnet_Option_Class);

private

   type Telnet_Params_Ptr is access Ada.Streams.Stream_Element_Array;

   protected type Param_Holder is
      procedure Set(New_Params : Ada.Streams.Stream_Element_Array);
      function Get return Ada.Streams.Stream_Element_Array;
      procedure Destroy;
   private
      Params : Telnet_Params_Ptr
        := new Ada.Streams.Stream_Element_Array(1 .. 0);
   end Param_Holder;

   type Telnet_Command_Handler is abstract tagged null record;

   type Telnet_Output_Handler is abstract tagged null record;

   type Telnet_State is (Normal, Cmd1, Cmd2, Subneg_Option,
                         Subneg_Params, Subneg_Cmd1, Subneg_Cmd2);

   type Telnet_Option is abstract tagged limited record
      Next           : Telnet_Option_Class := null;

      Option_Code    : Ada.Streams.Stream_Element;
      Local_Params   : Param_Holder;
      Remote_Params  : Param_Holder;
      Registered     : Boolean := False;
      Processor      : Telnet_Processor_Class;
   end record;

   type Telnet_Processor is tagged limited record
      Self_Ptr       : Telnet_Processor_Class;
      State          : Telnet_State := Normal;
      Cmd            : Ada.Streams.Stream_Element_Array(1 .. 2);
      Cmd_Handler    : Telnet_Command_Handler_Class := null;
      Out_Handler    : Telnet_Output_Handler_Class;
      Options        : Telnet_Option_Class := null;
      Working_Params : Telnet_Params_Ptr := null;
      Params_Length  : Ada.Streams.Stream_Element_Count;
      Working_Option : Telnet_Option_Class := null;
      Last_In_Was_CR  : Boolean := False;
      Last_Out_Was_CR : Boolean := False;
   end record;

end Asl.Protocol.Telnet;
