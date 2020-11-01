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

with Asl.Protocol.Telnet; use Asl.Protocol.Telnet;
with Asl.Protocol.Telnet.Option; use Asl.Protocol.Telnet.Option;
with Ada.Streams;

package Test_Telnet_Handlers is

   type Expected_Telnet_Command_Ops is (Command);

   type Test_Telnet_Command_Handler is new Telnet_Command_Handler
     with private;
   type Test_Telnet_Command_Handler_Ptr is access Test_Telnet_Command_Handler;

   procedure Handle_Telnet_Command
     (H   : in out Test_Telnet_Command_Handler;
      P   : in out Telnet_Processor'Class;
      Cmd : in Ada.Streams.Stream_Element_Array);

   procedure Add_Expected_Command(H    : in out Test_Telnet_Command_Handler;
                                  Op   : in Expected_Telnet_Command_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array);

   procedure Check_Empty(Name : String;
                         H    : in out Test_Telnet_Command_Handler);

   type Expected_Telnet_Output_Ops is (Output);

   type Test_Telnet_Output_Handler is new Telnet_Output_Handler
     with private;
   type Test_Telnet_Output_Handler_Ptr is access Test_Telnet_Output_Handler;

   procedure Telnet_Output
     (H      : in out Test_Telnet_Output_Handler;
      P      : in out Telnet_Processor'Class;
      Data   : in Ada.Streams.Stream_Element_Array);

   procedure Add_Expected_Command(H    : in out Test_Telnet_Output_Handler;
                                  Op   : in Expected_Telnet_Output_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array);

   procedure Check_Empty(Name : String;
                         H    : in out Test_Telnet_Output_Handler);

   type Expected_Option_Ops is (Registered, Will, Wont, Do_Op, Dont,
                                New_Remote);
   type Test_Telnet_Option is new Telnet_Base_Option with private;
   type Test_Telnet_Option_Ptr is access all Test_Telnet_Option;

   procedure Registered(Option : in out Test_Telnet_Option;
                        P      : in Telnet_Processor_Class);
   procedure Handle_Will(Option : in out Test_Telnet_Option;
                         P      : in out Telnet_Processor'Class);
   procedure Handle_Wont(Option : in out Test_Telnet_Option;
                         P      : in out Telnet_Processor'Class);
   procedure Handle_Do(Option : in out Test_Telnet_Option;
                       P      : in out Telnet_Processor'Class);
   procedure Handle_Dont(Option : in out Test_Telnet_Option;
                         P      : in out Telnet_Processor'Class);
   procedure New_Remote_Params(Option : in out Test_Telnet_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array);

   procedure Add_Expected_Command(O    : in out Test_Telnet_Option;
                                  Op   : in Expected_Option_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array;
                                  Old  : in Ada.Streams.Stream_Element_Array);

   procedure Check_Empty(Name : String;
                         O    : in out Test_Telnet_Option);


   type Expected_Option_Change_Ops is (Do_Change, Will_Change,
                                       Remote_Params_Change);
   type Test_Option_Change_Handler is new Telnet_Option_Change_Handler
     with private;
   type Test_Option_Change_Handler_Ptr is
      access all Test_Option_Change_Handler;

   procedure Remote_Do_Changed(H      : in out Test_Option_Change_Handler;
                               Option : in out Telnet_Base_Option'Class;
                               P      : in out Telnet_Processor'Class;
                               Val    : in Boolean);

   procedure Remote_Will_Changed(H      : in out Test_Option_Change_Handler;
                                 Option : in out Telnet_Base_Option'Class;
                                 P      : in out Telnet_Processor'Class;
                                 Val    : in Boolean);

   procedure Remote_Params_Changed
     (H      : in out Test_Option_Change_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array);

   procedure Add_Expected_Command(O    : in out Test_Option_Change_Handler;
                                  Op   : in Expected_Option_Change_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array;
                                  Old  : in Ada.Streams.Stream_Element_Array;
                                  Val  : in Boolean);

   procedure Check_Empty(Name : String;
                         O    : in out Test_Option_Change_Handler);

private

   type Expected_Telnet_Command;
   type Expected_Telnet_Command_Ptr is access all Expected_Telnet_Command;
   type Expected_Telnet_Command(Data_Size : Ada.Streams.Stream_Element_Count)
   is record
      Op   : Expected_Telnet_Command_Ops;
      Data : Ada.Streams.Stream_Element_Array(1 .. Data_Size);
      Next : Expected_Telnet_Command_Ptr := null;
   end record;

   type Test_Telnet_Command_Handler
   is new Telnet_Command_Handler with record
      Cmds : Expected_Telnet_Command_Ptr := null;
      Tail : Expected_Telnet_Command_Ptr := null;
   end record;


   type Expected_Telnet_Output;
   type Expected_Telnet_Output_Ptr is access all Expected_Telnet_Output;
   type Expected_Telnet_Output(Data_Size : Ada.Streams.Stream_Element_Count)
   is record
      Op   : Expected_Telnet_Output_Ops;
      Data : Ada.Streams.Stream_Element_Array(1 .. Data_Size);
      Next : Expected_Telnet_Output_Ptr := null;
   end record;

   type Test_Telnet_Output_Handler
   is new Telnet_Output_Handler with record
      Cmds : Expected_Telnet_Output_Ptr := null;
      Tail : Expected_Telnet_Output_Ptr := null;
   end record;


   type Expected_Telnet_Option;
   type Expected_Telnet_Option_Ptr is access all Expected_Telnet_Option;
   type Expected_Telnet_Option(Data_Size : Ada.Streams.Stream_Element_Count;
                               Old_Size  : Ada.Streams.Stream_Element_Count)
   is record
      Op   : Expected_Option_Ops;
      Data : Ada.Streams.Stream_Element_Array(1 .. Data_Size);
      Old  : Ada.Streams.Stream_Element_Array(1 .. Old_Size);
      Next : Expected_Telnet_Option_Ptr;
   end record;

   type Test_Telnet_Option is new Telnet_Base_Option with record
      Cmds, Tail : Expected_Telnet_Option_Ptr := null;
   end record;

   type Expected_Option_Change;
   type Expected_Option_Change_Ptr is access all Expected_Option_Change;
   type Expected_Option_Change(Data_Size : Ada.Streams.Stream_Element_Count;
                               Old_Size  : Ada.Streams.Stream_Element_Count)
   is record
      Op   : Expected_Option_Change_Ops;
      Data : Ada.Streams.Stream_Element_Array(1 .. Data_Size);
      Old  : Ada.Streams.Stream_Element_Array(1 .. Old_Size);
      Val  : Boolean;
      Next : Expected_Option_Change_Ptr;
   end record;

   type Test_Option_Change_Handler
   is new Telnet_Option_Change_Handler with record
      Cmds, Tail : Expected_Option_Change_Ptr := null;
   end record;

end Test_Telnet_Handlers;
