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
with Test_Telnet_Handlers; use Test_Telnet_Handlers;
with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
use type Ada.Streams.Stream_Element_Array;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Telnet is
   P : Telnet_Processor_Class := new Telnet_Processor;
   H : Test_Telnet_Handlers.Test_Telnet_Command_Handler_Ptr;
   G : Test_Telnet_Handlers.Test_Telnet_Output_Handler_Ptr;

   Data1 : Ada.Streams.Stream_Element_Array(1 .. 4) := (1, 2, 3, 4);
   Data2 : Ada.Streams.Stream_Element_Array(1 .. 4) := (1, 2, 255, 3);
   Data2_1 : Ada.Streams.Stream_Element_Array(1 .. 5) := (1, 2, 255, 255, 3);

   Data3 : Ada.Streams.Stream_Element_Array(1 .. 4) := (5, 6, 7, 8);
   Data4 : Ada.Streams.Stream_Element_Array(1 .. 7)
     := (9, 255, 243, 255, 255, 10, 12);
   Data4_1 : Ada.Streams.Stream_Element_Array(1 .. 4)
     := (9, 255, 10, 12);
   Data5 : Ada.Streams.Stream_Element_Array(1 .. 8)
     := (1, 255, 250, 29, 9, 255, 240, 2);
   Data5_1 : Ada.Streams.Stream_Element_Array(1 .. 2) := (1, 2);
   Data6 : Ada.Streams.Stream_Element_Array(1 .. 11)
     := (9, 255, 250, 5, 11, 255, 255, 12, 255, 240, 13);
   Data6_1 : Ada.Streams.Stream_Element_Array(1 .. 2) := (9, 13);
   Data6_2 : Ada.Streams.Stream_Element_Array(1 .. 3) := (11, 255, 12);

   Data7_1 : Ada.Streams.Stream_Element_Array(1 .. 2) := (255, 251);
   Data7_2 : Ada.Streams.Stream_Element_Array(1 .. 4) := (1, 255, 252, 1);
   Data7_3 : Ada.Streams.Stream_Element_Array(1 .. 1) := (1 => 255);
   Data7_4 : Ada.Streams.Stream_Element_Array(1 .. 5) := (253, 1, 255, 254, 1);

   Data8_1 : Ada.Streams.Stream_Element_Array(1 .. 3) := (255, 253, 99);
   Data8_2 : Ada.Streams.Stream_Element_Array(1 .. 3) := (255, 251, 99);

   Empty_Data : Ada.Streams.Stream_Element_Array(1 .. 0);

   Output1 : Ada.Streams.Stream_Element_Array(1 .. 3);
   Output2 : Ada.Streams.Stream_Element_Array(1 .. 4);
   Output3 : Ada.Streams.Stream_Element_Array(1 .. 5);
   Output4 : Ada.Streams.Stream_Element_Array(1 .. 6);

   Next  : Ada.Streams.Stream_Element_Offset;
   Last  : Ada.Streams.Stream_Element_Offset;

   Option1 : Test_Telnet_Option_Ptr := new Test_Telnet_Option;
   Option2 : Test_Telnet_Option_Ptr := new Test_Telnet_Option;

   Option1_Change : Test_Option_Change_Handler_Ptr
     := new Test_Option_Change_Handler;
   Option2_Change : Test_Option_Change_Handler_Ptr
     := new Test_Option_Change_Handler;

   package ElementOff_IO is new Integer_IO(Ada.Streams.Stream_Element_Offset);
   use ElementOff_IO;
   package Element_IO is new Modular_IO(Ada.Streams.Stream_Element);
   use Element_IO;

   procedure Validate_Output(Name   : String;
                             Output : Ada.Streams.Stream_Element_Array;
                             Expect : Ada.Streams.Stream_Element_Array;
                             Size   : Ada.Streams.Stream_Element_Count;
                             Next   : Ada.Streams.Stream_Element_Offset;
                             Enext  : Ada.Streams.Stream_Element_Offset;
                             Last   : Ada.Streams.Stream_Element_Offset;
                             Elast  : Ada.Streams.Stream_Element_Offset) is
   begin
      if (Next /= Enext) then
         Put(Name & " failed on Next: ");
         Put(Next);
         New_Line;
         raise Constraint_Error;
      end if;
      if (Last /= Elast) then
         Put(Name & " failed on Last: ");
         Put(Last);
         New_Line;
         raise Constraint_Error;
      end if;
      if (Output(1 .. Size) /= Expect(1 .. Size)) then
         Put_Line(Name & " failed on data");
         Put("Was:");
         for I in 1 .. Size loop
            Put(" ");
            Put(Output(I));
         end loop;
         New_Line;
         Put("Exp:");
         for I in 1 .. Size loop
            Put(" ");
            Put(Expect(I));
         end loop;
         New_Line;
         raise Constraint_Error;
      end if;
   end Validate_Output;

begin
   H := new Test_Telnet_Handlers.Test_Telnet_Command_Handler;
   G := new Test_Telnet_Handlers.Test_Telnet_Output_Handler;
   Initialize(P,
              Telnet_Command_Handler_Class(H),
              Telnet_Output_Handler_Class(G));

   Process_Output(P.all, Data1, Output1, Next, Last);
   Validate_Output("Process output 1.1",
                   Output1, Data1, 3,
                   Next, 4,
                   Last, 3);
   Process_Output(P.all, Data1, Output2, Next, Last);
   Validate_Output("Process output 1.1",
                   Output2, Data1, 4,
                   Next, 5,
                   Last, 4);
   Process_Output(P.all, Data1, Output3, Next, Last);
   Validate_Output("Process output 1.1",
                   Output3, Data1, 4,
                   Next, 5,
                   Last, 4);

   Process_Output(P.all, Data2, Output1, Next, Last);
   Validate_Output("Process output 2.1",
                   Output1, Data2, 2,
                   Next, 3,
                   Last, 2);
   Process_Output(P.all, Data2, Output2, Next, Last);
   Validate_Output("Process output 2.2",
                   Output2, Data2_1, 4,
                   Next, 4,
                   Last, 4);
   Process_Output(P.all, Data2, Output3, Next, Last);
   Validate_Output("Process output 2.3",
                   Output3, Data2_1, 5,
                   Next, 5,
                   Last, 5);
   Process_Output(P.all, Data2, Output4, Next, Last);
   Validate_Output("Process output 2.4",
                   Output4, Data2_1, 5,
                   Next, 5,
                   Last, 5);

   Process_Input(P.all, Data3, Output1, Next, Last);
   Validate_Output("Process input 3.1",
                   Output1, Data3, 3,
                   Next, 4,
                   Last, 3);
   Process_Input(P.all, Data3, Output2, Next, Last);
   Validate_Output("Process input 3.2",
                   Output2, Data3, 4,
                   Next, 5,
                   Last, 4);
   Process_Input(P.all, Data3, Output3, Next, Last);
   Validate_Output("Process input 3.3",
                   Output3, Data3, 4,
                   Next, 5,
                   Last, 4);

   Test_Telnet_Handlers.Add_Expected_Command(H.all, Command, (1 => 243));
   Process_Input(P.all, Data4, Output1, Next, Last);
   Validate_Output("Process input 4.1",
                   Output1, Data4_1, 3,
                   Next, 7,
                   Last, 3);
   Test_Telnet_Handlers.Check_Empty("Process input 4.1", H.all);
   Test_Telnet_Handlers.Add_Expected_Command(H.all, Command, (1 => 243));
   Process_Input(P.all, Data4, Output2, Next, Last);
   Validate_Output("Process input 4.2",
                   Output2, Data4_1, 4,
                   Next, 8,
                   Last, 4);
   Test_Telnet_Handlers.Check_Empty("Process input 4.1", H.all);
   Test_Telnet_Handlers.Add_Expected_Command(H.all, Command, (1 => 243));
   Process_Input(P.all, Data4, Output3, Next, Last);
   Validate_Output("Process input 4.3",
                   Output3, Data4_1, 4,
                   Next, 8,
                   Last, 4);
   Test_Telnet_Handlers.Check_Empty("Process input 4.1", H.all);

   Initialize(Option1.all);
   Initialize(Option2.all);

   Test_Telnet_Handlers.Add_Expected_Command(Option1.all, Registered,
                                             Empty_Data,
                                             Empty_Data);
   Register_Option(P.all, 1, Telnet_Option_Class(Option1));
   Test_Telnet_Handlers.Check_Empty("Register Option 6.1", Option1.all);
   Test_Telnet_Handlers.Check_Empty("Register Option 6.1", G.all);
   Set_Local_Will(Option2.all, True);
   Set_Local_Do(Option2.all, True);
   Test_Telnet_Handlers.Add_Expected_Command(Option2.all, Registered,
                                             Empty_Data,
                                             Empty_Data);
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 253, 5));
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 251, 5));
   Register_Option(P.all, 5, Telnet_Option_Class(Option2));
   Test_Telnet_Handlers.Check_Empty("Register Option 6.2", G.all);
   Test_Telnet_Handlers.Check_Empty("Register Option 6.2", Option2.all);
   if (Get_Local_Param_Array(Option1.all) /= Empty_Data) then
      Put_Line("Get local param array 6.3 not empty");
      raise Constraint_Error;
   end if;
   Set_Local_Param_Array(Option1.all, Data3);
   if (Get_Local_Param_Array(Option1.all) /= Data3) then
      Put_Line("Get local param array 6.4 not correct");
      raise Constraint_Error;
   end if;

   Set_Change_Handler(Telnet_Base_Option'Class(Option1.all),
                      Telnet_Option_Change_Handler_Class
                        (Option1_Change));
   Set_Change_Handler(Telnet_Base_Option'Class(Option2.all),
                      Telnet_Option_Change_Handler_Class
                        (Option2_Change));

   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output,
                                             (255, 250, 1, 13, 14, 255, 255,
                                              15, 255, 240));
   Send_Params(P.all, Option1.all, (13, 14, 255, 15));
   Test_Telnet_Handlers.Check_Empty("Internal output 5.1", G.all);

   -- try a non-existing option first.
   Process_Input(P.all, Data5, Output1, Next, Last);
   Validate_Output("Options 7.1",
                   Output1, Data5_1, 2,
                   Next, 9,
                   Last, 2);
   -- Now a valid option
   Test_Telnet_Handlers.Add_Expected_Command(Option2.all, New_Remote,
                                             Data6_2,
                                             Empty_Data);
   Test_Telnet_Handlers.Add_Expected_Command(Option2_Change.all,
                                             Remote_Params_Change,
                                             Data6_2,
                                             Empty_Data,
                                             False);
   Process_Input(P.all, Data6, Output1, Next, Last);
   Validate_Output("Options 7.2",
                   Output1, Data6_1, 2,
                   Next, 12,
                   Last, 2);
   Test_Telnet_Handlers.Check_Empty("Option 7.2", Option2.all);
   Test_Telnet_Handlers.Check_Empty("Option 7.2", Option2_Change.all);

   Test_Telnet_Handlers.Add_Expected_Command(Option1.all, Will,
                                             Empty_Data,
                                             Empty_Data);
   -- Receiving a will will cause a dont to be sent
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 254, 1));
   Test_Telnet_Handlers.Add_Expected_Command(Option1_Change.all,
                                             Will_Change,
                                             Empty_Data,
                                             Empty_Data,
                                             True);
   Test_Telnet_Handlers.Add_Expected_Command(Option1.all, Wont,
                                             Empty_Data,
                                             Empty_Data);
   -- Receiving a wont will cause a dont to be sent
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 254, 1));
   Test_Telnet_Handlers.Add_Expected_Command(Option1_Change.all,
                                             Will_Change,
                                             Empty_Data,
                                             Empty_Data,
                                             False);
   Test_Telnet_Handlers.Add_Expected_Command(Option1.all, Do_Op,
                                             Empty_Data,
                                             Empty_Data);
   -- Receiving a do will cause a wont to be sent.
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 252, 1));
   Test_Telnet_Handlers.Add_Expected_Command(Option1_Change.all,
                                             Do_Change,
                                             Empty_Data,
                                             Empty_Data,
                                             True);
   Test_Telnet_Handlers.Add_Expected_Command(Option1.all, Dont,
                                             Empty_Data,
                                             Empty_Data);
   -- Receiving a dont will cause a wont to be sent.
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 252, 1));
   Test_Telnet_Handlers.Add_Expected_Command(Option1_Change.all,
                                             Do_Change,
                                             Empty_Data,
                                             Empty_Data,
                                             False);
   Process_Input(P.all, Data7_1, Output1, Next, Last);
   Validate_Output("Options 7.3.1",
                   Output1, Empty_Data , 0,
                   Next, 3,
                   Last, 0);
   Process_Input(P.all, Data7_2, Output1, Next, Last);
   Validate_Output("Options 7.3.2",
                   Output1, Empty_Data , 0,
                   Next, 5,
                   Last, 0);
   Process_Input(P.all, Data7_3, Output1, Next, Last);
   Validate_Output("Options 7.3.3",
                   Output1, Empty_Data , 0,
                   Next, 2,
                   Last, 0);
   Process_Input(P.all, Data7_4, Output1, Next, Last);
   Validate_Output("Options 7.3.4",
                   Output1, Empty_Data , 0,
                   Next, 6,
                   Last, 0);
   Test_Telnet_Handlers.Check_Empty("Option 7.3", Option2.all);
   Test_Telnet_Handlers.Check_Empty("Option 7.3", Option2_Change.all);

   -- Validate that getting dos and wills for options we don't have will
   -- return the won'ts and don'ts
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 252, 99));
   Process_Input(P.all, Data8_1, Output1, Next, Last);
   Validate_Output("Options 7.4",
                   Output1, Empty_Data , 0,
                   Next, 4,
                   Last, 0);
   Test_Telnet_Handlers.Check_Empty("Options 7.4", G.all);
   Test_Telnet_Handlers.Add_Expected_Command(G.all, Output, (255, 254, 99));
   Process_Input(P.all, Data8_2, Output1, Next, Last);
   Validate_Output("Options 7.5",
                   Output1, Empty_Data , 0,
                   Next, 4,
                   Last, 0);
   Test_Telnet_Handlers.Check_Empty("Options 7.5", G.all);

   Destroy(P.all);
   Put_Line("Tests passed");
end Test_Telnet;
