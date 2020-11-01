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

with Ada.Unchecked_Deallocation;
with Ada.Streams; use type Ada.Streams.Stream_Element_Array;
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Telnet_Handlers is

   package ElementOff_IO is new Integer_IO(Ada.Streams.Stream_Element_Offset);
   use ElementOff_IO;
   package Element_IO is new Modular_IO(Ada.Streams.Stream_Element);
   use Element_IO;

   procedure Free is
     new Ada.Unchecked_Deallocation(Expected_Telnet_Command,
                                    Expected_Telnet_Command_Ptr);

   procedure Get_Next_Command(H   : in out Test_Telnet_Command_Handler;
                              Cmd : out Expected_Telnet_Command_Ptr) is
   begin
      if (H.Cmds = null) then
         raise Constraint_Error;
      else
         Cmd := H.Cmds;
         H.Cmds := H.Cmds.Next;
         if (H.Cmds = null) then
            H.Tail := null;
         end if;
      end if;
   end;


   procedure Handle_Telnet_Command
     (H   : in out Test_Telnet_Command_Handler;
      P   : in out Telnet_Processor'Class;
      Cmd : in Ada.Streams.Stream_Element_Array)
   is
      Expected : Expected_Telnet_Command_Ptr;
   begin
      Get_Next_Command(H, Expected);
      if (Expected.Op /= Command) then
         raise Constraint_Error;
      end if;
      if (Cmd /= Expected.Data) then
         raise Constraint_Error;
      end if;
   end Handle_Telnet_Command;

   procedure Add_Expected_Command(H    : in out Test_Telnet_Command_Handler;
                                  Op   : in Expected_Telnet_Command_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array)
   is
      Cmd : Expected_Telnet_Command_Ptr;
   begin
      Cmd := new Expected_Telnet_Command(Data'Length);
      Cmd.Op   := Op;
      Cmd.Data := Data;
      if (H.Tail = null) then
         H.Cmds := Cmd;
         H.Tail := Cmd;
      else
         H.Tail.Next := Cmd;
         H.Tail := Cmd;
      end if;
   end Add_Expected_Command;

   procedure Check_Empty(Name : String;
                         H    : in out Test_Telnet_Command_Handler) is
   begin
      if (H.Cmds /= null) then
         Put_Line(Name & " had non-empty telnet command handler");
         raise Constraint_Error;
      end if;
   end Check_Empty;

   procedure Get_Next_Command(H   : in out Test_Telnet_Output_Handler;
                              Cmd : out Expected_Telnet_Output_Ptr) is
   begin
      if (H.Cmds = null) then
         raise Constraint_Error;
      else
         Cmd := H.Cmds;
         H.Cmds := H.Cmds.Next;
         if (H.Cmds = null) then
            H.Tail := null;
         end if;
      end if;
   end;

   procedure Telnet_Output
     (H      : in out Test_Telnet_Output_Handler;
      P      : in out Telnet_Processor'Class;
      Data   : in Ada.Streams.Stream_Element_Array)
   is
      Expected : Expected_Telnet_Output_Ptr;
      -- The following works around a bug in GNAT.  It seems if you have an
      -- array in a record and use a discriminant to set its size, the
      -- array will not compare correctly with another array when the
      -- indexes are different ranges (but the same length).
      Tmp_Data : Ada.Streams.Stream_Element_Array(1 .. Data'Length) := Data;
   begin
      Get_Next_Command(H, Expected);
      if (Expected.Op /= Output) then
         raise Constraint_Error;
      end if;
      if ((Tmp_Data'Length /= Expected.Data'Length)
          or else (Tmp_Data /= Expected.Data))
      then
         Put_Line("Telnet Output failed on data");
         Put("Was:");
         for I in Tmp_Data'Range loop
            Put(" ");
            Put(I);
            Put(":");
            Put(Tmp_Data(I));
         end loop;
         New_Line;
         Put("Exp:");
         for I in Expected.Data'Range loop
            Put(" ");
            Put(I);
            Put(":");
            Put(Expected.Data(I));
         end loop;
         New_Line;
         raise Constraint_Error;
      end if;
   end Telnet_Output;

   procedure Add_Expected_Command(H    : in out Test_Telnet_Output_Handler;
                                  Op   : in Expected_Telnet_Output_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array)
   is
      Cmd : Expected_Telnet_Output_Ptr;
   begin
      Cmd := new Expected_Telnet_Output(Data'Length);
      Cmd.Op   := Op;
      Cmd.Data := Data;
      if (H.Tail = null) then
         H.Cmds := Cmd;
         H.Tail := Cmd;
      else
         H.Tail.Next := Cmd;
         H.Tail := Cmd;
      end if;
   end Add_Expected_Command;

   procedure Check_Empty(Name : String;
                         H    : in out Test_Telnet_Output_Handler) is
   begin
      if (H.Cmds /= null) then
         Put_Line(Name & " had non-empty telnet output handler");
         raise Constraint_Error;
      end if;
   end Check_Empty;


   procedure Get_Next_Command(O   : in out Test_Telnet_Option;
                              Cmd : out Expected_Telnet_Option_Ptr) is
   begin
      if (O.Cmds = null) then
         raise Constraint_Error;
      else
         Cmd := O.Cmds;
         O.Cmds := O.Cmds.Next;
         if (O.Cmds = null) then
            O.Tail := null;
         end if;
      end if;
   end;

   procedure Registered(Option : in out Test_Telnet_Option;
                        P      : in Telnet_Processor_Class)
   is
      Expected : Expected_Telnet_Option_Ptr;
   begin
      Registered(Telnet_Base_Option(Option), P);
      Get_Next_Command(Option, Expected);
      if (Expected.Op /= Registered) then
         raise Constraint_Error;
      end if;
   end Registered;

   procedure Handle_Will(Option : in out Test_Telnet_Option;
                         P      : in out Telnet_Processor'Class)
   is
      Expected : Expected_Telnet_Option_Ptr;
   begin
      Handle_Will(Telnet_Base_Option(Option), P);
      Get_Next_Command(Option, Expected);
      if (Expected.Op /= Will) then
         raise Constraint_Error;
      end if;
   end Handle_Will;

   procedure Handle_Wont(Option : in out Test_Telnet_Option;
                         P      : in out Telnet_Processor'Class)
   is
      Expected : Expected_Telnet_Option_Ptr;
   begin
      Handle_Wont(Telnet_Base_Option(Option), P);
      Get_Next_Command(Option, Expected);
      if (Expected.Op /= Wont) then
         raise Constraint_Error;
      end if;
   end Handle_Wont;

   procedure Handle_Do(Option : in out Test_Telnet_Option;
                       P      : in out Telnet_Processor'Class)
   is
      Expected : Expected_Telnet_Option_Ptr;
   begin
      Handle_Do(Telnet_Base_Option(Option), P);
      Get_Next_Command(Option, Expected);
      if (Expected.Op /= Do_Op) then
         raise Constraint_Error;
      end if;
   end Handle_Do;

   procedure Handle_Dont(Option : in out Test_Telnet_Option;
                         P      : in out Telnet_Processor'Class)
   is
      Expected : Expected_Telnet_Option_Ptr;
   begin
      Handle_Dont(Telnet_Base_Option(Option), P);
      Get_Next_Command(Option, Expected);
      if (Expected.Op /= Dont) then
         raise Constraint_Error;
      end if;
   end Handle_Dont;

   procedure New_Remote_Params(Option : in out Test_Telnet_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array)
   is
      Expected : Expected_Telnet_Option_Ptr;
      Old : Ada.Streams.Stream_Element_Array := Get_Remote_Param_Array(Option);
   begin
      New_Remote_Params(Telnet_Base_Option(Option), P, Params);
      Get_Next_Command(Option, Expected);
      if (Expected.Op /= New_Remote) then
         raise Constraint_Error;
      end if;
      if (Expected.Data /= Params) then
         raise Constraint_Error;
      end if;
      if (Expected.Old /= Old) then
         raise Constraint_Error;
      end if;
   end New_Remote_Params;

   procedure Add_Expected_Command(O    : in out Test_Telnet_Option;
                                  Op   : in Expected_Option_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array;
                                  Old  : in Ada.Streams.Stream_Element_Array)
   is
      Cmd : Expected_Telnet_Option_Ptr;
   begin
      Cmd := new Expected_Telnet_Option(Data'Length, Old'Length);
      Cmd.Op   := Op;
      Cmd.Data := Data;
      Cmd.Old  := Old;
      if (O.Tail = null) then
         O.Cmds := Cmd;
         O.Tail := Cmd;
      else
         O.Tail.Next := Cmd;
         O.Tail := Cmd;
      end if;
   end Add_Expected_Command;


   procedure Check_Empty(Name : String;
                         O    : in out Test_Telnet_Option) is
   begin
      if (O.Cmds /= null) then
         Put_Line(Name & " had non-empty telnet option handler");
         raise Constraint_Error;
      end if;
   end Check_Empty;


   procedure Get_Next_Command(O   : in out Test_Option_Change_Handler;
                              Cmd : out Expected_Option_Change_Ptr) is
   begin
      if (O.Cmds = null) then
         raise Constraint_Error;
      else
         Cmd := O.Cmds;
         O.Cmds := O.Cmds.Next;
         if (O.Cmds = null) then
            O.Tail := null;
         end if;
      end if;
   end;

   procedure Remote_Do_Changed(H      : in out Test_Option_Change_Handler;
                               Option : in out Telnet_Base_Option'Class;
                               P      : in out Telnet_Processor'Class;
                               Val    : in Boolean)
   is
      Expected : Expected_Option_Change_Ptr;
   begin
      Get_Next_Command(H, Expected);
      if (Expected.Op /= Do_Change) then
         raise Constraint_Error;
      end if;
      if (Expected.Val /= Val) then
         raise Constraint_Error;
      end if;
   end Remote_Do_Changed;

   procedure Remote_Will_Changed(H      : in out Test_Option_Change_Handler;
                                 Option : in out Telnet_Base_Option'Class;
                                 P      : in out Telnet_Processor'Class;
                                 Val    : in Boolean)
   is
      Expected : Expected_Option_Change_Ptr;
   begin
      Get_Next_Command(H, Expected);
      if (Expected.Op /= Will_Change) then
         raise Constraint_Error;
      end if;
      if (Expected.Val /= Val) then
         raise Constraint_Error;
      end if;
   end Remote_Will_Changed;

   procedure Remote_Params_Changed
     (H      : in out Test_Option_Change_Handler;
      Option : in out Telnet_Base_Option'Class;
      P      : in out Telnet_Processor'Class;
      Old    : in Ada.Streams.Stream_Element_Array;
      Params : in Ada.Streams.Stream_Element_Array)
   is
      Expected : Expected_Option_Change_Ptr;
   begin
      Get_Next_Command(H, Expected);
      if (Expected.Op /= Remote_Params_Change) then
         raise Constraint_Error;
      end if;

      if ((Params'Length /= Expected.Data'Length)
          or else (Params /= Expected.Data))
      then
         Put_Line("Remote params data wrong");
         Put("Was:");
         for I in Params'Range loop
            Put(" ");
            Put(I);
            Put(":");
            Put(Params(I));
         end loop;
         New_Line;
         Put("Exp:");
         for I in Expected.Data'Range loop
            Put(" ");
            Put(I);
            Put(":");
            Put(Expected.Data(I));
         end loop;
         New_Line;
         Put("old:");
         for I in Old'Range loop
            Put(" ");
            Put(I);
            Put(":");
            Put(Old(I));
         end loop;
         New_Line;
         raise Constraint_Error;
      end if;
      if (Expected.Old /= Old) then
         raise Constraint_Error;
      end if;
   end Remote_Params_Changed;

   procedure Add_Expected_Command(O    : in out Test_Option_Change_Handler;
                                  Op   : in Expected_Option_Change_Ops;
                                  Data : in Ada.Streams.Stream_Element_Array;
                                  Old  : in Ada.Streams.Stream_Element_Array;
                                  Val  : in Boolean)
   is
      Cmd : Expected_Option_Change_Ptr;
   begin
      Cmd := new Expected_Option_Change(Data'Length, Old'Length);
      Cmd.Op   := Op;
      Cmd.Data := Data;
      Cmd.Old  := Old;
      Cmd.Val  := Val;
      if (O.Tail = null) then
         O.Cmds := Cmd;
         O.Tail := Cmd;
      else
         O.Tail.Next := Cmd;
         O.Tail := Cmd;
      end if;
   end Add_Expected_Command;

   procedure Check_Empty(Name : String;
                         O    : in out Test_Option_Change_Handler) is
   begin
      if (O.Cmds /= null) then
         Put_Line(Name & " had non-empty telnet option change handler");
         raise Constraint_Error;
      end if;
   end Check_Empty;

end Test_Telnet_Handlers;
