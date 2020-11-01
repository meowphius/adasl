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

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

package body Asl.Protocol.Telnet is

   procedure Free is
     new Ada.Unchecked_Deallocation(Ada.Streams.Stream_Element_Array,
                                    Telnet_Params_Ptr);

   procedure Send_Params(P      : in out Telnet_Processor;
                         Option : in Telnet_Option'Class;
                         Data   : in Ada.Streams.Stream_Element_Array)
   is
      Processed_Data : Ada.Streams.Stream_Element_Array
                          (1 .. (Data'Length * 2) + 4);
      Next : Ada.Streams.Stream_Element_Offset;
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Process_Output(P, Data, Processed_Data(4 .. Processed_Data'Last-2),
                     Next, Last);
      Processed_Data(1) := IAC;
      Processed_Data(2) := SB;
      Processed_Data(3) := Option.Option_Code;
      Last := Last + 1;
      Processed_Data(Last) := IAC;
      Last := Last + 1;
      Processed_Data(Last) := SE;
      Telnet_Output(P.Out_Handler.all, P, Processed_Data(1 .. Last));
   end Send_Params;

   procedure Send_Command(P   : in out Telnet_Processor;
                          Cmd : Ada.Streams.Stream_Element) is
   begin
      Telnet_Output(P.Out_Handler.all, P, (IAC, Cmd));
   end Send_Command;

   procedure Send_Command(P    : in out Telnet_Processor;
                          Cmd  : Ada.Streams.Stream_Element;
                          Parm : Ada.Streams.Stream_Element) is
   begin
      Telnet_Output(P.Out_Handler.all, P, (IAC, Cmd, Parm));
   end Send_Command;

   function Get_Processor(Option : in Telnet_Option)
                          return Telnet_Processor_Class is
   begin
      return Option.Processor;
   end;

   function Find_Option
     (P           : in Telnet_Processor'Class;
      Option_Code : in Ada.Streams.Stream_Element)
      return Telnet_Option_Class
   is
      Curr : Telnet_Option_Class := P.Options;
   begin
      while (Curr /= null) loop
         exit when (Curr.Option_code = Option_Code);
         Curr := Curr.Next;
      end loop;

      return Curr;
   end;

   procedure Initialize(P       : access Telnet_Processor;
                        Handler : in Telnet_Command_Handler_Class;
                        Output  : in Telnet_Output_Handler_Class) is
   begin
      P.Self_Ptr    := Telnet_Processor_Class(P);
      P.Cmd_Handler := Handler;
      P.Out_Handler := Output;
   end Initialize;

   procedure Destroy(P : in out Telnet_Processor) is
      Option : Telnet_Option_Class := P.Options;
      Next_Option : Telnet_Option_Class;
   begin
      while (Option /= null) loop
         Next_Option := Option.Next;
         Destroy(Option.all, Option);
      end loop;
      if (P.Working_Params /= null) then
         Free(P.Working_Params);
      end if;
   end Destroy;

   procedure Destroy(Option     : in out Telnet_Option;
                     Option_Ptr : in out Telnet_Option_Class) is
   begin
      Option.Local_Params.Destroy;
      Option.Remote_Params.Destroy;
   end Destroy;

   procedure Process_Output
     (P        : in out Telnet_Processor;
      Data     : in Ada.Streams.Stream_Element_Array;
      Output   : out Ada.Streams.Stream_Element_Array;
      Next_In  : out Ada.Streams.Stream_Element_Offset;
      Last_Out : out Ada.Streams.Stream_Element_Offset)
   is
      I, J : Ada.Streams.Stream_Element_Offset;
   begin
      J := Output'First;
      I := Data'First;
      loop
         exit when (I > Data'Last);
         if P.Last_Out_Was_CR and (Data(I) /= LF) then
            exit when (J > Output'Last);
            Output(J) := NUL;
         end if;
         if (Data(I) = IAC) then
            -- We need two output character to hold the output characters,
            -- so we need a different calculation when outputting hex FF.
            exit when (J > (Output'Last - 1));
            Output(J) := IAC;
            J := J + 1;
            Output(J) := IAC;
            P.Last_Out_Was_CR := False;
         else
            -- Exit the loop if we cannot put the character into the
            -- output array.
            exit when (J > Output'Last);
            Output(J) := Data(I);
            if (Data(I) = CR) then
               P.Last_Out_Was_CR := True;
            else
               P.Last_Out_Was_CR := False;
            end if;
         end if;

         J := J + 1;
         I := I + 1;
      end loop;
      Next_In := I;
      Last_Out := J - 1;
   end Process_Output;

   procedure Handle_Command (P      : in out Telnet_Processor;
                             Length : in Ada.Streams.Stream_Element_Offset) is
   begin
      case P.Cmd(1) is
         when Will_Option | Wont_Option | Do_Option | Dont_Option =>
            if (Length /= 2) then
               return; -- Length is invalid
            end if;
            declare
               Option : Telnet_Option_Class := Find_Option(P, P.Cmd(2));
            begin
               if (Option /= null) then
                  case P.Cmd(1) is
                     when Will_Option => Handle_Will(Option.all, P);
                     when Wont_Option => Handle_Wont(Option.all, P);
                     when Do_Option   => Handle_Do(Option.all, P);
                     when Dont_Option => Handle_Dont(Option.all, P);
                     when others => null;
                  end case;
               else
                  -- Getting a will or a do for an option we don't have
                  -- will cause a don't or a won't to be returned.
                  case P.Cmd(1) is
                     when Will_Option | Wont_Option =>
                        Send_Command(P, Dont_Option, P.Cmd(2));
                     when Do_Option | Dont_Option   =>
                        Send_Command(P, Wont_Option, P.Cmd(2));
                     when others => null;
                  end case;
               end if;
            end;
         when others =>
            if (P.Cmd_Handler /= null) then
               Handle_Telnet_Command(P.Cmd_Handler.all, P, P.Cmd(1 .. Length));
            end if;
      end case;
   end;

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

   procedure Process_Input
     (P        : in out Telnet_Processor;
      Data     : in Ada.Streams.Stream_Element_Array;
      Output   : out Ada.Streams.Stream_Element_Array;
      Next_In  : out Ada.Streams.Stream_Element_Offset;
      Last_Out : out Ada.Streams.Stream_Element_Offset)
   is
      I, J : Ada.Streams.Stream_Element_Offset;
   begin
      J := Output'First;
      I := Data'First;
      loop
         exit when (I > Data'Last);
         case P.State is
            when Normal =>
               if (Data(I) = IAC) then
                  P.State := Cmd1;
               elsif P.Last_In_Was_CR and (Data(I) = NUL) then
                  -- Ignore a NUL after a CR.
                  P.Last_In_Was_CR := False;
               else
                  -- Exit the loop if we cannot put the character into the
                  -- output array.
                  exit when (J > Output'Last);
                  Output(J) := Data(I);
                  if (Data(I) = CR) then
                     P.Last_In_Was_CR := True;
                  else
                     P.Last_In_Was_CR := False;
                  end if;
                  J := J + 1;
               end if;

            when Cmd1 =>
               if (Data(I) = IAC) then
                  exit when (J > Output'Last);
                  Output(J) := Data(I);
                  J := J + 1;
                  P.State := Normal;
               elsif (Data(I) = SB) then
                  P.State := Subneg_Option;
               elsif (Data(I) = SE) then
                  null; -- ignore this one.
               elsif (Data(I) < SB) then
                  P.Cmd(1) := Data(I);
                  P.State := Normal;
                  Handle_Command(P, 1);
               else
                  P.Cmd(1) := Data(I);
                  P.State := Cmd2;
               end if;

            when Cmd2 =>
               P.Cmd(2) := Data(I);
               P.State := Normal;
               Handle_Command(P, 2);

            when Subneg_Option =>
               -- Get the option.  Note that this may return null.
               P.Working_Option := Find_Option(P, Data(I));
               P.Params_Length := 0;
               P.State := Subneg_Params;

            when Subneg_Params =>
               if (Data(I) = IAC) then
                  P.State := Subneg_Cmd1;
               else
                  Append_Param(P.Working_Params, P.Params_Length, Data(I));
               end if;

            when Subneg_Cmd1 =>
               if (Data(I) = IAC) then
                  Append_Param(P.Working_Params, P.Params_Length, IAC);
                  P.State := Subneg_Params;
               elsif (Data(I) = SB) then
                  null; -- Just ignore this.
               elsif (Data(I) = SE) then
                  if (P.Working_Option = null) then
                     Free(P.Working_Params);
                  else
                     New_Remote_Params
                       (P.Working_Option.all,
                        P,
                        P.Working_Params(1 .. P.Params_Length));
                     Free(P.Working_Params);
                  end if;
                  P.State := Normal;
               elsif (Data(I) < SB) then
                  P.Cmd(1) := Data(I);
                  P.State := Subneg_Params;
                  Handle_Command(P, 1);
               else
                  P.Cmd(1) := Data(I);
                  P.State := Subneg_Cmd2;
               end if;

            when Subneg_Cmd2 =>
               P.Cmd(2) := Data(I);
               P.State := Subneg_Params;
               Handle_Command(P, 2);
         end case;
         I := I + 1;
      end loop;
      Next_In := I;
      Last_Out := J - 1;
   end Process_Input;

   procedure Register_Option
     (P           : in out Telnet_Processor;
      Option_Code : in Ada.Streams.Stream_Element;
      Option      : in Telnet_Option_Class)
   is
      Curr : Telnet_Option_Class := Find_Option(P, Option_Code);
   begin
      if (Option.Registered or (Curr /= null)) then
         raise Constraint_Error;
      end if;

      Option.Option_Code := Option_Code;
      Option.Next := P.Options;
      P.Options := Option;
      Option.Processor := P.Self_Ptr;
      Option.Registered := True;
      Registered(Option.all, P.Self_Ptr);
   end Register_Option;

   function Get_Option_Code(Option : in Telnet_Option'Class)
                            return Ada.Streams.Stream_Element is
   begin
      return Option.Option_code;
   end Get_Option_Code;

   procedure Set_Local_Param_Array(Option : in out Telnet_Option'Class;
                                   Params : Ada.Streams.Stream_Element_Array)
   is
   begin
      Option.Local_Params.Set(Params);
   end Set_Local_Param_Array;

   procedure Set_Remote_Param_Array(Option : in out Telnet_Option'Class;
                                    Params : Ada.Streams.Stream_Element_Array)
   is
   begin
      Option.Remote_Params.Set(Params);
   end Set_Remote_Param_Array;

   function Get_Local_Param_Array(Option : in Telnet_Option'Class)
                                  return Ada.Streams.Stream_Element_Array is
   begin
      return Option.Local_Params.Get;
   end Get_Local_Param_Array;

   function Get_Remote_Param_Array(Option : in Telnet_Option'Class)
                                   return Ada.Streams.Stream_Element_Array is
   begin
      return Option.Remote_Params.Get;
   end Get_Remote_Param_Array;

   protected body Param_Holder is
      procedure Set(New_Params : Ada.Streams.Stream_Element_Array) is
         Old : Telnet_Params_Ptr := Params;
      begin
         Params := new Ada.Streams.Stream_Element_Array'(New_Params);
         Free(Old);
      end Set;

      function Get return Ada.Streams.Stream_Element_Array is
      begin
         return Params.all;
      end Get;

      procedure Destroy is
      begin
         Free(Params);
      end Destroy;
   end Param_Holder;
end Asl.Protocol.Telnet;
