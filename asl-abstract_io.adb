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

with Ada.Characters.Latin_1;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Characters.Handling;

with Ada.Text_IO;

package body Asl.Abstract_IO is

   type Character_Array is array (Integer  range <>) of Character;

   Conv_Chars : Character_Array(0 .. 15) := "0123456789ABCDEF";

   Space_Chars : String := " " & Ada.Characters.Latin_1.HT;

   function Is_Space(Ch : Character) return Boolean is
   begin
      for I in Space_Chars'Range loop
         if (Ch = Space_Chars(I)) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Space;


   procedure Put_Wrapped(File         : in out Abstract_File;
                         Item         : in String;
                         Left_Indent  : in Natural := 0;
                         Right_Indent : in Natural := 0) is
      Column  : Integer := Next_Column(Abstract_File'Class(File));
      Width   : Integer := IO_Width(Abstract_File'Class(File));
      Pos     : Integer := Item'First;
      Last    : Integer;
      Column_Start : Boolean := False;
   begin
      while (Pos <= Item'Last) loop
         if (Column = 1) then
            if (Left_Indent /= 0) then
               for I in 1 .. Left_Indent loop
                  Put(Abstract_File'Class(File), ' ');
                  Column := Column + 1;
               end loop;
            end if;
            -- Ignore spaces that would come at the beginning of a line.
            while (Pos <= Item'Last) and then Is_Space(Item(Pos)) loop
               Pos := Pos + 1;
            end loop;
            Column_Start := True;
         end if;

         Last := Pos + (Width - Column + 1) - Right_Indent;
         if (Last > Item'Last) then
            Last := Item'Last;
            while (Last > Pos) and then Is_Space(Item(Last)) loop
               Last := Last - 1;
            end loop;
            -- If it's all spaces, then leave the loop;
            exit when Last = Pos;
         else
            while (Last > Pos) and then not Is_Space(Item(Last)) loop
               Last := Last - 1;
            end loop;
            while (Last > Pos) and then Is_Space(Item(Last)) loop
               Last := Last - 1;
            end loop;
         end if;
         if (Last = Pos) then
            -- The current word will not fit onto the current line, so move
            -- to a new line.  Note that if the current word won't fit onto
            -- a whole line, we still put it out.
            if (Column_Start) then
               Last := Pos + (Width - Column) - Right_Indent;
               if (Last > Item'Last) then
                  Last := Item'Last;
               end if;
               while (Last <= Item'Last) and then not Is_Space(Item(Last)) loop
                  Last := Last + 1;
               end loop;
               Put(Abstract_File'Class(File), Item(Pos .. Last-1));
               Column := Column + (Last - Pos);
               Column_Start := False;
               Pos := Last;
               -- Ignore spaces that would come at the beginning of a line.
               while (Pos <= Item'Last) and then Is_Space(Item(Pos)) loop
                  Pos := Pos + 1;
               end loop;
            else
               New_Line(Abstract_File'Class(File));
               Column := 1;
            end if;
         else
            Put(Abstract_File'Class(File), Item(Pos .. Last));
            Column_Start := False;
            Column := Column + (Last - Pos + 1);
            Pos := Last + 1;
         end if;
      end loop;
   end Put_Wrapped;

   procedure Load_Char(File : in out Abstract_File;
                       Ch   : out Character;
                       Left : in out Integer;
                       Done : in out Boolean) is
   begin
      if Done then
         return;
      end if;

      if (Left = 0) then
         Done := True;
         return;
      end if;

      Get(Abstract_File'Class(File), Ch);

      if End_Of_Line(Abstract_File'Class(File), Ch) then
         Putback_Char(Abstract_File'Class(File), Ch);
         Done := True;
         return;
      end if;

      Left := Left - 1;
   end Load_Char;

   package body Integer_IO is

      procedure Char_To_Digit(Ch   : in Character;
                              Base : in Number_Base;
                              Val  : out Num;
                              Done : in out Boolean) is
      begin
         for I in 0 .. Base-1 loop
            if (Ada.Characters.Handling.To_Upper(Ch) = Conv_Chars(I)) then
               Val := Num(I);
               return;
            end if;
         end loop;

         Done := True;
      end Char_To_Digit;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in Field := 0;
                    Base  : in Number_Base := Default_Base) is
         Val        : Num := 0;
         Ch         : Character;
         Done       : Boolean := False;
         Left       : Integer;
         Negative   : Boolean := False;
         Tmp        : Num;
      begin
         if (Width = 0) then
            Left := -1;
         else
            Left := Width;
         end if;

         Load_Char(File, Ch, Left, Done);
         while not Done and then Is_Space(Ch) loop
            Load_Char(File, Ch, Left, Done);
         end loop;

         if Done then
            raise Data_Error;
         end if;

         if (Ch = '+') then
            Load_Char(File, Ch, Left, Done);
         elsif (Ch = '-') then
            Negative := True;
            Load_Char(File, Ch, Left, Done);
         end if;

         if Done then
            raise Data_Error;
         end if;

         Char_To_Digit(Ch, Base, Val, Done);
         if (Done) then
            raise Data_Error;
         end if;

         Load_Char(File, Ch, Left, Done);
         while not Done loop
            Char_To_Digit(Ch, Base, Tmp, Done);
            if Done then
               Done := False;
               if (Left < 0) then
                  -- We only put back characters if Width was zero, othersize
                  -- we read the whole field.
                  Putback_Char(Abstract_File'Class(File), Ch);
               end if;
               exit;
            end if;
            Val := (Val * Num(Base)) + Tmp;
            Load_Char(File, Ch, Left, Done);
         end loop;

         while not Done and (Left >= 0) loop
            if not Is_Space(Ch) then
               raise Data_Error;
            end if;
            Load_Char(File, Ch, Left, Done);
         end loop;

         if (Negative) then
            Item := -Val;
         else
            Item := Val;
         end if;
      end Get;

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Width   : in Field := Default_Width;
                    Base    : in Number_Base := Default_Base;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "") is
         Strval : String := Num'Image(Item);
         Fill   : String(1 .. Width);
         Fill_Count : Integer := Width - Strval'Length;
         Start  : Integer := Strval'First;
      begin
         if (Strval(Start) = ' ') then
            Start := Start + 1;
            Fill_Count := Fill_Count + 1;
         end if;

         if (Fill_Count < 0) then
            Fill_Count := 0;
         end if;
         if Fill_Count > 0 then
            for I in 1 .. Fill_Count loop
               Fill(I) := ' ';
            end loop;
         end if;
         if Wrapped then
            Put_Wrapped(Abstract_File'Class(File),
                        (Prefix & Fill(1 .. Fill_Count)
                         & Strval(Start .. Strval'Last)),
                        Left_Indent => Left_Indent,
                        Right_Indent => Right_Indent);
         else
            Put(Abstract_File'Class(File),
                Prefix & Fill(1 .. Fill_Count) & Strval(Start .. Strval'Last));
         end if;
      end Put;

   end Integer_IO;

   package body Modular_IO is

      procedure Char_To_Digit(Ch   : in Character;
                              Base : in Number_Base;
                              Val  : out Num;
                              Done : in out Boolean) is
      begin
         for I in 0 .. Base-1 loop
            if (Ada.Characters.Handling.To_Upper(Ch) = Conv_Chars(I)) then
               Val := Num(I);
               return;
            end if;
         end loop;

         Done := True;
      end Char_To_Digit;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in Field := 0;
                    Base  : in Number_Base := Default_Base) is
         Val        : Num := 0;
         Ch         : Character;
         Done       : Boolean := False;
         Left       : Integer;
         Tmp        : Num;
      begin
         if (Width = 0) then
            Left := -1;
         else
            Left := Width;
         end if;

         Load_Char(File, Ch, Left, Done);
         while not Done and then Is_Space(Ch) loop
            Load_Char(File, Ch, Left, Done);
         end loop;

         if Done then
            raise Data_Error;
         end if;

         Char_To_Digit(Ch, Base, Val, Done);
         if (Done) then
            raise Data_Error;
         end if;

         Load_Char(File, Ch, Left, Done);
         while not Done loop
            Char_To_Digit(Ch, Base, Tmp, Done);
            if Done then
               Done := False;
               if (Left < 0) then
                  -- We only put back characters if Width was zero, othersize
                  -- we read the whole field.
                  Putback_Char(Abstract_File'Class(File), Ch);
               end if;
               exit;
            end if;
            Val := (Val * Num(Base)) + Tmp;
            Load_Char(File, Ch, Left, Done);
         end loop;

         while not Done and (Left >= 0) loop
            if not Is_Space(Ch) then
               raise Data_Error;
            end if;
            Load_Char(File, Ch, Left, Done);
         end loop;

         Item := Val;
      end Get;

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Width   : in Field := Default_Width;
                    Base    : in Number_Base := Default_Base;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "") is
         Strval : String := Num'Image(Item);
         Fill   : String(1 .. Width);
         Fill_Count : Integer := Width - Strval'Length;
         Start  : Integer := Strval'First;
      begin
         if (Strval(Start) = ' ') then
            Start := Start + 1;
            Fill_Count := Fill_Count - 1;
         end if;

         if (Fill_Count < 0) then
            Fill_Count := 0;
         end if;
         if Fill_Count > 0 then
            for I in 1 .. Fill_Count loop
               Fill(I) := ' ';
            end loop;
         end if;
         if Wrapped then
            Put_Wrapped(Abstract_File'Class(File),
                        (Prefix & Fill(1 .. Fill_Count) &
                         Strval(Start .. Strval'Last)),
                        Left_Indent => Left_Indent,
                        Right_Indent => Right_Indent);
         else
            Put(Abstract_File'Class(File),
                Prefix & Fill(1 .. Fill_Count) & Strval(Start .. Strval'Last));
         end if;
      end Put;

   end Modular_IO;

   procedure Char_To_Digit(Ch   : in Character;
                           Base : in Number_Base;
                           Val  : out Integer;
                           Done : in out Boolean) is
   begin
      for I in 0 .. Base-1 loop
         if (Ada.Characters.Handling.To_Upper(Ch) = Conv_Chars(I)) then
            Val := I;
            return;
         end if;
      end loop;

      Done := True;
   end Char_To_Digit;

   procedure Do_Out(File    : in out Abstract_File;
                    Output  : in out String;
                    Out_Pos : in out Integer;
                    Ch      : in Character) is
   begin
      Output(Out_Pos) := Ch;

      if (Out_Pos = Output'Last) then
         Put(Abstract_File'Class(File), Output);
         Out_Pos := Output'First;
      else
         Out_Pos := Out_Pos + 1;
      end if;
   end Do_Out;

   procedure Round_Up(File    : in out Abstract_File;
                      Buffer  : in out String;
                      Out_Pos : in out Integer) is
      Pos    : Integer := Out_Pos - 1;
      Carry  : Boolean := True;
      Old_Ch : Character;
   begin
      while ((Pos >= Buffer'First)
             and then (((Buffer(Pos) >= '0') and (Buffer(Pos) <= '9'))
                       or (Buffer(Pos) = '.')))
      loop
         if (Buffer(Pos) /= '.') then
            if (Buffer(Pos) = '9') then
               Buffer(Pos) := '0';
            else
               Buffer(Pos) := Character'Succ(Buffer(Pos));
               Carry := False;
               exit;
            end if;
         end if;
         Pos := Pos - 1;
      end loop;

      if Carry then
         -- We have to add a '1' in front of the whole thing.
         if (Out_Pos-1 < Buffer'First) then
            Do_Out(File, Buffer, Out_Pos, '1');
         else
            -- Store the one we need to re-append onto the end.
            Old_Ch := Buffer(Out_Pos-1);
            for I in reverse Out_Pos - 2 .. Pos loop
               Buffer(I+1) := Buffer(I);
            end loop;
            Buffer(Pos) := '1';
            Do_Out(File, Buffer, Out_Pos, Old_Ch);
         end if;
      end if;
   end Round_Up;

   package body Float_IO is

      procedure Out_After_Dec(File    : in out Abstract_File;
                              Output  : in out String;
                              Out_Pos : in out Integer;
                              Strval  : in String;
                              Start   : in out Integer;
                              Left    : in Integer) is
         Ch : Character;
      begin
         if (Strval(Start) /= 'E') then
            Ch := Strval(Start);
            Start := Start + 1;
            if (Left = 0) and (Strval(Start) /= 'E') and (Strval(Start) >= '5')
            then
               -- Do rounding.
               -- This assumes all characters are contiguous.
               if (Ch = '9') then
                  Round_Up(File, Output, Out_Pos);
                  Ch := '0';
               else
                  Ch := Character'Succ(Ch);
               end if;
            end if;
            Do_Out(File, Output, Out_Pos, Ch);
         else
            Do_Out(File, Output, Out_Pos, '0');
         end if;
      end Out_After_Dec;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in  Field := 0) is
         Val        : Num := 0.0;
         Val2       : Num := 0.1;
         Val3       : Integer := 0;
         Ch         : Character;
         Done       : Boolean := False;
         Left       : Integer;
         Negative   : Boolean := False;
         Tmp        : Integer;
         Saw_Dot    : Boolean := False;
         Saw_Exp    : Boolean := False;
         Need_Digit_After_Dot : Boolean := False;
      begin
         if (Width = 0) then
            Left := -1;
         else
            Left := Width;
         end if;

         Load_Char(File, Ch, Left, Done);
         while not Done and then Is_Space(Ch) loop
            Load_Char(File, Ch, Left, Done);
         end loop;

         if Done then
            raise Data_Error;
         end if;

         if (Ch = '+') then
            Load_Char(File, Ch, Left, Done);
         elsif (Ch = '-') then
            Negative := True;
            Load_Char(File, Ch, Left, Done);
         end if;

         if Done then
            raise Data_Error;
         end if;

         if (Ch = '.') then
            Load_Char(File, Ch, Left, Done);
            Saw_Dot := True;
            Need_Digit_After_Dot := True;
         else
            Char_To_Digit(Ch, 10, Tmp, Done);
            if (Done) then
               raise Data_Error;
            end if;

            Val := (Val * 10.0) + Num(Tmp);
            Load_Char(File, Ch, Left, Done);
            while not Done loop
               if (Ch = '.') then
                  Load_Char(File, Ch, Left, Done);
                  Saw_Dot := True;
                  exit;
               end if;
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  Done := False;
                  if (Left < 0) then
                     -- We only put back characters if Width was zero,
                     -- othersize we read the whole field.
                     Putback_Char(Abstract_File'Class(File), Ch);
                  end if;
                  exit;
               end if;
               Val := (Val * 10.0) + Num(Tmp);
               Load_Char(File, Ch, Left, Done);
            end loop;
         end if;

         if not Done and Saw_Dot then
            if (Need_Digit_After_Dot) then
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  raise Data_Error;
               end if;
               Val := Val + (Val2 * Num(Tmp));
               Val2 := Val2 / 10.0;
               Load_Char(File, Ch, Left, Done);
            end if;
            while not Done loop
               if (Ch = 'E') or (Ch = 'e') then
                  Saw_Exp := True;
                  Load_Char(File, Ch, Left, Done);
                  exit;
               end if;
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  Done := False;
                  if (Left < 0) then
                     -- We only put back characters if Width was zero,
                     -- othersize we read the whole field.
                     Putback_Char(Abstract_File'Class(File), Ch);
                  end if;
                  exit;
               end if;
               Val := Val + (Val2 * Num(Tmp));
               Val2 := Val2 / 10.0;
               Load_Char(File, Ch, Left, Done);
            end loop;
         end if;

         if Negative then
            Val := - Val;
         end if;
         Negative := False;

         if Saw_Exp then
            if Done then
               raise Data_Error;
            end if;

            if (Ch = '+') then
               Load_Char(File, Ch, Left, Done);
            elsif (Ch = '-') then
               Negative := True;
               Load_Char(File, Ch, Left, Done);
            end if;

            if Done then
               raise Data_Error;
            end if;

            Char_To_Digit(Ch, 10, Val3, Done);
            if Done then
               raise Data_Error;
            end if;
            Load_Char(File, Ch, Left, Done);

            while not Done loop
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  Done := False;
                  if (Left < 0) then
                     -- We only put back characters if Width was zero,
                     -- otherwise we read the whole field.
                     Putback_Char(Abstract_File'Class(File), Ch);
                  end if;
                  exit;
               end if;
               Val3 := (Val3 * 10) + Tmp;
               Load_Char(File, Ch, Left, Done);
            end loop;

            if Negative then
               Val3 := -Val3;
            end if;

            Val := Val * (10.0 ** Val3);
         end if;

         while not Done and (Left >= 0) loop
            if not Is_Space(Ch) then
               raise Data_Error;
            end if;
            Load_Char(File, Ch, Left, Done);
         end loop;

         Item := Val;
      end Get;

      -- FIXME - the prefix does not work for things longer than a buffer.
      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Fore    : in Field := Default_Fore;
                    Aft     : in Field := Default_Aft;
                    Exp     : in Field := Default_Exp;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "") is
         Strval : String := Num'Image(Item);
         Start  : Integer;
         Last   : Integer;
         Shift  : Integer := 0;
         Output : String(1 .. 256);
         Out_Pos : Integer := Output'First;
         Curr_Count : Integer := 2;
         Aft_Left : Integer;
         Exp_Left : Integer;
      begin
         Start := Strval'First;
         if (Strval(Start) = ' ') then
            Start := Start + 1;
            Curr_Count := 1;
         end if;

         -- If exp is zero, then we don't have an exponent and we might
         -- need to do a lot of shifting.
         if (Exp = 0) then
            Last := Start;
            while (Strval(Last) /= 'E') loop
               Last := Last + 1;
            end loop;
            Last := Last + 1;
            declare
               Tmp  : Integer;
               Done : Boolean := False;
               Neg  : Boolean := False;
            begin
               if (Strval(Last) = '-') then
                  Last := Last + 1;
                  Neg := True;
               elsif (Strval(Last) = '+') then
                  Last := Last + 1;
               end if;
               while (Last <= Strval'Last) loop
                  Char_To_Digit(Strval(Last), 10, Tmp, Done);
                  Shift := (Shift * 10) + Tmp;
                  Last := Last + 1;
               end loop;
               if (Neg) then
                  Shift := - Shift;
               end if;
            end;

            if (Shift >= 0) then
               for I in Curr_Count + Shift .. Fore-1 loop
                  Do_Out(File, Output, Out_Pos, ' ');
               end loop;
               while (Curr_Count > 0) loop
                  Do_Out(File, Output, Out_Pos, Strval(Start));
                  Start := Start + 1;
                  Curr_Count := Curr_Count - 1;
               end loop;
               Start := Start + 1; -- skip '.'
               while (Shift > 0) loop
                  Shift := Shift - 1;
                  Out_After_Dec(File, Output, Out_Pos, Strval, Start, Shift);
               end loop;
               Do_Out(File, Output, Out_Pos, '.');
               Aft_Left := Aft;
               if (Aft_Left = 0) then
                  Out_After_Dec(File, Output, Out_Pos, Strval, Start, 0);
               else
                  while (Aft_Left > 0) loop
                     Aft_Left := Aft_Left - 1;
                     Out_After_Dec(File, Output, Out_Pos, Strval,
                                   Start, Aft_Left);
                  end loop;
               end if;
            elsif (Shift < 0) then
               for I in Curr_Count .. Fore loop
                  Do_Out(File, Output, Out_Pos, ' ');
               end loop;
               if (Curr_Count = 2) then
                  Do_Out(File, Output, Out_Pos, Strval(Start));
                  Start := Start + 1;
               end if;
               Do_Out(File, Output, Out_Pos, '0');
               Do_Out(File, Output, Out_Pos, '.');
               Shift := Shift + 1;
               Aft_Left := Aft;
               if (Aft_Left = 0) then
                  Aft_Left := 1;
               end if;
               while (Aft_Left > 0) and (Shift < 0) loop
                  Do_Out(File, Output, Out_Pos, '0');
                  Shift := Shift + 1;
                  Aft_Left := Aft_Left - 1;
               end loop;
               while (Aft_Left > 0) loop
                  if (Strval(Start) /= '.') then
                     Aft_Left := Aft_Left - 1;
                     Out_After_Dec(File, Output, Out_Pos, Strval,
                                   Start, Aft_Left);
                  else
                     Start := Start + 1;
                  end if;
               end loop;
            end if;
         else
            for I in Curr_Count + Shift .. (Fore-1) loop
               Do_Out(File, Output, Out_Pos, ' ');
            end loop;
            while (Curr_Count > 0) loop
               Do_Out(File, Output, Out_Pos, Strval(Start));
               Start := Start + 1;
               Curr_Count := Curr_Count - 1;
            end loop;
            Do_Out(File, Output, Out_Pos, Strval(Start));
            Start := Start + 1;
            Aft_Left := Aft;
            if (Aft_Left = 0) then
               Out_After_Dec(File, Output, Out_Pos, Strval, Start, 0);
            else
               while (Aft_Left > 0) loop
                  Aft_Left := Aft_Left - 1;
                  Out_After_Dec(File, Output, Out_Pos, Strval,
                                Start, Aft_Left);
               end loop;
            end if;
            while (Strval(Start) /= 'E') loop
               Start := Start + 1;
            end loop;
            -- put the 'E+/-' out
            Do_Out(File, Output, Out_Pos, Strval(Start));
            Start := Start + 1;
            Do_Out(File, Output, Out_Pos, Strval(Start));
            Start := Start + 1;
            Exp_Left := Exp - 1;
            while (Exp_Left > (Strval'Last - Start + 1)) loop
               Do_Out(File, Output, Out_Pos, '0');
               Exp_Left := Exp_Left - 1;
            end loop;
            while (Start <= Strval'Last) loop
               Do_Out(File, Output, Out_Pos, Strval(Start));
               Start := Start + 1;
            end loop;
         end if;

         if (Out_Pos /= 1) then
            -- We only do wrapped I/O on the last output.  If the number is
            -- larger than the buffer, wrapped I/O is rather meaningless.
            if Wrapped then
               Put_Wrapped(Abstract_File'Class(File),
                           Prefix & Output(1 .. Out_Pos-1),
                           Left_Indent => Left_Indent,
                           Right_Indent => Right_Indent);
            else
               Put(Abstract_File'Class(File), Prefix & Output(1 .. Out_Pos-1));
            end if;
         end if;
      end Put;

   end Float_IO;

   package body Fixed_IO is

      procedure Out_After_Dec(File    : in out Abstract_File;
                              Output  : in out String;
                              Out_Pos : in out Integer;
                              Strval  : in String;
                              Start   : in out Integer;
                              Left    : in Integer) is
         Ch : Character;
      begin
         if (Start <= Strval'Last) then
            Ch := Strval(Start);
            Start := Start + 1;
            if (Ch = '.') then
               -- Just ignore periods.
               return;
            end if;

            if ((Left = 0)
                and then (Start <= Strval'Last)
                and then (Strval(Start) >= '5'))
            then
               -- Do rounding
               -- This assumes all characters are contiguous.
               if (Ch = '9') then
                  Round_Up(File, Output, Out_Pos);
                  Ch := '0';
               else
                  Ch := Character'Succ(Ch);
               end if;
            end if;
            Do_Out(File, Output, Out_Pos, Ch);
         else
            Do_Out(File, Output, Out_Pos, '0');
         end if;
      end Out_After_Dec;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in  Field := 0) is
         Val        : Num := 0.0;
         Val2       : Num := 0.1;
         Val3       : Integer := 0;
         Ch         : Character;
         Done       : Boolean := False;
         Left       : Integer;
         Negative   : Boolean := False;
         Tmp        : Integer;
         Saw_Dot    : Boolean := False;
         Saw_Exp    : Boolean := False;
         Need_Digit_After_Dot : Boolean := False;
      begin
         if (Width = 0) then
            Left := -1;
         else
            Left := Width;
         end if;

         Load_Char(File, Ch, Left, Done);
         while not Done and then Is_Space(Ch) loop
            Load_Char(File, Ch, Left, Done);
         end loop;

         if Done then
            raise Data_Error;
         end if;

         if (Ch = '+') then
            Load_Char(File, Ch, Left, Done);
         elsif (Ch = '-') then
            Negative := True;
            Load_Char(File, Ch, Left, Done);
         end if;

         if Done then
            raise Data_Error;
         end if;

         if (Ch = '.') then
            Load_Char(File, Ch, Left, Done);
            Saw_Dot := True;
            Need_Digit_After_Dot := True;
         else
            Char_To_Digit(Ch, 10, Tmp, Done);
            if (Done) then
               raise Data_Error;
            end if;

            Val := (Val * 10.0) + Num(Tmp);
            Load_Char(File, Ch, Left, Done);
            while not Done loop
               if (Ch = '.') then
                  Load_Char(File, Ch, Left, Done);
                  Saw_Dot := True;
                  exit;
               end if;
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  Done := False;
                  if (Left < 0) then
                     -- We only put back characters if Width was zero,
                     -- othersize we read the whole field.
                     Putback_Char(Abstract_File'Class(File), Ch);
                  end if;
                  exit;
               end if;
               Val := (Val * 10.0) + Num(Tmp);
               Load_Char(File, Ch, Left, Done);
            end loop;
         end if;

         if not Done and Saw_Dot then
            if (Need_Digit_After_Dot) then
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  raise Data_Error;
               end if;
               Val := Val + (Val2 * Num(Tmp));
               Val2 := Val2 / 10.0;
               Load_Char(File, Ch, Left, Done);
            end if;
            while not Done loop
               if (Ch = 'E') or (Ch = 'e') then
                  Saw_Exp := True;
                  Load_Char(File, Ch, Left, Done);
                  exit;
               end if;
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  Done := False;
                  if (Left < 0) then
                     -- We only put back characters if Width was zero,
                     -- othersize we read the whole field.
                     Putback_Char(Abstract_File'Class(File), Ch);
                  end if;
                  exit;
               end if;
               Val := Val + (Val2 * Num(Tmp));
               Val2 := Val2 / 10.0;
               Load_Char(File, Ch, Left, Done);
            end loop;
         end if;

         if Negative then
            Val := - Val;
         end if;
         Negative := False;

         if Saw_Exp then
            if Done then
               raise Data_Error;
            end if;

            if (Ch = '+') then
               Load_Char(File, Ch, Left, Done);
            elsif (Ch = '-') then
               Negative := True;
               Load_Char(File, Ch, Left, Done);
            end if;

            if Done then
               raise Data_Error;
            end if;

            Char_To_Digit(Ch, 10, Val3, Done);
            if Done then
               raise Data_Error;
            end if;
            Load_Char(File, Ch, Left, Done);

            while not Done loop
               Char_To_Digit(Ch, 10, Tmp, Done);
               if Done then
                  Done := False;
                  if (Left < 0) then
                     -- We only put back characters if Width was zero,
                     -- otherwise we read the whole field.
                     Putback_Char(Abstract_File'Class(File), Ch);
                  end if;
                  exit;
               end if;
               Val3 := (Val3 * 10) + Tmp;
               Load_Char(File, Ch, Left, Done);
            end loop;


            -- The following gives GNAT heartburn, so do it another way. It's
            -- slow, but it doesn't crash the compiler.
            -- if Negative then
            --   Val3 := -Val3;
            -- end if;
            -- Val := Val * (10.0 ** Val3);
            if Negative then
               for I in 1 .. Val3 loop
                  Val := Val / 10.0;
               end loop;
            else
               for I in 1 .. Val3 loop
                  Val := Val * 10.0;
               end loop;
            end if;
         end if;

         while not Done and (Left >= 0) loop
            if not Is_Space(Ch) then
               raise Data_Error;
            end if;
            Load_Char(File, Ch, Left, Done);
         end loop;

         Item := Val;
      end Get;

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Fore    : in Field := Default_Fore;
                    Aft     : in Field := Default_Aft;
                    Exp     : in Field := Default_Exp;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "") is
         Strval : String := Num'Image(Item);
         Start  : Integer;
         Shift  : Integer;
         Output : String(1 .. 256);
         Out_Pos : Integer := Output'First;
         Aft_Left : Integer;
         Dec_Pos : Integer;
         Negative : Integer;
      begin
         if (Aft <= 0) then
            Aft_Left := 1;
         else
            Aft_Left := Aft;
         end if;

         Start := Strval'First;
         if (Strval(Start) = '-') then
            Negative := 1;
         else
            Negative := 0;
         end if;
         Start := Start + 1;

         Dec_Pos := Start;
         while (Strval(Dec_Pos) /= '.') loop
            Dec_Pos := Dec_Pos + 1;
         end loop;

         if (Exp = 0) then
            for I in (Dec_Pos - Start - Negative) .. Fore-1 loop
                  Do_Out(File, Output, Out_Pos, ' ');
            end loop;
            if (Negative = 1) then
               Do_Out(File, Output, Out_Pos, '-');
            end if;
            while (Start < Dec_Pos) loop
               Do_Out(File, Output, Out_Pos, Strval(Start));
               Start := Start + 1;
            end loop;
            Do_Out(File, Output, Out_Pos, Strval(Start));
            Start := Start + 1;
            while (Aft_Left > 0) loop
               Aft_Left := Aft_Left - 1;
               Out_After_Dec(File, Output, Out_Pos, Strval, Start, Aft_Left);
            end loop;
         else
            if ((Dec_Pos - Start) = 1) and (Strval(Start) = '0') then
               -- Zero before the decimal, might have to shift left
               Shift := -1;
               Start := Dec_Pos + 1;
               while ((Start <= Strval'Last)
                      and then (Strval(Start) = '0'))
               loop
                  Start := Start + 1;
                  Shift := Shift - 1;
               end loop;
               if (Start > Strval'Last) then
                  -- It's all zero, just set it up for that.
                  Start := Strval'Last;
               end if;
            else
               Shift := Dec_Pos - Start - 1;
            end if;
            for I in Negative .. Fore-1 loop
               Do_Out(File, Output, Out_Pos, ' ');
            end loop;
            if (Negative = 1) then
               Do_Out(File, Output, Out_Pos, '-');
            end if;
            Do_Out(File, Output, Out_Pos, Strval(Start));
            Start := Start + 1;
            Do_Out(File, Output, Out_Pos, '.');
            while (Aft_Left > 0) loop
               Aft_Left := Aft_Left - 1;
               Out_After_Dec(File, Output, Out_Pos, Strval, Start,
                             Aft_Left);
            end loop;
            Do_Out(File, Output, Out_Pos, 'E');
            if (Shift < 0) then
               Do_Out(File, Output, Out_Pos, '-');
               Shift := -Shift;
            else
               Do_Out(File, Output, Out_Pos, '+');
            end if;
            declare
               Exponent : String := Integer'Image(Shift);
            begin
               for I in Exp .. Exponent'Length loop
                  Do_Out(File, Output, Out_Pos, '0');
               end loop;
               for I in Exponent'First+1 .. Exponent'Last loop
                  Do_Out(File, Output, Out_Pos, Exponent(I));
               end loop;
            end;
         end if;

         if (Out_Pos /= 1) then
            -- We only do wrapped I/O on the last output.  If the number is
            -- larger than the buffer, wrapped I/O is rather meaningless.
            if Wrapped then
               Put_Wrapped(Abstract_File'Class(File),
                           Prefix & Output(1 .. Out_Pos-1),
                           Left_Indent => Left_Indent,
                           Right_Indent => Right_Indent);
            else
               Put(Abstract_File'Class(File), Prefix & Output(1 .. Out_Pos-1));
            end if;
         end if;
      end Put;

   end Fixed_IO;

--     package body Decimal_IO is

--        procedure Get(File  : in out Abstract_File;
--                      Item  : out Num;
--                      Width : in  Field := 0) is
--        begin
--           null;
--        end Get;

--        procedure Put(File    : in out Abstract_File;
--                      Item    : in Num;
--                      Fore    : in Field := Default_Fore;
--                      Aft     : in Field := Default_Aft;
--                      Exp     : in Field := Default_Exp;
--                      Wrapped : in Boolean := False;
--                      Left_Indent  : in Natural := 0;
--                      Right_Indent : in Natural := 0;
--                      Prefix  : in String := "") is
--        begin
--           null;
--        end Put;

--     end Decimal_IO;

   package body Enumeration_IO is

      Max_Width : constant Integer := Enum'Width;

      procedure Get(File : in out Abstract_File;
                    Item : out Enum) is
         Ch     : Character;
         Done   : Boolean := False;
         Left   : Integer := -1;
         Strval : String(1 .. Max_Width);
         Pos    : Integer;
      begin
         Load_Char(File, Ch, Left, Done);
         while not Done and then Is_Space(Ch) loop
            Load_Char(File, Ch, Left, Done);
         end loop;

         if Done then
            raise Data_Error;
         end if;

         Pos := Strval'First;
         while not Done and then not Is_Space(Ch) loop
            if (Pos > Strval'Last) then
               raise Data_Error;
            end if;
            Strval(Pos) := Ch;
            Pos := Pos + 1;
            Load_Char(File, Ch, Left, Done);
         end loop;
         Putback_Char(Abstract_File'Class(File), Ch);

         begin
            Item := Enum'Value(Strval(1 .. Pos-1));
         exception
            when Constraint_Error => raise Data_Error;
         end;
      end Get;

      procedure Put(File    : in out Abstract_File;
                    Item    : in Enum;
                    Width   : in Field    := Default_Width;
                    Set     : in Type_Set := Default_Setting;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "") is
         Strval   : String := Enum'Image(Item);
         Fill     : String(1 .. Width);
         Fill_Pos : Integer := 0;
      begin
         if (Set = Lower_Case) then
            for I in Strval'Range loop
               Strval(I) := Ada.Characters.Handling.To_Lower(Strval(I));
            end loop;
         end if;

         Fill_Pos := Width - Strval'Length;
         if (Fill_Pos < 0) then
            Fill_Pos := 0;
         end if;
         for I in 1 .. Fill_Pos loop
            Fill(I) := ' ';
         end loop;

         if Wrapped then
            Put_Wrapped(Abstract_File'Class(File),
                        (Prefix & Fill(1 .. Fill_Pos) & Strval),
                        Left_Indent => Left_Indent,
                        Right_Indent => Right_Indent);
         else
            Put(Abstract_File'Class(File),
                Prefix & Fill(1 .. Fill_Pos) & Strval);
         end if;
      end Put;

   end Enumeration_IO;

end Asl.Abstract_IO;
