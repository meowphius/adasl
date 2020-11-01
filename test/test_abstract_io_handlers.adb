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

with Ada.Text_IO; use Ada.Text_IO;

package body Test_Abstract_IO_Handlers is

   procedure Put(File : in out Test_Abstract_File;
                 Item : in Character) is
   begin
      File.Last_Out := File.Last_Out + 1;
      File.Output(File.Last_Out) := Item;
      File.Column := File.Column + 1;
   end Put;

   procedure Put(File : in out Test_Abstract_File;
                 Item : in String) is
   begin
      File.Output(File.Last_Out + 1 .. File.Last_Out + Item'Length)
        := Item;
      File.Last_Out := File.Last_Out + Item'Length;
      File.Column := File.Column + Item'Length;
   end Put;

   procedure Put_Line(File : in out Test_Abstract_File;
                      Item : in String) is
   begin
      Put(File, Item);
      Put(File, Newline_Marker);
      File.Column := 1;
   end Put_Line;

   procedure New_Line(File : in out Test_Abstract_File) is
   begin
      Put(File, Newline_Marker);
      File.Column := 1;
   end New_Line;



   procedure Get(File : in out Test_Abstract_File;
                 Item : out Character) is
   begin
      if (File.Curr_In > File.Last_In) then
         raise Constraint_Error;
      end if;

      Item := File.Input(File.Curr_In);
      File.Curr_In := File.Curr_In + 1;
      if (File.Curr_In = (File.Last_In + 1)) then
         File.Curr_In := 1;
         File.Last_In := 0;
      end if;
   end Get;

   procedure Get(File : in out Test_Abstract_File;
                 Item : out String) is
      Curr : Integer := Item'First;
   begin
      while (Curr <= Item'Last) loop
         Get(File, Item(Curr));
      end loop;
   end Get;

   procedure Get_Line(File : in out Test_Abstract_File;
                      Item : out String;
                      Last : out Natural) is
      Curr : Integer := Item'First;
      Ch   : Character;
   begin
      while (Curr <= Item'Last) loop
         Get(File, Ch);
         exit when Ch = Newline_Marker;
         Item(Curr) := Ch;
      end loop;
      Last := Curr - 1;
   end Get_Line;

   function End_Of_Line(File : in Test_Abstract_File;
                        Ch   : in Character)
                        return Boolean is
   begin
      return Ch = Newline_Marker;
   end End_Of_Line;

   procedure Putback_Char(File : in out Test_Abstract_File;
                          Item : in Character) is
   begin
      if (File.Curr_In = 1) then
         -- Handle if the buffer is empty.
         File.Last_In := 1;
         File.Curr_In := 2;
      end if;
      File.Curr_In := File.Curr_In - 1;
      File.Input(File.Curr_In) := Item;
   end Putback_Char;

   function Next_Column(File : in Test_Abstract_File)
                        return Positive is
   begin
      return File.Column;
   end Next_Column;

   function IO_Width(File : in Test_Abstract_File)
                     return Positive is
   begin
      return 80;
   end IO_Width;

   function IO_Height(File : in Test_Abstract_File)
                      return Positive is
   begin
      return 24;
   end IO_Height;

   procedure Add_Input(File : in out Test_Abstract_File;
                       Data   : in String) is
   begin
      File.Input(File.Last_In + 1 .. File.Last_In + Data'Length) := Data;
      File.Last_In := File.Last_In + Data'Length;
   end Add_Input;

   procedure Add_Input_Newline(File : in out Test_Abstract_File) is
   begin
      Add_Input(File, (1 => Newline_Marker));
   end Add_Input_Newline;

   procedure Verify_Input(File : in out Test_Abstract_File;
                          Data   : in String) is
   begin
      if (Data'Length > (File.Last_In - File.Curr_In + 1)) then
         Put_Line("Verify data error, not enough data");
         Put_Line(" exp: '" & Data & "'");
         Put_Line(" was: '" & File.Input(File.Curr_In..File.Last_In)
                  & "'");
         raise Test_Error;
      elsif (Data /= File.Input(File.Curr_In
                                   .. File.Curr_In + Data'Length - 1))
      then
         Put_Line("Verify data error, invalid data");
         Put_Line(" exp: '" & Data & "'");
         Put_Line(" was: '" & File.Input(File.Curr_In..File.Last_In)
                  & "'");
         raise Test_Error;
      else
         File.Curr_In := File.Curr_In + Data'Length;
         if (File.Curr_In = (File.Last_In + 1)) then
            File.Curr_In := 1;
            File.Last_In := 0;
         end if;
      end if;
   end Verify_Input;

   procedure Verify_Input_Newline(File : in out Test_Abstract_File) is
   begin
      Verify_Input(File, (1 => Newline_Marker));
   end Verify_Input_Newline;

   procedure Verify_Input_Empty(File : in Test_Abstract_File) is
   begin
      if (File.Curr_In /= (File.Last_In + 1)) then
         raise Test_Error;
      end if;
   end Verify_Input_Empty;

   procedure Clear_Input(File : in out Test_Abstract_File) is
   begin
      File.Curr_In := 1;
      File.Last_In := 0;
   end Clear_Input;

   procedure Verify_Output(File : in out Test_Abstract_File;
                           Data   : in String) is
   begin
      if (Data'Length > (File.Last_Out - File.Curr_Out + 1)) then
         Put_Line("Verify data error, not enough data");
         Put_Line(" exp: '" & Data & "'");
         Put_Line(" was: '" & File.Output(File.Curr_Out..File.Last_Out)
                  & "'");
         raise Test_Error;
      elsif (Data /= File.Output(File.Curr_Out
                                   .. File.Curr_Out + Data'Length - 1))
      then
         Put_Line("Verify data error, invalid data");
         Put_Line(" exp: '" & Data & "'");
         Put_Line(" was: '" & File.Output(File.Curr_Out..File.Last_Out)
                  & "'");
         raise Test_Error;
      else
         File.Curr_Out := File.Curr_Out + Data'Length;
         if (File.Curr_Out = (File.Last_Out + 1)) then
            File.Curr_Out := 1;
            File.Last_Out := 0;
         end if;
      end if;
   end Verify_Output;

   procedure Verify_Output_Newline(File : in out Test_Abstract_File) is
   begin
      Verify_Output(File, (1 => Newline_Marker));
   end Verify_Output_Newline;

   procedure Verify_Output_Empty(File : in Test_Abstract_File) is
   begin
      if (File.Curr_Out /= (File.Last_Out + 1)) then
         Put_Line("Verify data error, output not empty");
         Put_Line(" was: '" & File.Output(File.Curr_Out..File.Last_Out)
                  & "'");
         raise Test_Error;
      end if;
   end Verify_Output_Empty;

end Test_Abstract_IO_Handlers;
