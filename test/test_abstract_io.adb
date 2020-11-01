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

with Asl.Abstract_IO; use Asl.Abstract_IO;
with Test_Abstract_IO_Handlers; use Test_Abstract_IO_Handlers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Test_Abstract_IO is
   package Abstract_Integer_IO is new Asl.Abstract_IO.Integer_IO(Integer);
   use Abstract_Integer_IO;
   package Abstract_Modular_IO is new Asl.Abstract_IO.Modular_IO(Modular_Type);
   use Abstract_Modular_IO;
   package Abstract_Float_IO is new Asl.Abstract_IO.Float_IO(Float);
   use Abstract_Float_IO;
   package Abstract_Fixed_IO is new Asl.Abstract_IO.Fixed_IO(Fixed_Type);
   use Abstract_Fixed_IO;
   package Abstract_Enum_IO is new Asl.Abstract_IO.Enumeration_IO(Enum_Type);
   use Abstract_Enum_IO;
   File : Test_Abstract_File;

   procedure Test_Integer is
      Val : Integer;
   begin
      Put(Abstract_File(File), Integer(10), Width => 0);
      New_Line(File);
      Verify_Output(File, "10");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Integer(10), Width => 2);
      New_Line(File);
      Verify_Output(File, "10");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Integer(-47), Width => 10);
      New_Line(File);
      Verify_Output(File, "       -47");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Add_Input(File, " -39");
      Add_Input_Newline(File);
      Get(Abstract_File(File), Val);
      if (Val /= -39) then
         raise Test_Error;
      end if;
      Verify_Input_Newline(File);
      Verify_Input_Empty(File);

      Add_Input(File, " 208 ");
      Get(Abstract_File(File), Val, Width => 5);
      if (Val /= 208) then
         raise Test_Error;
      end if;
      Verify_Input_Empty(File);

      Add_Input(File, " 3b3 ");
      Get(Abstract_File(File), Val, Base => 16);
      if (Val /= 16#3b3#) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " 3b3 ");
      begin
         Get(Abstract_File(File), Val, Base => 10, Width => 5);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, "      ");
      begin
         Get(Abstract_File(File), Val, Base => 10, Width => 5);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, "  ");
      Add_Input_Newline(File);
      begin
         Get(Abstract_File(File), Val, Base => 10);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);
   end Test_Integer;

   procedure Test_Modular is
      Val : Modular_Type;
   begin
      Put(Abstract_File(File), Modular_Type(10), Width => 0);
      New_Line(File);
      Verify_Output(File, "10");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Modular_Type(10), Width => 2);
      New_Line(File);
      Verify_Output(File, "10");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Add_Input(File, " 39");
      Add_Input_Newline(File);
      Get(Abstract_File(File), Val);
      if (Val /= 39) then
         raise Test_Error;
      end if;
      Verify_Input_Newline(File);
      Verify_Input_Empty(File);

      Add_Input(File, " 208 ");
      Get(Abstract_File(File), Val, Width => 5);
      if (Val /= 208) then
         raise Test_Error;
      end if;
      Verify_Input_Empty(File);

      Add_Input(File, " 3b3 ");
      Get(Abstract_File(File), Val, Base => 16);
      if (Val /= 16#3b3#) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " 3b3 ");
      begin
         Get(Abstract_File(File), Val, Base => 10, Width => 5);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, "      ");
      begin
         Get(Abstract_File(File), Val, Base => 10, Width => 5);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, "  ");
      Add_Input_Newline(File);
      begin
         Get(Abstract_File(File), Val, Base => 10);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);
   end Test_Modular;

   procedure Test_Float is
      Val : Float;
   begin
      Put(Abstract_File(File), Float(10.0), Fore=>0);
      New_Line(File);
      Verify_Output(File, "1.00000E+01");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Float(-0.2), Fore=>0);
      New_Line(File);
      Verify_Output(File, "-2.00000E-01");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Float(0.0), Fore=>0);
      New_Line(File);
      Verify_Output(File, "0.00000E+00");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Float(-0.9287394), Fore=>0, Exp=>0);
      New_Line(File);
      Verify_Output(File, "-0.92874");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Float(-0.928734), Fore=>0, Exp=>0);
      New_Line(File);
      Verify_Output(File, "-0.92873");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Float(-0.928792), Fore=>0, Exp=>0, Aft=>7);
      New_Line(File);
      Verify_Output(File, "-0.9287920");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Float(473.0), Fore=>10, Exp=>0);
      New_Line(File);
      Verify_Output(File, "       473.00000");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Add_Input(File, " 208 ");
      Get(Abstract_File(File), Val, Width => 5);
      if (Val /= 208.0) then
         raise Test_Error;
      end if;
      Verify_Input_Empty(File);

      Add_Input(File, " 1.987e-14 ");
      Get(Abstract_File(File), Val);
      if (abs(Val - 1.98700e-14) > 0.0001) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " 0.00e+00 ");
      Get(Abstract_File(File), Val);
      if (abs(Val - 0.0) > 0.0001) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " -292.928e+09");
      Add_Input_Newline(File);
      Get(Abstract_File(File), Val);
      if (abs(Val + 292.928e+09) > 1.0E5) then
         raise Test_Error;
      end if;
      Verify_Input_Newline(File);
      Verify_Input_Empty(File);

      Add_Input(File, " .928e+09 ");
      Get(Abstract_File(File), Val);
      if (abs(Val - 0.928e+09) > 1.0E5) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " 9.3 ");
      Get(Abstract_File(File), Val, Width=>5);
      if (abs(Val - 9.3) > 0.0001) then
         raise Test_Error;
      end if;
      Verify_Input_Empty(File);

      Add_Input(File, " . ");
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, " + ");
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, "  ");
      Add_Input_Newline(File);
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, " 9.3b ");
      begin
         Get(Abstract_File(File), Val, Width=>6);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, " 9 3 ");
      begin
         Get(Abstract_File(File), Val, Width=>5);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

   end Test_Float;

   procedure Test_Fixed is
      Val : Fixed_Type;
   begin
      Put(Abstract_File(File), Fixed_Type(10.0));
      New_Line(File);
      Verify_Output(File, "   10.000");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Fixed_Type(10.0), Fore=>0);
      New_Line(File);
      Verify_Output(File, "10.000");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Fixed_Type(-0.2), Fore=>0, Aft=>2);
      New_Line(File);
      Verify_Output(File, "-0.20");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Fixed_Type(-0.928), Aft=>2, Fore=>0, Exp=>0);
      New_Line(File);
      Verify_Output(File, "-0.93");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Fixed_Type(-0.923), Aft=>2, Fore=>0, Exp=>0);
      New_Line(File);
      Verify_Output(File, "-0.92");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Fixed_Type(-0.928), Fore=>0, Exp=>0, Aft=>7);
      New_Line(File);
      Verify_Output(File, "-0.9280000");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Fixed_Type(473.0), Fore=>10, Exp=>0);
      New_Line(File);
      Verify_Output(File, "       473.000");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Add_Input(File, " 208 ");
      Get(Abstract_File(File), Val, Width => 5);
      if (Val /= 208.0) then
         raise Test_Error;
      end if;
      Verify_Input_Empty(File);

      Add_Input(File, " 1.987e-2 ");
      Get(Abstract_File(File), Val);
      if (abs(Val - 1.98700e-2) > 0.0001) then
         Put_Line("val was " & Fixed_Type'Image(Val));
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " -292.928e+00");
      Add_Input_Newline(File);
      Get(Abstract_File(File), Val);
      if (abs(Val + 292.928e+00) > 0.005) then
         Put_Line("val was " & Fixed_Type'Image(Val));
         raise Test_Error;
      end if;
      Verify_Input_Newline(File);
      Verify_Input_Empty(File);

      Add_Input(File, " .928e+02 ");
      Get(Abstract_File(File), Val);
      if (abs(Val - 0.928e+02) > 1.0) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " 9.3 ");
      Get(Abstract_File(File), Val, Width=>5);
      if (abs(Val - 9.3) > 0.001) then
         Put_Line("val was " & Fixed_Type'Image(Val));
         raise Test_Error;
      end if;
      Verify_Input_Empty(File);

      Add_Input(File, " . ");
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, " + ");
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, "  ");
      Add_Input_Newline(File);
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, " 9.3b ");
      begin
         Get(Abstract_File(File), Val, Width=>6);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

      Add_Input(File, " 9 3 ");
      begin
         Get(Abstract_File(File), Val, Width=>5);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);

   end Test_Fixed;

   procedure Test_Enum is
      Val : Enum_Type;
   begin
      Put(Abstract_File(File), Asdf, Width=>6);
      New_Line(File);
      Verify_Output(File, "  ASDF");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Put(Abstract_File(File), Asdf, Width=>3, Set=>Lower_Case);
      New_Line(File);
      Verify_Output(File, "asdf");
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      Add_Input(File, " jkl ");
      Get(Abstract_File(File), Val);
      if (Val /= Jkl) then
         raise Test_Error;
      end if;
      Verify_Input(File, " ");
      Verify_Input_Empty(File);

      Add_Input(File, " jkl; ");
      begin
         Get(Abstract_File(File), Val);
         raise Test_Error;
      exception
         when Data_Error => null;
      end;
      Clear_Input(File);
   end Test_Enum;

   procedure Test_Wrap is
      type String_Array is array (Integer range <>) of String(1 .. 9);
      Val1 : String_Array(1 .. 10) := ("123456789",
                                       "234567891",
                                       "345678912",
                                       "456789123",
                                       "567891234",
                                       "678912345",
                                       "789123456",
                                       "891234567",
                                       "912345678",
                                       "012345678");
   begin
      -- Get to column 1.
      New_Line(Abstract_File'Class(File));
      Verify_Output_Newline(File);
      Verify_Output_Empty(File);

      for I in Val1'Range loop
         Put_Wrapped(Abstract_File(File), " " & Val1(I) & " ");
      end loop;
      Verify_Output(File,
                    (Val1(1) & " " & Val1(2) & " " & Val1(3) & " "
                     & Val1(4) & " " & Val1(5) & " " & Val1(6) & " "
                     & Val1(7) & " " & Val1(8)));
      Verify_Output_Newline(File);

      Put_Wrapped(Abstract_File(File),
                  (" " & Val1(1) & " " & Val1(2) & " " & Val1(3) & " "
                   & Val1(4) & " " & Val1(5) & " " & Val1(6) & " "
                   & Val1(7) & " " & Val1(8)));

      Verify_Output(File, (Val1(9) & " " & Val1(10) & " " & Val1(1) & " "
                           & Val1(2) & " " & Val1(3) & " " & Val1(4) & " "
                           & Val1(5) & " " & Val1(6)));
      Verify_Output_Newline(File);

      Put_Wrapped(Abstract_File(File),
                  (" " & Val1(1) & Val1(2) & Val1(3)
                   & Val1(4) & Val1(5) & Val1(6)
                   & Val1(7) & Val1(8) & Val1(9) & Val1(10)));

      Verify_Output(File, (Val1(7) & " " & Val1(8)));
      Verify_Output_Newline(File);

      Verify_Output(File, (Val1(1) & Val1(2) & Val1(3)
                           & Val1(4) & Val1(5) & Val1(6)
                           & Val1(7) & Val1(8) & Val1(9) & Val1(10)));
      Verify_Output_Empty(File);
   end Test_Wrap;

begin
   Test_Integer;
   Test_Modular;
   Test_Float;
   Test_Fixed;
   Test_Enum;
   Test_Wrap;
   Put_Line("Tests passed");
end Test_Abstract_IO;
