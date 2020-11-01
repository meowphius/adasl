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

package Test_Abstract_IO_Handlers is

   type Modular_Type is mod 65536;

   type Fixed_Type is delta 0.001 range -1000.000 .. 1000.000;

   type Enum_Type is (Asdf, Jkl, Qwerty);

   Test_Error : exception;

   type Test_Abstract_File is new Abstract_File with record
      Output   : String(1 .. 1024);
      Last_Out : Integer := 0;
      Curr_Out : Integer := 1;
      Input    : String(1 .. 1024);
      Last_In  : Integer := 0;
      Curr_In  : Integer := 1;
      Column   : Positive := 1;
   end record;

   procedure Put(File : in out Test_Abstract_File;
                 Item : in Character);

   procedure Put(File : in out Test_Abstract_File;
                 Item : in String);

   procedure Put_Line(File : in out Test_Abstract_File;
                      Item : in String);

   procedure New_Line(File : in out Test_Abstract_File);


   procedure Get(File : in out Test_Abstract_File;
                 Item : out Character);

   procedure Get(File : in out Test_Abstract_File;
                 Item : out String);

   procedure Get_Line(File : in out Test_Abstract_File;
                      Item : out String;
                      Last : out Natural);

   function End_Of_Line(File : in Test_Abstract_File;
                        Ch   : in Character)
                        return Boolean;

   procedure Putback_Char(File : in out Test_Abstract_File;
                          Item : in Character);


   procedure Add_Input(File : in out Test_Abstract_File;
                       Data : in String);
   procedure Add_Input_Newline(File : in out Test_Abstract_File);
   procedure Verify_Input(File : in out Test_Abstract_File;
                          Data : in String);
   procedure Verify_Input_Newline(File : in out Test_Abstract_File);
   procedure Verify_Input_Empty(File : in Test_Abstract_File);
   procedure Clear_Input(File : in out Test_Abstract_File);
   procedure Verify_Output(File : in out Test_Abstract_File;
                           Data : in String);
   procedure Verify_Output_Newline(File : in out Test_Abstract_File);
   procedure Verify_Output_Empty(File : in Test_Abstract_File);

   function Next_Column(File : in Test_Abstract_File)
                        return Positive;
   function IO_Width(File : in Test_Abstract_File)
                     return Positive;
   function IO_Height(File : in Test_Abstract_File)
                      return Positive;

private

   -- Mark a new line with this.
   Newline_Marker : constant Character := Character'Val(0);

end Test_Abstract_IO_Handlers;
