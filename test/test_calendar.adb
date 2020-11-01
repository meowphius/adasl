-- The Ada Structured Library - A set of packages for Ada95
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

with Asl.Date_Time; use Asl.Date_Time;
with Asl.Date_Time.Calendar; use Asl.Date_Time.Calendar;
with Asl.Date_Time.Timezone; use Asl.Date_Time.Timezone;
with Asl.Date_Time.Register_Simple_Timezones;
with Asl.Date_Time.Register_Complex_Timezones;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Asl.Strings; use Asl.Strings;

procedure Test_Calendar is
   Cal1      : Calendar_Time;
   Cal2      : Calendar_Time;
   CST_Zone  : Time_Zone_Class;
   UTC_Zone  : Time_Zone_Class;
   T         : Time;
   Tnum      : String(1 .. 20);
   Tlen      : Integer;
   Use_Zones : Zone_Factory_Ptr;

   procedure Ch_Year(C : in Calendar_Time; V : in Year_Range; Str : in String)
   is
      Val : Year_Range;
   begin
      Val := Get_Year(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Year invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Year;

   procedure Ch_Month(C   : in Calendar_Time;
                      V   : in Month_Range;
                      Str : in String)
   is
      Val : Month_Range;
   begin
      Val := Get_Month(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Month invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Month;

   procedure Ch_Day_Of_Year(C   : in Calendar_Time;
                            V   : in Day_Of_Year_Range;
                            Str : in String)
   is
      Val : Day_Of_Year_Range;
   begin
      Val := Get_Day_Of_Year(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Day_Of_Year invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Day_Of_Year;

   procedure Ch_Hour_Of_Day(C   : in Calendar_Time;
                            V   : in Hour_Of_Day_Range;
                            Str : in String)
   is
      Val : Hour_Of_Day_Range;
   begin
      Val := Get_Hour_Of_Day(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Hour_Of_Day invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Hour_Of_Day;

   procedure Ch_Minute(C   : in Calendar_Time;
                       V   : in Minute_Range;
                       Str : in String)
   is
      Val : Minute_Range;
   begin
      Val := Get_Minute(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Minute invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Minute;

   procedure Ch_Second(C   : in Calendar_Time;
                       V   : in Second_Range;
                       Str : in String)
   is
      Val : Second_Range;
   begin
      Val := Get_Second(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Second invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Second;

   procedure Ch_Sub_Second(C   : in Calendar_Time;
                           V   : in Sub_Second_Duration;
                           Str : in String)
   is
      Val : Sub_Second_Duration;
   begin
      Val := Get_Sub_Second(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Sub_Second invalid");
         Put_Line("  Value was " & Duration'Image(Val)
                  & " but expected " & Duration'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Sub_Second;

   procedure Ch_DST_Active(C   : in Calendar_Time;
                           V   : in Boolean;
                           Str : in String)
   is
      Val : Boolean;
   begin
      Val := Get_DST_Active(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": DST_Active invalid");
         Put_Line("  Value was " & Boolean'Image(Val)
                  & " but expected " & Boolean'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_DST_Active;

   procedure Ch_Week_Of_Year(C   : in Calendar_Time;
                    V   : in Week_Of_Year_Range;
                    Str : in String)
   is
      Val : Week_Of_Year_Range;
   begin
      Val := Get_Week_Of_Year(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Week_Of_Year invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Week_Of_Year;

   procedure Ch_Week_Of_Month(C   : in Calendar_Time;
                    V   : in Week_Of_Month_Range;
                    Str : in String)
   is
      Val : Week_Of_Month_Range;
   begin
      Val := Get_Week_Of_Month(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Week_Of_Month invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Week_Of_Month;

   procedure Ch_Day_Of_Month(C   : in Calendar_Time;
                    V   : in Day_Of_Month_Range;
                    Str : in String)
   is
      Val : Day_Of_Month_Range;
   begin
      Val := Get_Day_Of_Month(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Day_Of_Month invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Day_Of_Month;

   procedure Ch_Weekday(C   : in Calendar_Time;
                        V   : in Weekday_Range;
                        Str : in String)
   is
      Val : Weekday_Range;
   begin
      Val := Get_Weekday(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Weekday invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Weekday;

   procedure Ch_AM_PM(C   : in Calendar_Time;
                    V   : in AM_PM_Range;
                    Str : in String)
   is
      Val : AM_PM_Range;
   begin
      Val := Get_AM_PM(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": AM_PM invalid");
         Put_Line("  Value was " & AM_PM_Range'Image(Val)
                  & " but expected " & AM_PM_Range'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_AM_PM;

   procedure Ch_Hour(C   : in Calendar_Time;
                    V   : in Hour_Range;
                    Str : in String)
   is
      Val : Hour_Range;
   begin
      Val := Get_Hour(C);
      if (Val /= V) then
         Put_Line(Tnum(1 .. Tlen) & Str & ": Hour invalid");
         Put_Line("  Value was " & Integer'Image(Val)
                  & " but expected " & Integer'Image(V));
         raise Constraint_Error;
      end if;
   end Ch_Hour;

   procedure Set_Tnum(Str : in String) is
   begin
      --Put_Line("Starting test " & Str);
      Tnum(1 .. Str'Length) := Str;
      Tlen := Str'Length;
   end Set_Tnum;

   procedure Basic_Tests is
      procedure Validate_V1(Cal1 : in Calendar_Time; Str : in String) is
      begin
         Ch_Year(Cal1, 2001, Str);
         Ch_Month(Cal1, 10, Str);
         Ch_Hour_Of_Day(Cal1, 1, Str);
         Ch_Minute(Cal1, 29, Str);
         Ch_Second(Cal1, 32, Str);
         Ch_Sub_Second(Cal1, 0.25, Str);
         Ch_Hour_Of_Day(Cal1, 1, Str);
         Ch_Minute(Cal1, 29, Str);
         Ch_Week_Of_Year(Cal1, 42, Str);
         Ch_Week_Of_Month(Cal1, 3, Str);
         Ch_Hour(Cal1, 1, Str);
         Ch_AM_PM(Cal1, AM, Str);
         Ch_Second(Cal1, 32, Str);
      end Validate_V1;
   begin
      Set_Tnum("Basic");
      Set_Timezone(Cal1, CST_Zone);
      -- Time is Oct 14, 2001 01:29:32.250000
      T := Time_Of(2001, 10, 14, 3600.0 + 1740.0 + 32.25);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "1");
      Ch_Day_Of_Year(Cal1, 287, "1");
      Ch_Day_Of_Month(Cal1, 14, "1");
      Ch_Weekday(Cal1, 1, "1");

      Set_Day_Of_Month(Cal1, 15);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "2");
      Ch_Day_Of_Year(Cal1, 288, "2");
      Ch_Day_Of_Month(Cal1, 15, "2");
      Ch_Weekday(Cal1, 2, "2");

      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "3");
      Ch_Day_Of_Year(Cal1, 289, "3");
      Ch_Day_Of_Month(Cal1, 16, "3");
      Ch_Weekday(Cal1, 3, "3");

      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "4");
      Ch_Day_Of_Year(Cal1, 290, "4");
      Ch_Day_Of_Month(Cal1, 17, "4");
      Ch_Weekday(Cal1, 4, "4");

      Set_Day_Of_Month(Cal1, 18);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "5");
      Ch_Day_Of_Year(Cal1, 291, "5");
      Ch_Day_Of_Month(Cal1, 18, "5");
      Ch_Weekday(Cal1, 5, "5");

      Add_To_Second(Cal1, 24 * 3600);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "6");
      Ch_Day_Of_Year(Cal1, 292, "6");
      Ch_Day_Of_Month(Cal1, 19, "6");
      Ch_Weekday(Cal1, 6, "6");

      Add_To_Second(Cal1, 24 * 3600);
      Compute_Fields(Cal1);
      Validate_V1(Cal1, "7");
      Ch_Day_Of_Year(Cal1, 293, "7");
      Ch_Day_Of_Month(Cal1, 20, "7");
      Ch_Weekday(Cal1, 7, "7");
   end Basic_Tests;

   procedure DST_Tests is
   begin
      Set_Tnum("DST");
      Set_Timezone(Cal1, CST_Zone);
      -- Time is Oct 28, 2001 01:00:00
      T := Time_Of(2001, 10, 28, 3600.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Ch_Hour_Of_Day(Cal1, 1, "1");
      Add_To_Second(Cal1, 3600-1);
      Compute_Fields(Cal1);
      Ch_Hour_Of_Day(Cal1, 1, "2");
      Ch_Minute(Cal1, 59, "2");
      Ch_Second(Cal1, 59, "2");
      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);
      -- Adding an hour on end of daylight savings falls back, going back
      -- to 1:00 again.
      Ch_Hour_Of_Day(Cal1, 1, "3");
      Ch_Minute(Cal1, 0, "3");
      Ch_Second(Cal1, 0, "3");

      -- Time is Apr 1, 2001 01:00:00
      T := Time_Of(2001, 4, 1, 3600.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Ch_Hour_Of_Day(Cal1, 1, "4");
      Add_To_Second(Cal1, 3600-1);
      Compute_Fields(Cal1);
      Ch_Hour_Of_Day(Cal1, 1, "5");
      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);
      Ch_Hour_Of_Day(Cal1, 3, "6");
   end DST_Tests;

   procedure Week_Tests is
   begin
      Set_Tnum("Week");
      Set_Timezone(Cal1, CST_Zone);
      -- Time is Mar 1, 2001 01:00:00
      T := Time_Of(2001, 3, 1, 3600.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 4, "1");
      Set_Day_Of_Month(Cal1, 2);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 4, "2");
      Set_Day_Of_Month(Cal1, 3);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 4, "3");
      Set_Day_Of_Month(Cal1, 4);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 1, "4");

      -- Time is Aug 1, 2001 01:00:00
      T := Time_Of(2001, 8, 1, 3600.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 1, "5");
      Set_Day_Of_Month(Cal1, 31);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 5, "6");
      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 5, "7");
      Ch_Day_Of_Month(Cal1, 1, "7");
      Ch_Month(Cal1, 9, "7");
      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Ch_Week_Of_Month(Cal1, 1, "8");

      -- Time is Jan 1, 2004 01:00:00
      T := Time_Of(2004, 1, 1, 3600.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Cal2 := Cal1;
      Ch_Weekday(Cal1, 5, "9");
      Ch_Week_Of_Month(Cal1, 5, "9");
      Ch_Week_Of_Year(Cal1, 53, "9");
      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Ch_Week_Of_Year(Cal1, 53, "10");
      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Ch_Week_Of_Year(Cal1, 53, "11");
      Add_To_Day_Of_Year(Cal1, 1);
      Compute_Fields(Cal1);
      Ch_Week_Of_Year(Cal1, 1, "12");

      -- Now set monday as the first day of the week and retry this.
      Set_First_Weekday(Cal2, 2);
      Compute_Fields(Cal2);
      Ch_Weekday(Cal2, 5, "9");
      Ch_Week_Of_Month(Cal2, 1, "9");
      Ch_Week_Of_Year(Cal2, 1, "9");
      Add_To_Day_Of_Year(Cal2, 1);
      Compute_Fields(Cal2);
      Ch_Week_Of_Year(Cal2, 1, "10");
      Add_To_Day_Of_Year(Cal2, 1);
      Compute_Fields(Cal2);
      Ch_Week_Of_Year(Cal2, 1, "11");
      Add_To_Day_Of_Year(Cal2, 1);
      Compute_Fields(Cal2);
      Ch_Week_Of_Year(Cal2, 1, "12");
      Add_To_Day_Of_Year(Cal2, 1);
      Compute_Fields(Cal2);
      Ch_Week_Of_Year(Cal2, 2, "12");
      Add_To_Day_Of_Year(Cal2, 1);
      Compute_Fields(Cal2);
      Ch_Week_Of_Year(Cal2, 2, "12");
   end Week_Tests;

   procedure Timezone_Tests is
   begin
      Set_Tnum("Timezone");
      Set_Timezone(Cal1, CST_Zone);
      -- Time is Feb 28, 2001 19:00:00
      T := Time_Of(2001, 2, 28, 19.0 * 3600.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Set_Timezone(Cal1, UTC_Zone);
      Compute_Fields(Cal1);
      Ch_Year(Cal1, 2001, "1");
      Ch_Month(Cal1, 3, "1");
      Ch_Day_Of_Month(Cal1, 1, "1");
      Ch_Hour_Of_Day(Cal1, 1, "1");

      Set_Year(Cal1, 2001);
      Set_Month(Cal1, 4);
      Set_Day_Of_Month(Cal1, 1);
      Set_Hour_Of_Day(Cal1, 10);
      Compute_Fields(Cal1);

      Set_Timezone(Cal1, CST_Zone);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "2");
      Ch_Month(Cal1, 4, "2");
      Ch_Day_Of_Month(Cal1, 1, "2");
      Ch_Hour_Of_Day(Cal1, 5, "2");

      Set_Year(Cal1, 2001);
      Set_Month(Cal1, 10);
      Set_Day_Of_Month(Cal1, 28);
      Set_Hour_Of_Day(Cal1, 4);
      Compute_Fields(Cal1);

      Set_Timezone(Cal1, UTC_Zone);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "3");
      Ch_Month(Cal1, 10, "3");
      Ch_Day_Of_Month(Cal1, 28, "3");
      Ch_Hour_Of_Day(Cal1, 10, "3");

      -- Now some south of the equator stuff.  This also tests the "standard"
      -- time stuff.
      Set_Timezone(Cal1, Find_Timezone(Use_Zones.all, "Australia/Adelaide"));
      -- Time is Oct 28, 2001 01:59:59
      T := Time_Of(2001, 10, 28, (2.0 * 3600.0) - 1.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "4");
      Ch_Month(Cal1, 10, "4");
      Ch_Day_Of_Month(Cal1, 28, "4");
      Ch_Hour_Of_Day(Cal1, 1, "4");
      Ch_Minute(Cal1, 59, "4");
      Ch_Second(Cal1, 59, "4");
      Ch_DST_Active(Cal1, False, "4");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "5");
      Ch_Month(Cal1, 10, "5");
      Ch_Day_Of_Month(Cal1, 28, "5");
      Ch_Hour_Of_Day(Cal1, 3, "5");
      Ch_Minute(Cal1, 0, "5");
      Ch_Second(Cal1, 0, "5");
      Ch_DST_Active(Cal1, True, "5");

      -- Time is Mar 25, 2001 2:59:59
      T := Time_Of(2001, 3, 25, (3.0 * 3600.0) - 1.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "6");
      Ch_Month(Cal1, 3, "6");
      Ch_Day_Of_Month(Cal1, 25, "6");
      Ch_Hour_Of_Day(Cal1, 2, "6");
      Ch_Minute(Cal1, 59, "6");
      Ch_Second(Cal1, 59, "6");
      Ch_DST_Active(Cal1, True, "6");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "7");
      Ch_Month(Cal1, 3, "7");
      Ch_Day_Of_Month(Cal1, 25, "7");
      Ch_Hour_Of_Day(Cal1, 2, "7");
      Ch_Minute(Cal1, 0, "7");
      Ch_Second(Cal1, 0, "7");
      Ch_DST_Active(Cal1, False, "7");

      -- Now try the EU, with the universal time stuff.
      Set_Timezone(Cal1, Find_Timezone(Use_Zones.all, "Europe/Sofia"));
      -- Time is Mar 25 2001, 2:59:59
      T := Time_Of(2001, 3, 25, (3.0 * 3600.0) - 1.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "8");
      Ch_Month(Cal1, 3, "8");
      Ch_Day_Of_Month(Cal1, 25, "8");
      Ch_Hour_Of_Day(Cal1, 2, "8");
      Ch_Minute(Cal1, 59, "8");
      Ch_Second(Cal1, 59, "8");
      Ch_DST_Active(Cal1, False, "8");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "9");
      Ch_Month(Cal1, 3, "9");
      Ch_Day_Of_Month(Cal1, 25, "9");
      Ch_Hour_Of_Day(Cal1, 4, "9");
      Ch_Minute(Cal1, 0, "9");
      Ch_Second(Cal1, 0, "9");
      Ch_DST_Active(Cal1, True, "9");

      -- Time is Oct 28, 2001 3:59:59
      T := Time_Of(2001, 10, 28, (4.0 * 3600.0) - 1.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "10");
      Ch_Month(Cal1, 10, "10");
      Ch_Day_Of_Month(Cal1, 28, "10");
      Ch_Hour_Of_Day(Cal1, 3, "10");
      Ch_Minute(Cal1, 59, "10");
      Ch_Second(Cal1, 59, "10");
      Ch_DST_Active(Cal1, True, "10");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "11");
      Ch_Month(Cal1, 10, "11");
      Ch_Day_Of_Month(Cal1, 28, "11");
      Ch_Hour_Of_Day(Cal1, 3, "11");
      Ch_Minute(Cal1, 0, "11");
      Ch_Second(Cal1, 0, "11");
      Ch_DST_Active(Cal1, False, "11");
   end Timezone_Tests;

   Simple_Zones, Complex_Zones : Zone_Factory_Ptr;

   procedure Complex_Zones_Tests is
   begin
      Set_Tnum("Complex_Zones");
      -- Test an offset timezone moving into a rule timezone.
      Set_Timezone(Cal1, Find_Timezone(Complex_Zones.all, "Europe/Tallinn"));
      -- Time is Sep 24, 1989 2:59:59
      T := Time_Of(1989, 9, 24, (3.0 * 3600.0) - 1.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1989, "1");
      Ch_Month(Cal1, 9, "1");
      Ch_Day_Of_Month(Cal1, 24, "1");
      Ch_Hour_Of_Day(Cal1, 2, "1");
      Ch_Minute(Cal1, 59, "1");
      Ch_Second(Cal1, 59, "1");
      Ch_DST_Active(Cal1, True, "1");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1989, "2");
      Ch_Month(Cal1, 9, "2");
      Ch_Day_Of_Month(Cal1, 24, "2");
      Ch_Hour_Of_Day(Cal1, 2, "2");
      Ch_Minute(Cal1, 0, "2");
      Ch_Second(Cal1, 0, "2");
      Ch_DST_Active(Cal1, False, "2");

      -- Now some south of the equator stuff
      Set_Timezone(Cal1, Find_Timezone(Complex_Zones.all, "Australia/Darwin"));
      -- Time is Jan 1, 1917 00:00:59
      T := Time_Of(1917, 1, 1, 59.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1917, "3");
      Ch_Month(Cal1, 1, "3");
      Ch_Day_Of_Month(Cal1, 1, "3");
      Ch_Hour_Of_Day(Cal1, 0, "3");
      Ch_Minute(Cal1, 0, "3");
      Ch_Second(Cal1, 59, "3");
      Ch_DST_Active(Cal1, False, "3");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1917, "5");
      Ch_Month(Cal1, 1, "5");
      Ch_Day_Of_Month(Cal1, 1, "5");
      Ch_Hour_Of_Day(Cal1, 1, "5");
      Ch_Minute(Cal1, 1, "5");
      Ch_Second(Cal1, 0, "5");
      Ch_DST_Active(Cal1, True, "5");

      -- Time is Mar 25, 1917 01:59:59
      T := Time_Of(1917, 3, 25, (1.0 * 3600.0) + (59.0 * 60.0) + 59.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1917, "6");
      Ch_Month(Cal1, 3, "6");
      Ch_Day_Of_Month(Cal1, 25, "6");
      Ch_Hour_Of_Day(Cal1, 1, "6");
      Ch_Minute(Cal1, 59, "6");
      Ch_Second(Cal1, 59, "6");
      Ch_DST_Active(Cal1, True, "6");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1917, "7");
      Ch_Month(Cal1, 3, "7");
      Ch_Day_Of_Month(Cal1, 25, "7");
      Ch_Hour_Of_Day(Cal1, 1, "7");
      Ch_Minute(Cal1, 0, "7");
      Ch_Second(Cal1, 0, "7");
      Ch_DST_Active(Cal1, False, "7");
   end Complex_Zones_Tests;

   procedure Leap_Seconds_Tests is
   begin
      Set_Tnum("Leap Seconds");
      Set_Timezone(Cal1, CST_Zone);
      Invalidate_Time(Cal1);
      Set_Do_Leap_Seconds(Cal1, True);

      -- Time is Feb 28, 2001 19:00:00 + 22 leap seconds
      T := Time_Of(2001, 2, 28, 19.0 * 3600.0 + 22.0);
      Set_Time(Cal1, T);
      Compute_Fields(Cal1);
      Set_Timezone(Cal1, UTC_Zone);
      Compute_Fields(Cal1);
      Ch_Year(Cal1, 2001, "1");
      Ch_Month(Cal1, 3, "1");
      Ch_Day_Of_Month(Cal1, 1, "1");
      Ch_Hour_Of_Day(Cal1, 1, "1");
      Ch_Minute(Cal1, 0, "1");
      Ch_Second(Cal1, 0, "1");

      Set_Year(Cal1, 2001);
      Set_Month(Cal1, 4);
      Set_Day_Of_Month(Cal1, 1);
      Set_Hour_Of_Day(Cal1, 10);
      Compute_Fields(Cal1);

      Set_Timezone(Cal1, CST_Zone);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "2");
      Ch_Month(Cal1, 4, "2");
      Ch_Day_Of_Month(Cal1, 1, "2");
      Ch_Hour_Of_Day(Cal1, 5, "2");
      Ch_Minute(Cal1, 0, "2");
      Ch_Second(Cal1, 0, "2");

      Set_Timezone(Cal1, UTC_Zone);
      -- Time is Dec 31, 1990, 23:59:59.  The next second is a leap
      -- second.  This test runs the time over the leap second one
      -- second at a time.
      Set_Year(Cal1, 1990);
      Set_Month(Cal1, 12);
      Set_Day_Of_Month(Cal1, 31);
      Set_Hour_Of_Day(Cal1, 23);
      Set_Minute(Cal1, 59);
      Set_Second(Cal1, 59);
      Compute_Fields(Cal1);
      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1990, "3");
      Ch_Month(Cal1, 12, "3");
      Ch_Day_Of_Month(Cal1, 31, "3");
      Ch_Hour_Of_Day(Cal1, 23, "3");
      Ch_Minute(Cal1, 59, "3");
      Ch_Second(Cal1, 60, "3");

      Add_To_Second(Cal1, 1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1991, "4");
      Ch_Month(Cal1, 1, "4");
      Ch_Day_Of_Month(Cal1, 1, "4");
      Ch_Hour_Of_Day(Cal1, 0, "4");
      Ch_Minute(Cal1, 0, "4");
      Ch_Second(Cal1, 0, "4");

      -- Now run the time back the other way over the leap second.
      Add_To_Second(Cal1, -1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1990, "5");
      Ch_Month(Cal1, 12, "5");
      Ch_Day_Of_Month(Cal1, 31, "5");
      Ch_Hour_Of_Day(Cal1, 23, "5");
      Ch_Minute(Cal1, 59, "5");
      Ch_Second(Cal1, 60, "5");

      Add_To_Second(Cal1, -1);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1990, "6");
      Ch_Month(Cal1, 12, "6");
      Ch_Day_Of_Month(Cal1, 31, "6");
      Ch_Hour_Of_Day(Cal1, 23, "6");
      Ch_Minute(Cal1, 59, "6");
      Ch_Second(Cal1, 59, "6");

      -- Now jump over the leap second.
      Add_To_Second(Cal1, 2);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1991, "7");
      Ch_Month(Cal1, 1, "7");
      Ch_Day_Of_Month(Cal1, 1, "7");
      Ch_Hour_Of_Day(Cal1, 0, "7");
      Ch_Minute(Cal1, 0, "7");
      Ch_Second(Cal1, 0, "7");

      -- backup over the previous minute
      Add_To_Second(Cal1, -70);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1990, "8");
      Ch_Month(Cal1, 12, "8");
      Ch_Day_Of_Month(Cal1, 31, "8");
      Ch_Hour_Of_Day(Cal1, 23, "8");
      Ch_Minute(Cal1, 58, "8");
      Ch_Second(Cal1, 51, "8");

      -- Now add the same amount back to go over the minute
      Add_To_Second(Cal1, 70);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1991, "9");
      Ch_Month(Cal1, 1, "9");
      Ch_Day_Of_Month(Cal1, 1, "9");
      Ch_Hour_Of_Day(Cal1, 0, "9");
      Ch_Minute(Cal1, 0, "9");
      Ch_Second(Cal1, 0, "9");

      Set_Do_Leap_Seconds(Cal1, False);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1991, "10");
      Ch_Month(Cal1, 1, "10");
      Ch_Day_Of_Month(Cal1, 1, "10");
      Ch_Hour_Of_Day(Cal1, 0, "10");
      Ch_Minute(Cal1, 0, "10");
      Ch_Second(Cal1, 16, "10");

      Add_To_Second(Cal1, -1);
      Set_Do_Leap_Seconds(Cal1, True);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 1990, "11");
      Ch_Month(Cal1, 12, "11");
      Ch_Day_Of_Month(Cal1, 31, "11");
      Ch_Hour_Of_Day(Cal1, 23, "11");
      Ch_Minute(Cal1, 59, "11");
      Ch_Second(Cal1, 60, "11");

      Set_Do_Leap_Seconds(Cal1, False);
      Compute_Fields(Cal1);
      Ch_Year(Cal1, 1991, "12");
      Ch_Month(Cal1, 1, "12");
      Ch_Day_Of_Month(Cal1, 1, "12");
      Ch_Hour_Of_Day(Cal1, 0, "12");
      Ch_Minute(Cal1, 0, "12");
      Ch_Second(Cal1, 15, "12");
   end Leap_Seconds_Tests;

   procedure Unix_Time_Tests is
      Ut : Unix_Time;
   begin
      Set_Tnum("Unix Time");
      Ut.Second := 1_005_577_836;
      Ut.Microsecond := 10_000;
      Set_Timezone(Cal1, Cst_Zone);
      Set_Time(Cal1, Ut);
      Compute_Fields(Cal1);

      Ch_Year(Cal1, 2001, "1");
      Ch_Month(Cal1, 11, "1");
      Ch_Day_Of_Month(Cal1, 12, "1");
      Ch_Hour_Of_Day(Cal1, 9, "1");
      Ch_Minute(Cal1, 10, "1");
      Ch_Second(Cal1, 36, "1");
      Ch_Sub_Second(Cal1, 0.01, "1");

      Ut := Get_Time(Cal1);
      if (Ut /= (1_005_577_836, 10_000)) then
         Put_Line("Conversion back to unix time failed");
         raise Constraint_Error;
      end if;
   end Unix_Time_Tests;

   procedure Portable_Time_Tests is
      Pt : Portable_Time;
   begin
      Set_Tnum("Portable Time");

      Pt := (2001, 27_270_636, 10_000);
      Set_Timezone(Cal1, CST_Zone);
      Set_Time(Cal1, Pt);

      Ch_Year(Cal1, 2001, "1");
      Ch_Month(Cal1, 11, "1");
      Ch_Day_Of_Month(Cal1, 12, "1");
      Ch_Hour_Of_Day(Cal1, 9, "1");
      Ch_Minute(Cal1, 10, "1");
      Ch_Second(Cal1, 36, "1");
      Ch_Sub_Second(Cal1, 0.01, "1");

      Pt := Get_Time(Cal1);
      if (Pt /= (2001, 27_270_636, 10_000)) then
         Put_Line("Conversion back to portable time failed");
         raise Constraint_Error;
      end if;
   end Portable_Time_Tests;

   procedure Diff_Tests is
      Ut : Unix_Time;
   begin
      Set_Tnum("Unix Time");
      Ut.Second := 1_005_577_836;
      Ut.Microsecond := 10_000;
      Set_Timezone(Cal1, CST_Zone);
      Set_Time(Cal1, Ut);
      Compute_Fields(Cal1);

      Ut.Second := 978307200;
      Ut.Microsecond := 20_000;
      Set_Timezone(Cal2, UTC_Zone);
      Set_Time(Cal2, Ut);
      Compute_Fields(Cal2);

      if ((Cal1 - Cal2) /= 27_270_635.99) then
         Put_Line("Diff tests resulted in " & Duration'Image(Cal1 - Cal2)
                  & " but should have been 27_270_635.99");
         raise Constraint_Error;
      end if;

      if ((Cal2 - Cal1) /= -27_270_635.99) then
         Put_Line("Diff tests resulted in " & Duration'Image(Cal2 - Cal1)
                  & " but should have been 27_270_635.99");
         raise Constraint_Error;
      end if;

      Invalidate_Time(Cal2);
      Set_Year(Cal2, 1987);
      Set_Month(Cal2, 3);
      Set_Day_Of_Month(Cal2, 27);
      Set_Hour_Of_Day(Cal2, 10);
      Set_Minute(Cal2, 43);
      Set_Second(Cal2, 27);
      Compute_Fields(Cal2);

      if ((Cal1 - Cal2) /= 461_737_629.01) then
         Put_Line("Diff tests resulted in " & Duration'Image(Cal1 - Cal2)
                  & " but should have been 461_737_629.01");
         raise Constraint_Error;
      end if;
   end Diff_Tests;

   procedure Image_Tests is
   begin
      Set_Tnum("Image");
      Invalidate_Time(Cal1);
      Set_Timezone(Cal1, CST_Zone);
      Set_Year(Cal1, 2001);
      Set_Month(Cal1, 10);
      Set_Day_Of_Month(Cal1, 28);
      Set_Hour_Of_Day(Cal1, 4);
      Set_Minute(Cal1, 27);
      Set_Second(Cal1, 19);
      Compute_Fields(Cal1);
      declare
         Str : String := Image(Cal1);
         Cmp : String := "28 Oct 2001 04:27:19.000000 CST";
      begin
         if (Compare(Str, Cmp) /= 0) then
            Put_Line("Image not correct, was '" & Str
                     & "' but should have been '" & Cmp & "'");
            raise Constraint_Error;
         end if;
      end;

      -- Move into DST range
      Add_To_Second(Cal1, -(4 * 3600));
      Add_To_Sub_Second(Cal1, 0.025);
      Compute_Fields(Cal1);
      declare
         Str : String := Image(Cal1, "%% %a %A %b %B %d %D %e %H %I %j %k"
                               & " %l %m %M %p %r %S %T %U %w %W %y %Y %Z"
                               & " %F %f");
         Cmp : String := ("% Sun Sunday Oct October 28 10/28/01 28 01"
                          & " 01 301  1  1 10 27 AM 01:27:19 AM 19 01:27:19"
                          & " 44 1 43 01 2001 CDT 025000 025");
      begin
         if (Compare(Str, Cmp) /= 0)
         then
            Put_Line("Image not correct, was");
            Put_Line("'" & Str & "'");
            Put_Line(" but should have been");
            Put_Line("'" & Cmp & "'");
            raise Constraint_Error;
         end if;
      end;
   end Image_Tests;

   procedure Sloppy_Tests is
   begin
      Set_Tnum("Sloppy");
      begin
         -- Set it in the no-mans land and make sure it raises an exception
         Invalidate_Time(Cal1);
         Set_Timezone(Cal1, CST_Zone);
         Set_Year(Cal1, 2001);
         Set_Month(Cal1, 4);
         Set_Day_Of_Month(Cal1, 1);
         Set_Hour_Of_Day(Cal1, 2);
         Set_Minute(Cal1, 0);
         Set_Second(Cal1, 0);
         Compute_Fields(Cal1);
         Put_Line("Calcuate_fields didn't raise the exceptin properly");
         raise Constraint_Error;
      exception
         when Invalid_Date_Info => null;
      end;

      -- Set it in the no-mans land and calculate sloppily
      Set_Year(Cal1, 2001);
      Set_Month(Cal1, 4);
      Set_Day_Of_Month(Cal1, 1);
      Set_Hour_Of_Day(Cal1, 2);
      Set_Minute(Cal1, 0);
      Set_Second(Cal1, 0);
      Compute_Fields(Cal1, Sloppy => True);

      Ch_Year(Cal1, 2001, "1");
      Ch_Month(Cal1, 4, "1");
      Ch_Day_Of_Month(Cal1, 1, "1");
      Ch_Hour_Of_Day(Cal1, 3, "1");
      Ch_Minute(Cal1, 0, "1");
      Ch_Second(Cal1, 0, "1");

      -- Now setting a leap second when not valid
      Invalidate_Time(Cal1);
      Set_Do_Leap_Seconds(Cal1, False);
      Set_Timezone(Cal1, UTC_Zone);
      begin
         Set_Year(Cal1, 1973);
         Set_Month(Cal1, 12);
         Set_Day_Of_Month(Cal1, 31);
         Set_Hour_Of_Day(Cal1, 23);
         Set_Minute(Cal1, 59);
         Set_Second(Cal1, 60);
         Compute_Fields(Cal1);
         Put_Line("Compute_Fields didn't raise the exception on 61st second");
         raise Constraint_Error;
      exception
         when Invalid_Date_Info => null;
      end;

      -- Now set a leap second when valid.
      Invalidate_Time(Cal1);
      Set_Do_Leap_Seconds(Cal1, True);
      Set_Year(Cal1, 1973);
      Set_Month(Cal1, 12);
      Set_Day_Of_Month(Cal1, 31);
      Set_Hour_Of_Day(Cal1, 23);
      Set_Minute(Cal1, 59);
      Set_Second(Cal1, 60);
      Compute_Fields(Cal1);
      Cal2 := Cal1;

      begin
         -- Add a minute unsloppily, it should raise an exception
         Add_To_Minute(Cal1, 1);
         Put_Line("Add_To_Minute didn't raise an exception on leap second");
         raise Constraint_Error;
      exception
         when Invalid_Date_Info => null;
      end;

      -- Sloppily, it should work.
      Add_To_Minute(Cal2, 1, Sloppy => True);
      Ch_Minute(Cal2, 1, "2");
      Ch_Second(Cal2, 0, "2");

      Set_Do_Leap_Seconds(Cal1, False);
      Set_Do_Leap_Seconds(Cal2, False);
   end Sloppy_Tests;

   procedure Run_Tests(Zones : in Zone_Factory_Ptr) is
   begin
      CST_Zone := Find_Timezone(Zones.all, "CST6CDT");
      UTC_Zone := Find_Timezone(Zones.all, "UTC");
      Use_Zones := Zones;

      Basic_Tests;
      Week_Tests;
      DST_Tests;
      Timezone_Tests;
      Unix_Time_Tests;
      Leap_Seconds_Tests;
      Portable_Time_Tests;
      Diff_Tests;
      Image_Tests;
      Sloppy_Tests;
   end Run_Tests;
begin
   Asl.Date_Time.Register_Simple_Timezones(Default_Timezones);
   Simple_Zones := Default_Timezones;
   Default_Timezones := new Zone_Factory;
   Asl.Date_Time.Register_Complex_Timezones(Default_Timezones);
   Complex_Zones := Default_Timezones;

   Complex_Zones_Tests;

   Run_Tests(Simple_Zones);
   Run_Tests(Complex_Zones);

   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);
end Test_Calendar;
