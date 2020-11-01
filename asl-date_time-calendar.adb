-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
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
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Asl.Strings; use Asl.Strings;
with Interfaces.C; use type Interfaces.C.long;

package body Asl.Date_Time.Calendar is

   Secs_In_Day : constant Integer := 24 * 60 * 60;
   Secs_In_Hour : constant Integer := 60 * 60;
   Secs_In_Minute : constant Integer := 60;

   UTC_Zone : Time_Zone_Class := Allocate_No_DST_Zone("UTC", "UTC", 0);

   type Leap_Second_Rec is record
      Year   : Year_Range;
      Month  : Month_Range;
      Day    : Day_Of_Month_Range;
      Hour   : Hour_Of_Day_Range;
      Minute : Minute_Range;
      Second : Second_Range;
      Offset : Integer;
   end record;

   -- These are the official leap seconds as of 2001
   Leap_Seconds : array(Positive range <>) of Leap_Second_Rec :=
     ((1972,  6, 30, 23, 59, 60, 1),
      (1972, 12, 31, 23, 59, 60, 1),
      (1973, 12, 31, 23, 59, 60, 1),
      (1974, 12, 31, 23, 59, 60, 1),
      (1975, 12, 31, 23, 59, 60, 1),
      (1976, 12, 31, 23, 59, 60, 1),
      (1977, 12, 31, 23, 59, 60, 1),
      (1978, 12, 31, 23, 59, 60, 1),
      (1979, 12, 31, 23, 59, 60, 1),
      (1981,  6, 30, 23, 59, 60, 1),
      (1982,  6, 30, 23, 59, 60, 1),
      (1983,  6, 30, 23, 59, 60, 1),
      (1985,  6, 30, 23, 59, 60, 1),
      (1987, 12, 31, 23, 59, 60, 1),
      (1989, 12, 31, 23, 59, 60, 1),
      (1990, 12, 31, 23, 59, 60, 1),
      (1992,  6, 30, 23, 59, 60, 1),
      (1993,  6, 30, 23, 59, 60, 1),
      (1994,  6, 30, 23, 59, 60, 1),
      (1995, 12, 31, 23, 59, 60, 1),
      (1997,  6, 30, 23, 59, 60, 1),
      (1998, 12, 31, 23, 59, 60, 1));

   function Cmp_Leap_Secs(Idx    : in Integer;
                          Cal    : in Calendar_Time)
                          return Integer is
   begin
      if (Cal.Year /= Leap_Seconds(Idx).Year) then
         if (Cal.Year > Leap_Seconds(Idx).Year) then return 1;
         elsif (Cal.Year < Leap_Seconds(Idx).Year) then return -1;
         end if;
      end if;

      if (Cal.Month /= Leap_Seconds(Idx).Month) then
         if (Cal.Month > Leap_Seconds(Idx).Month) then return 1;
         elsif (Cal.Month < Leap_Seconds(Idx).Month) then return -1;
         end if;
      end if;

      if (Cal.Day_Of_Month /= Leap_Seconds(Idx).Day) then
         if (Cal.Day_Of_Month > Leap_Seconds(Idx).Day) then return 1;
         elsif (Cal.Day_Of_Month < Leap_Seconds(Idx).Day) then return -1;
         end if;
      end if;

      if (Cal.Hour_Of_Day /= Leap_Seconds(Idx).Hour) then
         if (Cal.Hour_Of_Day > Leap_Seconds(Idx).Hour) then return 1;
         elsif (Cal.Hour_Of_Day < Leap_Seconds(Idx).Hour) then return -1;
         end if;
      end if;

      if (Cal.Minute /= Leap_Seconds(Idx).Minute) then
         if (Cal.Minute > Leap_Seconds(Idx).Minute) then return 1;
         elsif (Cal.Minute < Leap_Seconds(Idx).Minute) then return -1;
         end if;
      end if;

      if (Cal.Second > Leap_Seconds(Idx).Second) then return 1;
      elsif (Cal.Second < Leap_Seconds(Idx).Second) then return -1;
      else return 0;
      end if;
   end Cmp_Leap_Secs;

   procedure Conv_To_UTC(Cal : in out Calendar_Time) is
      Secs : Integer := Cal.DST_Offset + Cal.UTC_Offset;
   begin
      -- Turn off leap seconds for these calculations, because they shouldn't
      -- matter and it saves some effort.
      Cal.Do_Leap_Seconds := False;

      -- We add to minutes here because we don't want it messing up the
      -- seconds field (and doing all the leap second checking there).
      -- I'm sure some political entity was stupid enough to have a
      -- UTC or DST offset that wasn't an even number of minutes, but
      -- they won't have working leap seconds.
      Add_To_Minute(Cal, Secs / Secs_In_Minute, False);
      Secs := Secs mod Secs_In_Minute;
      if (Secs /= 0) then
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Cannot do leap seconds on time with a UTC/DST"
                         & " offset that is not an even number of minutes");
      end if;
   end Conv_To_UTC;

   -- Return the number of seconds that were added leap seconds (some
   -- time was added to the minute) minus the number of seconds that
   -- were subtracted leap seconds (minute had less than 60 seconds).
   function Leap_Secs_In_Range(Cal1, Cal2 : in Calendar_Time)
                               return Integer is
      T_Cal1 : Calendar_Time := Cal1;
      T_Cal2 : Calendar_Time := Cal2;
      Rv     : Integer;
      I1     : Integer;
      I2     : Integer;
   begin
      Conv_To_UTC(T_Cal1);
      Conv_To_UTC(T_Cal2);
      I1 := Leap_Seconds'First - 1;
      while (I1+1 <= Leap_Seconds'Last) loop
         exit when (Cmp_Leap_Secs(I1+1, T_Cal1) <= 0);
         I1 := I1 + 1;
      end loop;

      I2 := Leap_Seconds'First - 1;
      while (I2+1 <= Leap_Seconds'Last) loop
         exit when (Cmp_Leap_Secs(I2+1, T_Cal2) <= 0);
         I2 := I2 + 1;
      end loop;

      Rv := 0;
      if (I1 > I2) then
         for I in I2+1 .. I1 loop
            Rv := Rv - Leap_Seconds(I).Offset;
         end loop;
      else
         for I in I1+1 .. I2 loop
            Rv := Rv + Leap_Seconds(I).Offset;
         end loop;
      end if;

      return Rv;
   end Leap_Secs_In_Range;

   function Find_Secs_In_Minute(Cal : in Calendar_Time)
                                return Integer is
      T_Cal : Calendar_Time := Cal;
   begin
      if (not Cal.Do_Leap_Seconds) then
         return 60;
      end if;

      Conv_To_UTC(T_Cal);

      for I in Leap_Seconds'Range loop
         if ((T_Cal.Year = Leap_Seconds(I).Year)
             and (T_Cal.Month = Leap_Seconds(I).Month)
             and (T_Cal.Day_Of_Month = Leap_Seconds(I).Day)
             and (T_Cal.Hour_Of_Day = Leap_Seconds(I).Hour)
             and (T_Cal.Minute = Leap_Seconds(I).Minute))
         then
            return 60 + Leap_Seconds(I).Offset;
         end if;
      end loop;

      return 60;
   end Find_Secs_In_Minute;

   procedure Year_Day_To_Week(Cal : in Calendar_Time;
                              Y   : in Year_Range;
                              M   : in Month_Range;
                              D   : in Day_Of_Year_Range;
                              Md  : in Day_Of_Month_Range;
                              Wd  : out Weekday_Range;
                              Wm  : out Week_Of_Month_Range;
                              Wy  : out Week_Of_Year_Range) is
      C_Weekday : Weekday_Range;
      V         : Natural;
   begin
      -- Calculate the day of the week based upon the reference year (whose
      -- first day is Sunday).
      C_Weekday := Weekday_From_Year_Day(Y, D);

      -- Now calculate the week of the month and year.  The week containing
      -- the first day of the month/year is considered to be the first week
      -- of the month/year if it has at least 4 days in the month/year.
      -- Otherwise, the first week is the week after the one containing
      -- the first day of the year/month.
      Wd := C_Weekday;
      V := ((Md + 3 +
             ((7 - C_Weekday + (Cal.First_Weekday - 1)) mod 7))
            / 7);
      if (V = 0) then
         -- This means the day goes with the previous month's week, so
         -- move it back.
         if (M = 1) then
            V := ((Md + 3 + Days_In_Month(Y - 1, 12)
                   + ((7 - C_Weekday + (Cal.First_Weekday - 1)) mod 7))
                  / 7);
         else
            V := ((Md + 3 + Days_In_Month(Y, M - 1)
                   + ((7 - C_Weekday + (Cal.First_Weekday - 1)) mod 7))
                  / 7);
         end if;
      end if;
      Wm := V;

      V := ((D + 3 +
             ((7 - C_Weekday + (Cal.First_Weekday - 1)) mod 7))
            / 7);
      if (V = 0) then
         -- This means the week goes with the previous year, so recalculate
         -- in the previous year.
         V := ((D + 3 + Days_In_Year(Y - 1)
                + ((7 - C_Weekday + (Cal.First_Weekday - 1)) mod 7))
               / 7);
      end if;
      Wy := V;
   end Year_Day_To_Week;

   procedure Check_Leap_Second(Cal    : in out Calendar_Time;
                               Sloppy : in Boolean) is
      Tmp : Integer;
   begin
      Tmp := Find_Secs_In_Minute(Cal);
      if (Cal.Second >= Tmp) then
         if (Sloppy) then
            Add_To_Minute(Cal, 1, False);
            Cal.Second := 0;
         else
            Raise_Exception(Invalid_Date_Info'Identity,
                            "Time moved into a leap-second no-mans-land");
         end if;
      end if;
   end Check_Leap_Second;

   procedure Compute_Fields(Cal    : in out Calendar_Time;
                            Sloppy : in Boolean := False) is
      Tmp        : Integer;
      DST_Active : DST_State;
   begin
      if (not Cal.Year_Valid) then
         -- At least the year must be valid.
         Raise_Exception(Invalid_Date_Info'Identity, "Year not valid");
      end if;

      if (Cal.Day_Of_Month_Valid and Cal.Month_Valid) then
         -- Calculate from the month and day of the month.
         Cal.Day_Of_Year := Month_Day_To_Year_Day(Cal.Year,
                                                  Cal.Month,
                                                  Cal.Day_Of_Month);
      elsif (Cal.Week_Of_Year_Valid and Cal.Weekday_Valid)
      then
         -- Calculate from the week/day of week.

         -- Calculate the first day of week of the year, ie what weekday
         -- is Jan 1 of the year.  We calculate it so the first day of
         -- the week is a zero here, because that eases the later
         -- calculations.
         Tmp := Weekday_From_Year_Day(Cal.Year, 1) - 1;
         if (Tmp <= 3) then
            -- The first day of the year is in the first week, so subtract
            -- the stuff from the first week that is not in this year.
            Tmp := -Tmp;
         else
            -- The first day of the year is not in the first week, so
            -- add the days from this year that are before the first
            -- week.
            Tmp := 7 - Tmp;
         end if;

         Cal.Day_Of_Year := ((7 * Cal.Week_Of_Year)
                             - (7 - Cal.Weekday)
                             + Tmp);
      elsif (Cal.Month_Valid
             and Cal.Week_Of_Month_Valid
             and Cal.Weekday_Valid)
      then
         -- Calculate from the month/week of month/day of week.

         -- Calculate the first day of week of the month, ie what weekday
         -- is day 1 of the month.  We calculate it so the first day of
         -- the week is a zero here.
         Tmp := Weekday_From_Month_Day(Cal.Year, Cal.Month, 1) - 1;
         if (Tmp <= 3) then
            -- The first day of the month is in the first week, so subtract
            -- the stuff from the first week that is not in this month.
            Tmp := -Tmp;
         else
            -- The first day of the month is not in the first week, so
            -- add the days from this month that are before the first
            -- week.
            Tmp := 7 - Tmp;
         end if;

         Cal.Day_Of_Year := ((7 * Cal.Week_Of_Year)
                             - (7 - Cal.Weekday)
                             + Tmp);
      elsif Cal.Day_Of_Year_Valid then
         null; -- This is the field we really want.
      else
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Cannot determine day of year, need either"
                         & " Month and Day_Of_Month, Week_Of_Year and"
                         & " Weekday, or Month, Week_Of_Month and"
                         & " Weekday to be valid");
      end if;

      -- At this point in time, we have a valid year and day of the year.
      -- Now calculate everything else that is day-relative.

      -- Get the month and day of the month.
      Year_Day_To_Month_Day(Cal.Year,
                            Cal.Day_Of_Year,
                            Cal.Month,
                            Cal.Day_Of_Month);

      -- Get the various week-related things.
      Year_Day_To_Week(Cal,
                       Cal.Year,
                       Cal.Month,
                       Cal.Day_Of_Year,
                       Cal.Day_Of_Month,
                       Cal.Weekday,
                       Cal.Week_Of_Month,
                       Cal.Week_Of_Year);

      if ((Cal.Hour_Of_Day_Valid
           or (Cal.Hour_Valid and Cal.AM_PM_Valid))
          and Cal.Minute_Valid
          and Cal.Second_Valid)
      then
         if (not Cal.Hour_Of_Day_Valid) then
            if (Cal.Hour = 12) then
               Cal.Hour_Of_Day := 0;
            else
               Cal.Hour_Of_Day := Cal.Hour;
            end if;
            if (Cal.AM_PM = PM) then
               Cal.Hour_Of_Day := Cal.Hour_Of_Day + 12;
            end if;
         else
            if ((Cal.Hour_Of_Day = 0) or (Cal.Hour_Of_Day = 12)) then
               Cal.Hour := 12;
            else
               Cal.Hour := Cal.Hour_Of_Day mod 12;
            end if;
            if (Cal.Hour_Of_Day >= 12) then
               Cal.AM_PM := PM;
            else
               Cal.AM_PM := AM;
            end if;
         end if;

         Cal.Can_Calculate := True;

         Cal.UTC_Offset := Get_UTC_Offset(Cal.Zone.all,
                                          Cal.Year,
                                          Cal.Day_Of_Year,
                                          Cal.Hour_Of_Day,
                                          Cal.Minute,
                                          Cal.Second);
         Daylight_Savings_Active(DST_Active,
                                 Cal.DST_Offset,
                                 Cal.Zone.all,
                                 Cal.Year,
                                 Cal.Day_Of_Year,
                                 Cal.Hour_Of_Day,
                                 Cal.Minute,
                                 Cal.Second);
         case DST_Active is
            when On =>  Cal.DST_Active := True;

            when Off => Cal.DST_Active := False;

            when No_Mans_Land =>
               if (Sloppy) then
                  Add_To_Second(Cal, Cal.DST_Offset);
               else
                  Raise_Exception(Invalid_Date_Info'Identity,
                                  "Time falls into range where daylight savings"
                                  & " has 'lept forward' over the time, and thus"
                                  & " is not a valid time");
               end if;

            when Overlay =>
               -- If not set, turn it on, otherwise leave it alone.
               if (not Cal.DST_Active_Valid) then
                  Cal.DST_Active := True;
               end if;
               if (not Cal.DST_Active) then
                  Cal.DST_Offset := 0;
               end if;
         end case;
      else
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Time of day not valid, need Seconds, Minutes,"
                         & " and either Hours_Of_Day or Hours and AM_PM"
                         & " to be valid");
      end if;

      Check_Leap_Second(Cal, Sloppy);

      -- If microseconds are not specified, assume they are zero.
      if (not Cal.Sub_Second_Valid) then
         Cal.Sub_Second := 0.0;
      end if;

      Cal.Year_Valid := True;
      Cal.Month_Valid := True;
      Cal.Week_Of_Year_Valid := True;
      Cal.Week_Of_Month_Valid := True;
      Cal.Day_Of_Year_Valid := True;
      Cal.Day_Of_Month_Valid := True;
      Cal.Weekday_Valid := True;
      Cal.Hour_Of_Day_Valid := True;
      Cal.AM_PM_Valid := True;
      Cal.Hour_Valid := True;
      Cal.Minute_Valid := True;
      Cal.Second_Valid := True;
      Cal.Sub_Second_Valid := True;
      Cal.DST_Active_Valid := True;
   end Compute_Fields;

   procedure Set_First_Weekday(Cal : in out Calendar_Time;
                               Val : in First_Weekday_Range) is
   begin
      Cal.First_Weekday := Val;
      Cal.Week_Of_Month_Valid := False;
      Cal.Week_Of_Year_Valid := False;
   end Set_First_Weekday;

   function Get_First_Weekday(Cal : in Calendar_Time)
                              return First_Weekday_Range is
   begin
      return Cal.First_Weekday;
   end Get_First_Weekday;

   procedure Set_Year(Cal  : in out Calendar_Time;
                      Year : in Year_Range) is
   begin
      Cal.Year := Year;
      Cal.Year_Valid := True;

      Cal.Week_Of_Year_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.Day_Of_Year_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Year;

   procedure Set_Month(Cal   : in out Calendar_Time;
                       Month : in Month_Range) is
   begin
      Cal.Month := Month;
      Cal.Month_Valid := True;

      Cal.Week_Of_Year_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.Day_Of_Year_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Month;

   procedure Set_Short_Month(Cal   : in out Calendar_Time;
                             Month : in Short_Month) is
      V : Month_Range := Short_Month_To_Month_Range(Month);
   begin
      Set_Month(Cal, V);
   end Set_Short_Month;

   procedure Set_Long_Month(Cal   : in out Calendar_Time;
                            Month : in Long_Month) is
      V : Month_Range := Long_Month_To_Month_Range(Month);
   begin
      Set_Month(Cal, V);
   end Set_Long_Month;

   procedure Set_Week_Of_Year(Cal          : in out Calendar_Time;
                              Week_Of_Year : in Week_Of_Year_Range) is
   begin
      Cal.Week_Of_Year := Week_Of_Year;
      Cal.Week_Of_Year_Valid := True;

      Cal.Month_Valid := False;
      Cal.Day_Of_Year_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.Day_Of_Month_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Week_Of_Year;

   procedure Set_Week_Of_Month(Cal           : in out Calendar_Time;
                               Week_Of_Month : in Week_Of_Month_Range) is
   begin
      Cal.Week_Of_Month := Week_Of_Month;
      Cal.Week_Of_Month_Valid := True;

      Cal.Day_Of_Year_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.Day_Of_Month_Valid := False;
      Cal.Week_Of_Year_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Week_Of_Month;

   procedure Set_Day_Of_Year(Cal : in out Calendar_Time;
                             Day : in Day_Of_Year_Range) is
   begin
      Cal.Day_Of_Year := Day;
      Cal.Day_Of_Year_Valid := True;

      Cal.Month_Valid := False;
      Cal.Week_Of_Year_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.Day_Of_Month_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Day_Of_Year;

   procedure Set_Day_Of_Month(Cal : in out Calendar_Time;
                              Day : in Day_Of_Month_Range) is
   begin
      Cal.Day_Of_Month := Day;
      Cal.Day_Of_Month_Valid := True;

      Cal.Week_Of_Year_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.Day_Of_Year_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Day_Of_Month;

   procedure Set_Weekday(Cal     : in out Calendar_Time;
                         Weekday : in Weekday_Range) is
   begin
      Cal.Weekday := Weekday;
      Cal.Weekday_Valid := True;

      Cal.Day_Of_Month_Valid := False;
      Cal.Week_Of_Year_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.Day_Of_Year_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Weekday;

   procedure Set_Short_Weekday(Cal     : in out Calendar_Time;
                               Weekday : in Short_Weekday) is
      V : Weekday_Range := Short_Weekday_To_Weekday_Range(Weekday);
   begin
      Set_Weekday(Cal, V);
   end Set_Short_Weekday;

   procedure Set_Long_Weekday(Cal     : in out Calendar_Time;
                              Weekday : in Long_Weekday) is
      V : Weekday_Range := Long_Weekday_To_Weekday_Range(Weekday);
   begin
      Set_Weekday(Cal, V);
   end Set_Long_Weekday;

   procedure Set_DST_Active(Cal    : in out Calendar_Time;
                            Active : in Boolean) is
   begin
      Cal.DST_Active := Active;
      Cal.DST_Active_Valid := True;

      -- Turning DST on/off won't really affect anything else, generally.
   end Set_DST_Active;

   procedure Set_Hour_Of_Day(Cal  : in out Calendar_Time;
                             Hour : in Hour_Of_Day_Range) is
   begin
      Cal.Hour_Of_Day := Hour;
      if (Hour = 0) then
         Cal.Hour := 12;
      else
         Cal.Hour := Hour mod 12;
      end if;
      if (Hour >= 12) then
         Cal.AM_PM := PM;
      else
         Cal.AM_PM := AM;
      end if;
      Cal.Hour_Of_Day_Valid := True;
      Cal.Hour_Valid := True;
      Cal.AM_PM_Valid := True;

      Cal.Hour_Valid := False;
      Cal.AM_PM_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Hour_Of_Day;

   procedure Set_Hour(Cal  : in out Calendar_Time;
                      Hour : in Hour_Range) is
   begin
      Cal.Hour := Hour;
      if Cal.AM_PM_Valid then
         if (Hour = 12) then
            Cal.Hour_Of_Day := 0;
         else
            Cal.Hour_Of_Day := Cal.Hour;
         end if;
         if (Cal.AM_PM = PM) then
            Cal.Hour_Of_Day := Cal.Hour_Of_Day + 12;
         end if;
         Cal.Hour_Of_Day_Valid := True;
      end if;
      Cal.Hour_Valid := True;

      Cal.Hour_Of_Day_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Hour;

   procedure Set_AM_PM(Cal   : in out Calendar_Time;
                       AM_PM : in AM_PM_Range) is
   begin
      Cal.AM_PM := AM_PM;
      if Cal.Hour_Valid then
         if (Cal.Hour = 12) then
            Cal.Hour_Of_Day := 0;
         else
            Cal.Hour_Of_Day := Cal.Hour;
         end if;
         if (AM_PM = PM) then
            Cal.Hour_Of_Day := Cal.Hour_Of_Day + 12;
         end if;
         Cal.Hour_Of_Day_Valid := True;
      end if;
      Cal.AM_PM_Valid := True;

      Cal.Hour_Of_Day_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_AM_PM;

   procedure Set_Minute(Cal    : in out Calendar_Time;
                        Minute : in Minute_Range) is
   begin
      Cal.Minute := Minute;
      Cal.Minute_Valid := True;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Minute;

   procedure Set_Second(Cal    : in out Calendar_Time;
                        Second : in Second_Range) is
   begin
      Cal.Second := Second;
      Cal.Second_Valid := True;
      Cal.DST_Active_Valid := False;
      Cal.Can_Calculate := False;
   end Set_Second;

   procedure Set_Sub_Second(Cal        : in out Calendar_Time;
                            Sub_Second : in Sub_Second_Duration) is
   begin
      Cal.Sub_Second := Sub_Second;
      Cal.Sub_Second_Valid := True;
   end Set_Sub_Second;

   procedure Check_DST_Value(Cal    : in out Calendar_Time;
                             Sloppy : in Boolean) is
      Old_DST_Offset : Integer := Cal.DST_Offset;
      Old_DST_Active : Boolean := Cal.DST_Active;
      Old_UTC_Offset : Integer := Cal.UTC_Offset;
      DST_Active     : DST_State;
   begin
      Cal.UTC_Offset := Get_UTC_Offset(Cal.Zone.all,
                                       Cal.Year,
                                       Cal.Day_Of_Year,
                                       Cal.Hour_Of_Day,
                                       Cal.Minute,
                                       Cal.Second);
      if (Cal.UTC_Offset /= Old_UTC_Offset) then
         -- We changed UTC offsets, so adjust the time.
         -- FIXME - this will cause wierd problems with times, but I'm not
         -- sure how to fix it.  You probably shouldn't do time arithmetic
         -- like this in this case.
         Add_To_Second(Cal, Cal.UTC_Offset - Old_UTC_Offset);
      end if;

      Daylight_Savings_Active(DST_Active,
                              Cal.DST_Offset,
                              Cal.Zone.all,
                              Cal.Year,
                              Cal.Day_Of_Year,
                              Cal.Hour_Of_Day,
                              Cal.Minute,
                              Cal.Second);
      if (DST_Active = No_Mans_Land) then
         -- If we land in no man's land, then add the DST offset if sloppy.
         -- This should also turn on the DST_Active value.
         if (Sloppy) then
            Add_To_Second(Cal, Cal.DST_Offset - Old_DST_Offset, False);
            Cal.DST_Active := True;
         else
            Raise_Exception(Invalid_Date_Info'Identity,
                            "Date moved into DST no-man's land");
         end if;
      elsif ((not Old_DST_Active) and (Dst_Active = Overlay)) then
         -- We are in an overlay, and the previous DST was not active
         -- so leave it inactive and don't change the time;
         Cal.DST_Active := False;
      elsif (Old_DST_Active /= (DST_Active /= Off)) then
         -- If we changed DST states, then we need to add the differences
         -- between the old and the new DST offsets.
         Cal.DST_Active := DST_Active /= Off;
         Add_To_Second(Cal, Cal.DST_Offset - Old_DST_Offset, False);
      end if;
      Cal.DST_Active_Valid := True;
   end Check_DST_Value;

   procedure Handle_Month_Day(Cal    : in out Calendar_Time;
                              Sloppy : in Boolean) is
      Tmp : Integer;
   begin
      Tmp := Days_In_Month(Cal.Year, Cal.Month);
      if (Cal.Day_Of_Month > Tmp) then
         if Sloppy then
            Cal.Day_Of_Month := Tmp;
         else
            Raise_Exception(Invalid_Date_Info'Identity,
                            "Time move caused an invalid day of month");
         end if;
      end if;

      -- We use the month/day, not the day of year, information when we
      -- add a year.
      Cal.Day_Of_Year := Month_Day_To_Year_Day(Cal.Year,
                                               Cal.Month,
                                               Cal.Day_Of_Month);
   end Handle_Month_Day;

   procedure Add_To_Year(Cal    : in out Calendar_Time;
                         To_Add : in Integer;
                         Recalc : in Boolean := True;
                         Sloppy : in Boolean := False) is
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Set_Year(Cal, Cal.Year + To_Add);

      Handle_Month_Day(Cal, Sloppy);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Recalc) then
         Check_DST_Value(Cal, Sloppy);
         if (Cal.Do_Leap_Seconds) then
            Check_Leap_Second(Cal, Sloppy);
         end if;
      end if;
   end Add_To_Year;

   procedure Add_To_Month(Cal    : in out Calendar_Time;
                          To_Add : in Integer;
                          Recalc : in Boolean := True;
                          Sloppy : in Boolean := False) is
      Tmp : Integer;
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Tmp := Cal.Month + To_Add;
      while (Tmp < 1) loop
         Tmp := Tmp + 12;
         Cal.Year := Cal.Year - 1;
      end loop;
      while (Tmp > 12) loop
         Tmp := Tmp - 12;
         Cal.Year := Cal.Year + 1;
      end loop;
      Set_Month(Cal, Tmp);

      Handle_Month_Day(Cal, Sloppy);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Recalc) then
         Check_DST_Value(Cal, Sloppy);

         if (Cal.Do_Leap_Seconds) then
            Check_Leap_Second(Cal, Sloppy);
         end if;
      end if;
   end Add_To_Month;

   procedure Add_To_Day_Of_Year(Cal    : in out Calendar_Time;
                                To_Add : in Integer;
                                Recalc : in Boolean := True;
                                Sloppy : in Boolean := False) is
      Tmp, Tmp2 : Integer;
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Tmp := Cal.Day_Of_Year + To_Add;
      while (Tmp < 1) loop
         Tmp2 := Days_In_Year(Cal.Year-1);
         Tmp := Tmp + Tmp2;
         Add_To_Year(Cal, -1, False, Sloppy);
      end loop;
      Tmp2 := Days_In_Year(Cal.Year);
      while (Tmp > Tmp2) loop
         Tmp := Tmp - Tmp2;
         Add_To_Year(Cal, 1, False, Sloppy);
         Tmp2 := Days_In_Year(Cal.Year);
      end loop;
      Set_Day_Of_Year(Cal, Tmp);

      -- Recalculate the month information.
      Year_Day_To_Month_Day(Cal.Year, Cal.Day_Of_Year,
                            Cal.Month, Cal.Day_Of_Month);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Recalc) then
         Check_DST_Value(Cal, Sloppy);

         if (Cal.Do_Leap_Seconds) then
            Check_Leap_Second(Cal, Sloppy);
         end if;
      end if;
   end Add_To_Day_Of_Year;

   procedure Add_To_Hour_Of_Day(Cal    : in out Calendar_Time;
                                To_Add : in Integer;
                                Recalc : in Boolean := True;
                                Sloppy : in Boolean := False) is
      Tmp : Integer;
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Tmp := Cal.Hour_Of_Day + To_Add;
      if (Tmp >= 24) then
         Add_To_Day_Of_Year(Cal, Tmp / 24, False, Sloppy);
         Tmp := Tmp mod 24;
      elsif (Tmp < 0) then
         Add_To_Day_Of_Year(Cal, (Tmp-23) / 24, False, Sloppy);
         Tmp := Tmp mod 24;
      end if;
      Set_Hour_Of_Day(Cal, Tmp);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Recalc) then
         Check_DST_Value(Cal, True);

         if (Cal.Do_Leap_Seconds) then
            Check_Leap_Second(Cal, Sloppy);
         end if;
      end if;
   end Add_To_Hour_Of_Day;

   procedure Add_To_Minute(Cal    : in out Calendar_Time;
                           To_Add : in Integer;
                           Recalc : in Boolean := True;
                           Sloppy : in Boolean := False) is
      Tmp : Integer;
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Tmp := Cal.Minute + To_Add;
      if (Tmp >= 60) then
         Add_To_Hour_Of_Day(Cal, Tmp / 60, False, True);
         Tmp := Tmp mod 60;
      elsif (Tmp < 0) then
         Add_To_Hour_Of_Day(Cal, (Tmp-59) / 60, False, True);
         Tmp := Tmp mod 60;
      end if;
      Set_Minute(Cal, Tmp);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Recalc) then
         Check_DST_Value(Cal, True);

         if (Cal.Do_Leap_Seconds) then
            Check_Leap_Second(Cal, Sloppy);
         end if;
      end if;
   end Add_To_Minute;

   procedure Add_To_Second(Cal    : in out Calendar_Time;
                           To_Add : in Integer;
                           Recalc : in Boolean := True) is
      Tmp               : Integer;
      Start_Cal         : Calendar_Time := Cal;
      Minute_Length     : Integer;
      Orig_Do_Leap_Secs : Boolean := Cal.Do_Leap_Seconds;
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      -- We don't want leap seconds messing up these next calculations.  We'll handle
      -- them a little later.
      Cal.Do_Leap_Seconds := False;

      Tmp := Cal.Second + To_Add;
      if (Tmp >= Secs_In_Minute) then
         Add_To_Minute(Cal, Tmp / 60, False, True);
         Tmp := Tmp mod 60;
      elsif (Tmp < 0) then
         Add_To_Minute(Cal, (Tmp-59) / 60, False, True);
         Tmp := Tmp mod 60;
      end if;
      Set_Second(Cal, Tmp);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Orig_Do_Leap_Secs) then
         Cal.Do_Leap_Seconds := True;

         -- Account for leap seconds here.  This works recursively, but
         -- is always guaranteed to complete as long as the leap seconds
         -- are not within a few seconds of each other (should never
         -- happen).  This calculates the number of leap seconds between
         -- the two points.  If it is down to one, it does special
         -- handling to get to the last second of the minute, even if
         -- the minute has an unusual number of seconds.

         Minute_Length := Find_Secs_In_Minute(Cal);
         Tmp := -Leap_Secs_In_Range(Start_Cal, Cal);
         if (Tmp = 1) then
            if (Cal.Second >= Minute_Length) then
               Add_To_Minute(Cal, 1, False);
               Cal.Second := 0;
            else
               Cal.Second := Cal.Second + 1;
            end if;
         elsif (Tmp = -1) then
            if (Cal.Second = 0) then
               Add_To_Minute(Cal, -1, False);
               Cal.Second := Find_Secs_In_Minute(Cal) - 1;
            else
               Cal.Second := Cal.Second - 1;
            end if;
         elsif (Tmp /= 0) then
            Add_To_Second(Cal, Tmp);
         end if;
      end if;

      if (Recalc) then
         Check_DST_Value(Cal, True);
      end if;
   exception
      when others =>
         Cal.Do_Leap_Seconds := Orig_Do_Leap_Secs;
   end Add_To_Second;

   procedure Add_To_Sub_Second(Cal    : in out Calendar_Time;
                               To_Add : in Duration;
                               Recalc : in Boolean := True) is
      Tmp : Duration;
   begin
      if (not Cal.Can_Calculate) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Tmp := Cal.Sub_Second + To_Add;
      while (Tmp < 0.0) loop
         Add_To_Second(Cal, -1, False);
         Tmp := Tmp - 1_000_000.0;
      end loop;
      while (Tmp >= 1.0) loop
         Add_To_Second(Cal, 1, False);
         Tmp := Tmp + 1_000_000.0;
      end loop;
      Set_Sub_Second(Cal, Tmp);

      -- All calculations put the time back into a state where further
      -- calculations can be done.
      Cal.Can_Calculate := True;

      if (Recalc) then
         Check_DST_Value(Cal, True);
      end if;
   end Add_To_Sub_Second;

   function "+"(Cal : in Calendar_Time; To_Add : in Duration)
                return Calendar_Time is
      V       : Duration;
      Seconds : Integer;
      Rv      : Calendar_Time := Cal;
   begin
      -- Ada rounds, but we want truncation.
      V := Duration(Integer(To_Add));
      if (V > To_Add) then
         V := Duration(Integer(To_Add)-1);
      end if;
      Seconds := Integer(V);

      Add_To_Second(Rv, Seconds);
      Add_To_Sub_Second(Rv, To_Add - V, False);

      return Rv;
   end "+";

   function "-"(Cal : in Calendar_Time; To_Add : in Duration)
                return Calendar_Time is
   begin
      return Cal + (-To_Add);
   end "-";

   function "-"(Cal1, Cal2 : in Calendar_Time)
                return Duration is
      T_Cal1  : Calendar_Time := Cal1;
      T_Cal2  : Calendar_Time := Cal2;
      Days    : Integer;
      Seconds : Integer;
      Rv      : Duration;
   begin
      if ((not Cal1.Can_Calculate) or (not Cal2.Can_Calculate)) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      -- Converting to UTC makes the calculations easy.
      Set_Timezone(T_Cal1, UTC_Zone);
      Set_Timezone(T_Cal2, UTC_Zone);
      Set_Do_Leap_Seconds(T_Cal1, False);
      Set_Do_Leap_Seconds(T_Cal2, False);

      Days := Days_Between_Years(T_Cal1.Year, T_Cal2.Year);
      Days := Days + (T_Cal1.Day_Of_Year - T_Cal2.Day_Of_Year);
      Seconds := (T_Cal1.Hour_Of_Day * 3600) - (T_Cal2.Hour_Of_Day * 3600);
      Seconds := Seconds + (T_Cal1.Minute * 60) - (T_Cal2.Minute * 60);
      Seconds := Seconds + T_Cal1.Second - T_Cal2.Second;
      Rv := Duration(Seconds + (Days * Secs_In_Day));
      Rv := Rv + (T_Cal1.Sub_Second - T_Cal2.Sub_Second);

      return Rv;
   end "-";

   function "="(P_Cal1, P_Cal2 : in Calendar_Time)
                return Boolean is
      Cal1 : Calendar_Time := P_Cal1;
      Cal2 : Calendar_Time := P_Cal2;
   begin
      if ((not Cal1.Can_Calculate) or (not Cal2.Can_Calculate)) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Set_Timezone(Cal1, UTC_Zone);
      Set_Timezone(Cal2, UTC_Zone);
      Set_Do_Leap_Seconds(Cal1, False);
      Set_Do_Leap_Seconds(Cal2, False);

      return ((Cal1.Year = Cal2.Year)
              and (Cal1.Day_Of_Year = Cal2.Day_Of_Year)
              and (Cal1.Hour_Of_Day = Cal2.Hour_Of_Day)
              and (Cal1.Minute = Cal2.Minute)
              and (Cal1.Second = Cal2.Second)
              and (Cal1.Sub_Second = Cal2.Sub_Second));
   end "=";

   function ">"(P_Cal1, P_Cal2 : in Calendar_Time)
                return Boolean is
      Cal1 : Calendar_Time := P_Cal1;
      Cal2 : Calendar_Time := P_Cal2;
   begin
      if ((not Cal1.Can_Calculate) or (not Cal2.Can_Calculate)) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Set_Timezone(Cal1, UTC_Zone);
      Set_Timezone(Cal2, UTC_Zone);
      Set_Do_Leap_Seconds(Cal1, False);
      Set_Do_Leap_Seconds(Cal2, False);

      if (Cal1.Year /= Cal2.Year) then
         return Cal1.Year > Cal2.Year;
      end if;

      if (Cal1.Day_Of_Year /= Cal2.Day_Of_Year) then
         return Cal1.Day_Of_Year > Cal2.Day_Of_Year;
      end if;

      if (Cal1.Hour_Of_Day /= Cal2.Hour_Of_Day) then
         return Cal1.Hour_Of_Day > Cal2.Hour_Of_Day;
      end if;

      if (Cal1.Minute /= Cal2.Minute) then
         return Cal1.Minute > Cal2.Minute;
      end if;

      if (Cal1.Second /= Cal2.Second) then
         return Cal1.Second > Cal2.Second;
      end if;

      return Cal1.Sub_Second > Cal2.Sub_Second;
   end ">";

   function ">="(P_Cal1, P_Cal2 : in Calendar_Time)
                return Boolean is
      Cal1 : Calendar_Time := P_Cal1;
      Cal2 : Calendar_Time := P_Cal2;
   begin
      if ((not Cal1.Can_Calculate) or (not Cal2.Can_Calculate)) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Set_Timezone(Cal1, UTC_Zone);
      Set_Timezone(Cal2, UTC_Zone);
      Set_Do_Leap_Seconds(Cal1, False);
      Set_Do_Leap_Seconds(Cal2, False);

      if (Cal1.Year /= Cal2.Year) then
         return Cal1.Year > Cal2.Year;
      end if;

      if (Cal1.Day_Of_Year /= Cal2.Day_Of_Year) then
         return Cal1.Day_Of_Year > Cal2.Day_Of_Year;
      end if;

      if (Cal1.Hour_Of_Day /= Cal2.Hour_Of_Day) then
         return Cal1.Hour_Of_Day > Cal2.Hour_Of_Day;
      end if;

      if (Cal1.Minute /= Cal2.Minute) then
         return Cal1.Minute > Cal2.Minute;
      end if;

      if (Cal1.Second /= Cal2.Second) then
         return Cal1.Second > Cal2.Second;
      end if;

      return Cal1.Sub_Second >= Cal2.Sub_Second;
   end ">=";

   function "<"(P_Cal1, P_Cal2 : in Calendar_Time)
                return Boolean is
      Cal1 : Calendar_Time := P_Cal1;
      Cal2 : Calendar_Time := P_Cal2;
   begin
      if ((not Cal1.Can_Calculate) or (not Cal2.Can_Calculate)) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Set_Timezone(Cal1, UTC_Zone);
      Set_Timezone(Cal2, UTC_Zone);
      Set_Do_Leap_Seconds(Cal1, False);
      Set_Do_Leap_Seconds(Cal2, False);

      if (Cal1.Year /= Cal2.Year) then
         return Cal1.Year < Cal2.Year;
      end if;

      if (Cal1.Day_Of_Year /= Cal2.Day_Of_Year) then
         return Cal1.Day_Of_Year < Cal2.Day_Of_Year;
      end if;

      if (Cal1.Hour_Of_Day /= Cal2.Hour_Of_Day) then
         return Cal1.Hour_Of_Day < Cal2.Hour_Of_Day;
      end if;

      if (Cal1.Minute /= Cal2.Minute) then
         return Cal1.Minute < Cal2.Minute;
      end if;

      if (Cal1.Second /= Cal2.Second) then
         return Cal1.Second < Cal2.Second;
      end if;

      return Cal1.Sub_Second < Cal2.Sub_Second;
   end "<";

   function "<="(P_Cal1, P_Cal2 : in Calendar_Time)
                return Boolean is
      Cal1 : Calendar_Time := P_Cal1;
      Cal2 : Calendar_Time := P_Cal2;
   begin
      if ((not Cal1.Can_Calculate) or (not Cal2.Can_Calculate)) then
         Raise_Exception(Invalid_Date_Info'Identity, "Cannot calculate");
      end if;

      Set_Timezone(Cal1, UTC_Zone);
      Set_Timezone(Cal2, UTC_Zone);
      Set_Do_Leap_Seconds(Cal1, False);
      Set_Do_Leap_Seconds(Cal2, False);

      if (Cal1.Year /= Cal2.Year) then
         return Cal1.Year < Cal2.Year;
      end if;

      if (Cal1.Day_Of_Year /= Cal2.Day_Of_Year) then
         return Cal1.Day_Of_Year < Cal2.Day_Of_Year;
      end if;

      if (Cal1.Hour_Of_Day /= Cal2.Hour_Of_Day) then
         return Cal1.Hour_Of_Day < Cal2.Hour_Of_Day;
      end if;

      if (Cal1.Minute /= Cal2.Minute) then
         return Cal1.Minute < Cal2.Minute;
      end if;

      if (Cal1.Second /= Cal2.Second) then
         return Cal1.Second < Cal2.Second;
      end if;

      return Cal1.Sub_Second <= Cal2.Sub_Second;
   end "<=";

   function Get_Year(Cal : in Calendar_Time) return Year_Range is
   begin
      if (not Cal.Year_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Year not valid");
      end if;

      return Cal.Year;
   end Get_Year;

   function Get_Month(Cal : in Calendar_Time) return Month_Range is
   begin
      if (not Cal.Month_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Month not valid");
      end if;

      return Cal.Month;
   end Get_Month;

   function Get_Short_Month(Cal : in Calendar_Time) return Short_Month is
   begin
      return Month_Range_To_Short_Month(Get_Month(Cal));
   end Get_Short_Month;

   function Get_Long_Month(Cal : in Calendar_Time) return Long_Month is
   begin
      return Month_Range_To_Long_Month(Get_Month(Cal));
   end Get_Long_Month;

   function Get_Week_Of_Year(Cal : in Calendar_Time)
                             return Week_Of_Year_Range is
   begin
      if (not Cal.Week_Of_Year_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Week_Of_Year not valid");
      end if;

      return Cal.Week_Of_Year;
   end Get_Week_Of_Year;

   function Get_Week_Of_Month(Cal : in Calendar_Time)
                              return Week_Of_Month_Range is
   begin
      if (not Cal.Week_Of_Month_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Week_Of_Month not valid");
      end if;

      return Cal.Week_Of_Month;
   end Get_Week_Of_Month;

   function Get_Day_Of_Year(Cal : in Calendar_Time) return Day_Of_Year_Range is
   begin
      if (not Cal.Day_Of_Year_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Day_Of_Year not valid");
      end if;

      return Cal.Day_Of_Year;
   end Get_Day_Of_Year;

   function Get_Day_Of_Month(Cal : in Calendar_Time)
                             return Day_Of_Month_Range is
   begin
      if (not Cal.Day_Of_Month_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Day_Of_Month not valid");
      end if;

      return Cal.Day_Of_Month;
   end Get_Day_Of_Month;

   function Get_Weekday(Cal : in Calendar_Time) return Weekday_Range is
   begin
      if (not Cal.Weekday_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Weekday not valid");
      end if;

      return Cal.Weekday;
   end Get_Weekday;

   function Get_Short_Weekday(Cal : in Calendar_Time) return Short_Weekday is
   begin
      return Weekday_Range_To_Short_Weekday(Get_Weekday(Cal));
   end Get_Short_Weekday;

   function Get_Long_Weekday(Cal : in Calendar_Time) return Long_Weekday is
   begin
      return Weekday_Range_To_Long_Weekday(Get_Weekday(Cal));
   end Get_Long_Weekday;

   function Get_Hour_Of_Day(Cal : in Calendar_Time) return Hour_Of_Day_Range is
   begin
      if (not Cal.Hour_Of_Day_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Hour_Of_Day not valid");
      end if;

      return Cal.Hour_Of_Day;
   end Get_Hour_Of_Day;

   function Get_Hour(Cal : in Calendar_Time) return Hour_Range is
   begin
      if (not Cal.Hour_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Hour not valid");
      end if;

      return Cal.Hour;
   end Get_Hour;

   function Get_AM_PM(Cal : in Calendar_Time) return AM_PM_Range is
   begin
      if (not Cal.AM_PM_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "AM_PM not valid");
      end if;

      return Cal.AM_PM;
   end Get_AM_PM;

   function Get_Minute(Cal : in Calendar_Time) return Minute_Range is
   begin
      if (not Cal.Minute_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Minute not valid");
      end if;

      return Cal.Minute;
   end Get_Minute;

   function Get_Second(Cal : in Calendar_Time) return Second_Range is
   begin
      if (not Cal.Second_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Second not valid");
      end if;

      return Cal.Second;
   end Get_Second;

   function Get_Sub_Second(Cal : in Calendar_Time)
                           return Sub_Second_Duration is
   begin
      if (not Cal.Sub_Second_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "Sub_Second not valid");
      end if;

      return Cal.Sub_Second;
   end Get_Sub_Second;

   function Get_DST_Active(Cal : in Calendar_Time) return Boolean is
   begin
      if (not Cal.DST_Active_Valid) then
         Raise_Exception(Invalid_Date_Info'Identity, "DST_Active not valid");
      end if;

      return Cal.DST_Active;
   end Get_DST_Active;

   Base_Ref_Time : Calendar_Time;

   procedure Set_Do_Leap_Seconds(Cal : in out Calendar_Time;
                                 Val : in Boolean) is
      Tmp, Tmp2 : Integer;
   begin
      if (Val = Cal.Do_Leap_Seconds) then
         return;
      end if;

      if (Cal.Can_Calculate) then
         if (not Val) then
            -- We are turning leap seconds off when they were on.
            Tmp := Leap_Secs_In_Range(Base_Ref_Time, Cal);
            Add_To_Second(Cal, Tmp, False);

            -- Need to do the previous calculation with leap seconds on
            -- and the next on with leap seconds off.
            Cal.Do_Leap_Seconds := Val;

            -- We might have moved over a leap second, so we need to add
            -- this back on.
            Tmp2 := Leap_Secs_In_Range(Base_Ref_Time, Cal);
            if (Tmp /= Tmp2) then
               Add_To_Second(Cal, Tmp2-Tmp, False);
            end if;
         else
            -- We are turning leap seconds on when they were off.
            Tmp := Leap_Secs_In_Range(Base_Ref_Time, Cal);
            Add_To_Second(Cal, -Tmp, False);

            -- Need to do the previous calculation with leap seconds off
            -- and the next one with them on.
            Cal.Do_Leap_Seconds := Val;

            -- We might have moved over a leap second, so we need to add
            -- this back on.
            Tmp2 := Leap_Secs_In_Range(Base_Ref_Time, Cal);
            if (Tmp /= Tmp2) then
               Add_To_Second(Cal, Tmp-Tmp2, False);
            end if;

         end if;
      else
         Cal.Do_Leap_Seconds := Val;
      end if;
   end Set_Do_Leap_Seconds;

   function Get_Do_Leap_Seconds(Cal : in Calendar_Time)
                                return Boolean is
   begin
      return Cal.Do_Leap_Seconds;
   end Get_Do_Leap_Seconds;

   procedure Invalidate_Time(Cal : in out Calendar_Time) is
   begin
      Cal.Week_Of_Year_Valid := False;
      Cal.Week_Of_Month_Valid := False;
      Cal.Day_Of_Year_Valid := False;
      Cal.Weekday_Valid := False;
      Cal.AM_PM_Valid := False;
      Cal.Hour_Valid := False;
      Cal.DST_Active_Valid := False;
      Cal.Year_Valid := False;
      Cal.Month_Valid := False;
      Cal.Day_Of_Month_Valid := False;
      Cal.Hour_Of_Day_Valid := False;
      Cal.Minute_Valid := False;
      Cal.Second_Valid := False;
      Cal.Sub_Second_Valid := False;
      Cal.Can_Calculate := False;
   end Invalidate_Time;

   procedure Set_Time(Cal : in out Calendar_Time;
                      T   : in Ada.Calendar.Time) is
      Sec           : Duration;
      V             : Duration;
      Second_Of_Day : Integer;
      Year          : Year_Range;
      Month         : Month_Range;
      Day           : Day_Of_Month_Range;
      Old_Leap_Secs : Boolean;
      New_Sec       : Duration;
   begin
      Old_Leap_Secs := Cal.Do_Leap_Seconds;
      Cal.Do_Leap_Seconds := False;

      Ada.Calendar.Split(T, Year, Month, Day, Sec);

      Invalidate_Time(Cal);

      Set_Year(Cal, Year);
      Set_Month(Cal, Month);
      Set_Day_Of_Month(Cal, Day);

      -- Ada rounds, but we want truncation.
      V := Duration(Integer(Sec));
      if (V > Sec) then
         V := Duration(Integer(Sec)-1);
      end if;
      Second_Of_Day := Integer(V);

      Set_Hour_Of_Day(Cal, Second_Of_Day / Secs_In_Hour);
      Set_Minute(Cal, (Second_Of_Day mod Secs_In_Hour) / Secs_In_Minute);
      Set_Second(Cal, Second_Of_Day mod Secs_In_Minute);

      -- Now calculate the microseconds.  We assume a duration can
      -- hold up to a million.
      Set_Sub_Second(Cal, Sec - V);

      -- Attempt to detect if we are in the "Fall back" portion of DST.
      Ada.Calendar.Split(T + 3600.0, Year, Month, Day, New_Sec);
      if (New_Sec = Sec) then
         -- We are not in DST, so set it explicitly.  If we are in the
         -- DST portion of the overlay, the compute code automatically
         -- picks that if the DST value is invalid.
         Set_Dst_Active(Cal, False);
      end if;

      if (Old_Leap_Secs) then
         Compute_Fields(Cal);
         Set_Do_Leap_Seconds(Cal, Old_Leap_Secs);
      end if;
   exception
      when others =>
         Cal.Do_Leap_Seconds := Old_Leap_Secs;
   end Set_Time;

   function Get_Time(Cal : in Calendar_Time) return Ada.Calendar.Time is
      Sec           : Duration;
      Second_Of_Day : Integer;
      T_Cal         : Calendar_Time := Cal;
      Rv            : Time;
      DST_Active    : DST_State;
   begin
      if ((not T_Cal.Year_Valid)
          and (not T_Cal.Month_Valid)
          and (not T_Cal.Day_Of_Month_Valid)
          and (not T_Cal.Hour_Of_Day_Valid)
          and (not T_Cal.Minute_Valid)
          and (not T_Cal.Second_Valid))
      then
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Conversion to time requires the Year, Month,"
                         & " Day_Of_Month, Hour_Of_Day, Minute, and"
                         & " Second field to be valid");
      end if;

      -- The time always doesn't have leap seconds.
      Set_Do_Leap_Seconds(T_Cal, False);

      Second_Of_Day := ((T_Cal.Hour * Secs_In_Hour)
                        + (T_Cal.Minute * Secs_In_Minute)
                        + T_Cal.Second);

      Sec := Duration(Second_Of_Day);
      if T_Cal.Sub_Second_Valid then
         Sec := Sec + T_Cal.Sub_Second;
      end if;

      Rv := Ada.Calendar.Time_Of(T_Cal.Year,
                                 T_Cal.Month,
                                 T_Cal.Day_Of_Month,
                                 Sec);

      -- Attempt to compensate for DST.  This only really matters in overlay
      -- situations.
      Daylight_Savings_Active(DST_Active,
                              T_Cal.DST_Offset,
                              T_Cal.Zone.all,
                              T_Cal.Year,
                              T_Cal.Day_Of_Year,
                              T_Cal.Hour_Of_Day,
                              T_Cal.Minute,
                              T_Cal.Second);
      if (DST_Active = Overlay) then
         -- If we are in an overlay and DST is not active, then we are in
         -- the non-DST portion of the overlay, so compensate.
         if ((Cal.DST_Active_Valid) and then (not Cal.DST_Active)) then
            Rv := Rv + 3600.0;
         end if;
      end if;

      return Rv;
   end Get_Time;

   procedure Set_Time(Cal : in out Calendar_Time;
                      T   : in Portable_Time) is
      Orig_Zone     : Time_Zone_Class;
      Tmp           : Integer;
      Old_Leap_Secs : Boolean;
   begin
      Orig_Zone := Cal.Zone;
      Old_Leap_Secs := Cal.Do_Leap_Seconds;

      Cal.Zone := UTC_Zone;
      Cal.Do_Leap_Seconds := False;
      Set_Year(Cal, T.Year);
      Set_Day_Of_Year(Cal, (T.Second_Of_Year / Secs_In_Day) + 1);
      Tmp := T.Second_Of_Year mod Secs_In_Day;
      Set_Hour_Of_Day(Cal, Tmp / Secs_In_Hour);
      Tmp := Tmp mod Secs_In_Hour;
      Set_Minute(Cal, Tmp / Secs_In_Minute);
      Set_Second(Cal, Tmp mod Secs_In_Minute);
      Set_Sub_Second(Cal, Duration(T.Microsecond) / 1_000_000.0);
      Compute_Fields(Cal);
      Set_Timezone(Cal, Orig_Zone);
      Set_Do_Leap_Seconds(Cal, Old_Leap_Secs);
   exception
      when others =>
         Cal.Do_Leap_Seconds := Old_Leap_Secs;
         Cal.Zone := Orig_Zone;
   end Set_Time;

   function Get_Time(Cal : in Calendar_Time) return Portable_Time is
      New_Cal : Calendar_Time := Cal;
   begin
      if ((not Cal.Year_Valid)
          and (not Cal.Day_Of_Year_Valid)
          and (not Cal.Hour_Of_Day_Valid)
          and (not Cal.Minute_Valid)
          and (not Cal.Second_Valid))
      then
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Conversion to time requires the Year, Month,"
                         & " Day_Of_Month, Hour_Of_Day, Minute, and"
                         & " Second field to be valid");
      end if;

      Set_Timezone(New_Cal, UTC_Zone);
      Set_Do_Leap_Seconds(New_Cal, False);

      return (New_Cal.Year,
              (((New_Cal.Day_Of_Year-1) * Secs_In_Day)
               + (New_Cal.Hour_Of_Day * Secs_In_Hour)
               + (New_Cal.Minute * Secs_In_Minute)
               + (New_Cal.Second)),
              Integer(New_Cal.Sub_Second * 1_000_000.0));
   end Get_Time;

   procedure Set_Time(Cal : in out Calendar_Time;
                      T   : in Unix_Time) is
      Orig_Zone     : Time_Zone_Class;
      Days          : Integer;
      Tmp           : Integer;
      Year          : Integer;
      Year_Guess    : Integer;
      Days_Guess    : Integer;
      Old_Leap_Secs : Boolean;
   begin
      Orig_Zone := Cal.Zone;
      Old_Leap_Secs := Cal.Do_Leap_Seconds;

      Cal.Zone := UTC_Zone;
      Cal.Do_Leap_Seconds := False;
      Days := Integer(T.Second / Interfaces.C.long(Secs_In_Day));
      Tmp := Integer(T.Second mod Interfaces.C.long(Secs_In_Day));
      Year := 1970;
      while (Days > Days_In_Year(Year)) loop
         Year_Guess := Year + Days/365;
         Days_Guess := Days - Days_Between_Years(Year_Guess, Year);
         while (Days_Guess < 0) loop
            Year_Guess := Year_Guess - 1;
            Days_Guess := Days - Days_Between_Years(Year_Guess, Year);
         end loop;
         Year := Year_Guess;
         Days := Days_Guess;
      end loop;

      Set_Year(Cal, Year);
      Set_Day_Of_Year(Cal, days + 1);
      Set_Hour_Of_Day(Cal, Tmp / Secs_In_Hour);
      Tmp := Tmp mod Secs_In_Hour;
      Set_Minute(Cal, Tmp / Secs_In_Minute);
      Set_Second(Cal, Tmp mod Secs_In_Minute);
      Set_Sub_Second(Cal, Duration(T.Microsecond) / 1_000_000.0);
      Compute_Fields(Cal);
      Set_Timezone(Cal, Orig_Zone);
      Set_Do_Leap_Seconds(Cal, Old_Leap_Secs);
   exception
      when others =>
         Cal.Do_Leap_Seconds := Old_Leap_Secs;
         Cal.Zone := Orig_Zone;
   end Set_Time;

   function Get_Time(Cal : in Calendar_Time) return Unix_Time is
      New_Cal : Calendar_Time := Cal;
      Tmp     : Integer;
   begin
      if ((not Cal.Year_Valid)
          and (not Cal.Day_Of_Year_Valid)
          and (not Cal.Hour_Of_Day_Valid)
          and (not Cal.Minute_Valid)
          and (not Cal.Second_Valid))
      then
         Raise_Exception(Invalid_Date_Info'Identity,
                         "Conversion to time requires the Year, Month,"
                         & " Day_Of_Month, Hour_Of_Day, Minute, and"
                         & " Second field to be valid");
      end if;

      Set_Timezone(New_Cal, UTC_Zone);
      Set_Do_Leap_Seconds(New_Cal, False);

      Tmp := Days_Between_Years(New_Cal.Year, 1970);
      return ((Interfaces.C.long((Tmp+New_Cal.Day_Of_Year-1) * Secs_In_Day)
               + Interfaces.C.long(New_Cal.Hour_Of_Day * Secs_In_Hour)
               + Interfaces.C.long(New_Cal.Minute * Secs_In_Minute)
               + Interfaces.C.long(New_Cal.Second)),
              Interfaces.C.long(New_Cal.Sub_Second * 1_000_000.0));
   end Get_Time;

   procedure Set_Timezone(Cal  : in out Calendar_Time;
                          Zone : in Time_Zone_Class) is
      Old_Zone         : Time_Zone_Class;
      DST_Active       : DST_State;
      Was_DST_Active   : Boolean;
      Old_Do_Leap_Secs : Boolean;
   begin
      if Zone = Cal.Zone then
         return;
      end if;

      if ((Cal.Zone /= null) and Cal.Can_Calculate) then
         -- Move from one timezone to another.

         Old_Do_Leap_Secs := Cal.Do_Leap_Seconds;
         begin
            -- Don't do leap seconds in this computation.
            Set_Do_Leap_Seconds(Cal, False);

            -- Convert over to the new time zone.
            Compute_Fields(Cal);
            Old_Zone := Cal.Zone;
            Was_DST_Active := Cal.DST_Active;
            Cal.Zone := UTC_Zone;
            Cal.DST_Active := False;
            if (Was_DST_Active) then
               Add_To_Second(Cal, - Cal.DST_Offset, False);
            end if;
            -- Convert to the new timezone offset.  We ignore leap seconds
            -- in this conversion, since they don't matter.
            Add_To_Second(Cal, -Cal.UTC_Offset, False);
            Cal.UTC_Offset := Get_UTC_Offset(Zone.all,
                                             Cal.Year,
                                             Cal.Day_Of_Year,
                                             Cal.Hour_Of_Day,
                                             Cal.Minute,
                                             Cal.Second);
            Add_To_Second(Cal, Cal.UTC_Offset, False);
            Daylight_Savings_Active(DST_Active,
                                    Cal.DST_Offset,
                                    Zone.all,
                                    Cal.Year,
                                    Cal.Day_Of_Year,
                                    Cal.Hour_Of_Day,
                                    Cal.Minute,
                                    Cal.Second);
            if (DST_Active /= Off) then
               -- Add the daylight savings bias.
               Add_To_Second(Cal, Cal.DST_Offset, False);
            end if;
            Set_Do_Leap_Seconds(Cal, Old_Do_Leap_Secs);
         exception
            when others =>
               -- If we have some random failure, make sure to set the
               -- leap seconds back properly and continue the exception.
               Set_Do_Leap_Seconds(Cal, Old_Do_Leap_Secs);
               raise;
         end;
      end if;
      Cal.Zone := Zone;
   end Set_Timezone;

   type String_Ptr is access all String;

   Long_Month_Str : array(Month_Range) of String_Ptr :=
     (new String'("January"), new String'("February"),
      new String'("March"), new String'("April"),
      new String'("May"), new String'("June"),
      new String'("July"), new String'("August"),
      new String'("September"), new String'("October"),
      new String'("November"), new String'("December"));

   Short_Month_Str : array(Month_Range) of String_Ptr :=
     (new String'("Jan"), new String'("Feb"), new String'("Mar"),
      new String'("Apr"), new String'("May"), new String'("Jun"),
      new String'("Jul"), new String'("Aug"), new String'("Sep"),
      new String'("Oct"), new String'("Nov"), new String'("Dec"));

   Long_Weekday_Str : array(Weekday_Range) of String_Ptr :=
     (new String'("Sunday"), new String'("Monday"), new String'("Tuesday"),
      new String'("Wednesday"), new String'("Thursday"),
      new String'("Friday"), new String'("Saturday"));

   Short_Weekday_Str : array(Weekday_Range) of String_Ptr :=
     (new String'("Sun"), new String'("Mon"), new String'("Tue"),
      new String'("Wed"), new String'("Thu"), new String'("Fri"),
      new String'("Sat"));

   function Fill_In(N : in String; Fill : in Character; Len : in Integer)
                    return String is
      J  : Integer := Len;
      Rv : String(1 .. Len);
   begin
      for I in reverse N'Range loop
         exit when (J < Rv'First);
         if (N(I) = ' ') then
            Rv(J) := Fill;
         else
            Rv(J) := N(I);
         end if;
         J := J - 1;
      end loop;

      while (J >= Rv'First) loop
         Rv(J) := Fill;
         J := J - 1;
      end loop;

      return Rv;
   end Fill_In;

   function Six_Digit_Num(Num : in Integer) return String is
   begin
      return Fill_In(Integer'Image(Num), '0', 6);
   end Six_Digit_Num;

   function Four_Digit_Num(Num : in Integer) return String is
   begin
      return Fill_In(Integer'Image(Num), '0', 4);
   end Four_Digit_Num;

   function Three_Digit_Num(Num : in Integer) return String is
   begin
      return Fill_In(Integer'Image(Num), '0', 3);
   end Three_Digit_Num;

   function Two_Digit_Num(Num : in Integer) return String is
   begin
      return Fill_In(Integer'Image(Num), '0', 2);
   end Two_Digit_Num;

   function Two_Digit_Pad_Num(Num : in Integer) return String is
   begin
      return Fill_In(Integer'Image(Num), ' ', 2);
   end Two_Digit_Pad_Num;

   subtype One_Digit_Range is Integer range 0 .. 9;
   function One_Digit_Num(Num : in One_Digit_Range) return String is
      Str : String := Integer'Image(Num);
      Last : Integer := Str'Last;
   begin
      return Str(Last .. Last);
   end One_Digit_Num;

   function Image(Cal    : in Calendar_Time;
                  Format : in String := "%e %b %Y %H:%M:%S.%F %Z")
                  return String is
      Max_Length : Integer := 0;
      N          : Integer;
      I          : Integer;
   begin
      I := Format'First;
      while I <= Format'Last loop
         if (Format(I) = '%') then
            if (I = Format'Last) then
               Raise_Exception(Invalid_Format'Identity,
                               "% at end of line not permitted");
            end if;

            case Format(I+1) is
               when '%' => N := 1;
               when 'A' => N := Long_Weekday_Str(Cal.Weekday)'Length;
               when 'a' => N := 3;
               when 'B' => N := Long_Month_Str(Cal.Month).all'Length;
               when 'b' => N := 3;
               when 'D' => N := 8;
               when 'd' => N := 2;
               when 'e' => N := 2;
               when 'F' => N := 6;
               when 'f' => N := 3;
               when 'H' => N := 2;
               when 'I' => N := 2;
               when 'j' => N := 3;
               when 'k' => N := 2;
               when 'l' => N := 2;
               when 'M' => N := 2;
               when 'm' => N := 2;
               when 'p' => N := 2;
               when 'r' => N := 11;
               when 'S' => N := 2;
               when 'T' => N := 8;
               when 'U' => N := 2;
               when 'W' => N := 2;
               when 'w' => N := 1;
               when 'y' => N := 2;
               when 'Y' =>
                  -- Handle BC years if necessary.
                  if (Cal.Year > 0) then N := 4;
                  else N := 5;
                  end if;
               when 'Z' => N := 10;
               when others =>
                  Raise_Exception(Invalid_Format'Identity,
                                  "%" & Format(I+1) &
                                  " is not a valid format operator");
            end case;
            Max_Length := Max_Length + N;
            I := I + 2;
         else
            Max_Length := Max_Length + 1;
            I := I + 1;
         end if;
      end loop;

      declare
         Str  : String(1 .. Max_Length);
         Next : Integer := 1;
      begin
         I := Format'First;
         while I <= Format'Last loop
            if (Format(I) = '%') then
               if (I = Format'Last) then
                  Raise_Exception(Invalid_Format'Identity,
                                  "% at end of line not permitted");
               end if;

               case Format(I+1) is
                  when '%' =>
                     Str(Next) := '%';
                     N := 1;

                  when 'A' =>
                     N := Long_Weekday_Str(Cal.Weekday).all'Length;
                     Str(Next .. Next + N - 1)
                       := Long_Weekday_Str(Cal.Weekday).all;

                  when 'a' =>
                       N := 3;
                       Str(Next .. Next + 2)
                         := Short_Weekday_Str(Cal.Weekday).all;

                  when 'B' =>
                     N := Long_Month_Str(Cal.Month).all'Length;
                     Str(Next .. Next + N - 1)
                       := Long_Month_Str(Cal.Month).all;

                  when 'b' =>
                       N := 3;
                       Str(Next .. Next + 2)
                         := Short_Month_Str(Cal.Month).all;

                  when 'D' =>
                     N := 8;
                     Str(Next .. Next + 7)
                       := Image(Cal, "%m/%d/%y");

                  when 'd' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Num(Cal.Day_Of_Month);

                  when 'e' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Pad_Num(Cal.Day_Of_Month);

                  when 'F' =>
                     N := 6;
                     Str(Next .. Next + 5)
                       := Six_Digit_Num(Integer(Cal.Sub_Second * 1_000_000));

                  when 'f' =>
                     N := 3;
                     Str(Next .. Next + 2)
                       := Three_Digit_Num(Integer(Cal.Sub_Second * 1_000));

                  when 'H' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Num(Cal.Hour_Of_Day);

                  when 'I' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Num(Cal.Hour);

                  when 'j' =>
                     N := 3;
                     Str(Next .. Next + 2)
                       := Three_Digit_Num(Cal.Day_Of_Year);

                  when 'k' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Pad_Num(Cal.Hour_Of_Day);

                  when 'l' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Pad_Num(Cal.Hour);

                  when 'M' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Num(Cal.Minute);

                  when 'm' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Num(Cal.Month);

                  when 'p' =>
                     N := 2;
                     Str(Next .. Next + 1) := AM_PM_Range'Image(Cal.AM_PM);

                  when 'r' =>
                     N := 11;
                     Str(Next .. Next + 10) := Image(Cal, "%I:%M:%S %p");

                  when 'S' =>
                     N := 2;
                     Str(Next .. Next + 1)
                       := Two_Digit_Num(Cal.Second);

                  when 'T' =>
                     N := 8;
                     Str(Next .. Next + 7) := Image(Cal, "%H:%M:%S");

                  when 'U' =>
                     N := 2;
                     declare
                        T_Cal : Calendar_Time := Cal;
                     begin
                        T_Cal.First_Weekday := 1;
                        Compute_Fields(T_Cal);
                        Str(Next .. Next + 1)
                          := Two_Digit_Num(Cal.Week_Of_Year);
                     end;

                  when 'W' => N := 2;
                     N := 2;
                     declare
                        T_Cal : Calendar_Time := Cal;
                     begin
                        Set_First_Weekday(T_Cal, 2);
                        Compute_Fields(T_Cal);
                        Str(Next .. Next + 1)
                          := Two_Digit_Num(T_Cal.Week_Of_Year);
                     end;

                  when 'w' =>
                     N := 1;
                     Str(Next .. Next)
                       := One_Digit_Num(Cal.Weekday);

                  when 'y' =>
                     N := 2;
                     Str(Next .. Next + 1) := Two_Digit_Num(Cal.Year mod 100);

                  when 'Y' =>
                     -- Handle BC years if necessary.
                     if (Cal.Year > 0) then
                        N := 4;
                        Str(Next .. Next + 3) := Four_Digit_Num(Cal.Year);
                     else
                        N := 5;
                        Str(Next .. Next + 4) := Fill_In(Integer'Image(Cal.Year-1),
                                                         '0',
                                                         5);
                     end if;
                     N := 4;

                  when 'Z' =>
                     declare
                        Id : String := Get_Id(Cal.Zone.all,
                                              Cal.DST_Active,
                                              Cal.Year,
                                              Cal.Day_Of_Year,
                                              Cal.Hour_Of_Day,
                                              Cal.Minute,
                                              Cal.Second);
                     begin
                        N := Id'Length;
                        Str(Next .. Next + N - 1) := Id;
                     end;

                  when others =>
                     Raise_Exception(Invalid_Format'Identity,
                                     "%" & Format(I+1) &
                                     " is not a valid format operator");
               end case;
               Next := Next + N;
               I := I + 2;
            else
               Str(Next) := Format(I);
               Next := Next + 1;
               I := I + 1;
            end if;
         end loop;

         return Str(1 .. Next-1);
      end;

   end Image;

begin
   Set_Year(Base_Ref_Time, Year_Range'First);
   Set_Day_Of_Year(Base_Ref_Time, 1);
   Set_Hour_Of_Day(Base_Ref_Time, 0);
   Set_Minute(Base_Ref_Time, 0);
   Set_Second(Base_Ref_Time, 0);
   Set_Timezone(Base_Ref_Time, UTC_Zone);
   Compute_Fields(Base_Ref_Time);
end Asl.Date_Time.Calendar;
