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

package body Asl.Date_Time is

   function Weekday_Range_To_Long_Weekday(W : in Weekday_Range)
                                          return Long_Weekday is
   begin
      return Long_Weekday'Val(W-1);
   end Weekday_Range_To_Long_Weekday;

   function Weekday_Range_To_Short_Weekday(W : in Weekday_Range)
                                           return Short_Weekday is
   begin
      return Short_Weekday'Val(W-1);
   end Weekday_Range_To_Short_Weekday;

   function Long_Weekday_To_Weekday_Range(W : in Long_Weekday)
                                          return Weekday_Range is
   begin
      return Long_Weekday'Pos(W) + 1;
   end Long_Weekday_To_Weekday_Range;

   function Short_Weekday_To_Weekday_Range(W : in Short_Weekday)
                                           return Weekday_Range is
   begin
      return Short_Weekday'Pos(W) + 1;
   end Short_Weekday_To_Weekday_Range;

   function Month_Range_To_Long_Month(W : in Month_Range)
                                      return Long_Month is
   begin
      return Long_Month'Val(W-1);
   end Month_Range_To_Long_Month;

   function Month_Range_To_Short_Month(W : in Month_Range)
                                       return Short_Month is
   begin
      return Short_Month'Val(W-1);
   end Month_Range_To_Short_Month;

   function Long_Month_To_Month_Range(W : in Long_Month)
                                      return Month_Range is
   begin
      return Long_Month'Pos(W) + 1;
   end Long_Month_To_Month_Range;

   function Short_Month_To_Month_Range(W : in Short_Month)
                                       return Month_Range is
   begin
      return Short_Month'Pos(W) + 1;
   end Short_Month_To_Month_Range;


   function Is_Leap_Year(Year : in Year_Range) return Boolean is
   begin
      return ((((Year mod 4) = 0)
               and then ((Year mod 100) /= 0))
              or else ((Year mod 400) = 0));
   end Is_Leap_Year;

   function Days_In_Year(Year : in Year_Range) return Day_Of_Year_Range is
   begin
      if Is_Leap_Year(Year) then
         return 366;
      else
         return 365;
      end if;
   end Days_In_Year;

   type Month_Array is array(Boolean, 1 .. 12) of Integer;

   -- Number of days in each month, indexed by if it's a leap year, then
   -- the month.
   Days_N_Month : constant Month_Array := (False => (31, 28, 31, 30, 31, 30,
                                                     31, 31, 30, 31, 30, 31),
                                           True  => (31, 29, 31, 30, 31, 30,
                                                     31, 31, 30, 31, 30, 31));

   function Days_In_Month(Year  : in Integer;
                          Month : in Month_Range)
                          return Day_Of_Month_Range is
   begin
      return Days_N_Month(Is_Leap_Year(Year), Month);
   end Days_In_Month;

   Days_To_January   : constant Integer := 0;
   Days_To_February  : constant Integer := Days_To_January + 31;
   Days_To_March     : constant Integer := Days_To_February + 28;
   Days_To_April     : constant Integer := Days_To_March + 31;
   Days_To_May       : constant Integer := Days_To_April + 30;
   Days_To_June      : constant Integer := Days_To_May + 31;
   Days_To_July      : constant Integer := Days_To_June + 30;
   Days_To_August    : constant Integer := Days_To_July + 31;
   Days_To_September : constant Integer := Days_To_August + 31;
   Days_To_October   : constant Integer := Days_To_September + 30;
   Days_To_November  : constant Integer := Days_To_October + 31;
   Days_To_December  : constant Integer := Days_To_November + 30;

   -- Numbers of days before the first day of the month, indexed by if
   -- it's a leap year, then the month.
   Month_To_Day : constant Month_Array := (False => (Days_To_January,
                                                     Days_To_February,
                                                     Days_To_March,
                                                     Days_To_April,
                                                     Days_To_May,
                                                     Days_To_June,
                                                     Days_To_July,
                                                     Days_To_August,
                                                     Days_To_September,
                                                     Days_To_October,
                                                     Days_To_November,
                                                     Days_To_December),
                                           True  => (Days_To_January,
                                                     Days_To_February,
                                                     1 + Days_To_March,
                                                     1 + Days_To_April,
                                                     1 + Days_To_May,
                                                     1 + Days_To_June,
                                                     1 + Days_To_July,
                                                     1 + Days_To_August,
                                                     1 + Days_To_September,
                                                     1 + Days_To_October,
                                                     1 + Days_To_November,
                                                     1 + Days_To_December));

   function Month_Day_To_Year_Day(Year         : in Year_Range;
                                  Month        : in Month_Range;
                                  Day_Of_Month : in Day_Of_Month_Range)
                                  return Day_Of_Year_Range is
      Leap : Boolean := Is_Leap_Year(Year);
   begin
      if (Day_Of_Month > Days_N_Month(Leap, Month)) then
         raise Invalid_Date_Info;
      end if;
      return Month_To_Day(Leap, Month) + Day_Of_Month;
   end Month_Day_To_Year_Day;

   procedure Year_Day_To_Month_Day(Year         : in Year_Range;
                                   Day_Of_Year  : in Day_Of_Year_Range;
                                   Month        : out Month_Range;
                                   Day_Of_Month : out Day_Of_Month_Range) is
      I : Day_Of_Year_Range;
      J : Month_Range;
      Leap : Boolean := Is_Leap_Year(Year);
   begin
      if (Day_Of_Year > Days_In_Year(Year)) then
         raise Invalid_Date_Info;
      end if;

      I := 31;
      J := 1;
      while (I < Day_Of_Year) loop
         J := J + 1;
         I := I + Days_N_Month(Leap, J);
      end loop;
      Month := J;
      Day_Of_Month := Day_Of_Year - Month_To_Day(Leap, J);
   end Year_Day_To_Month_Day;

   -- The following two procedures were stolen from glib-2.2.  Actually,
   -- a lot of these algorithms were stolen from there and the GCJ libjava
   -- time routines.
   function Divider(Top, Bottom : in Integer) return Integer is
      Rv : Integer;
   begin
      Rv := (Top / Bottom);
      if ((Top mod Bottom) < 0) then
         Rv := Rv - 1;
      end if;

      return Rv;
   end Divider;

   function Leaps_Through_Start_Of(Y : in Year_Range) return Integer is
   begin
      return Divider(Y-1, 4) - Divider(Y-1, 100) + Divider(Y-1, 400);
   end Leaps_Through_Start_Of;

   function Days_Between_Years(Y1, Y2 : in Year_Range) return Integer is
   begin
      return (((Y1  - Y2) * 365)
              + (Leaps_Through_Start_Of(Y1) - Leaps_Through_Start_Of(Y2)));
   end Days_Between_Years;

   -- Jan 1, 2006 is a Sunday, so we base the day-of-week calculations
   -- off this.
   Week_Ref_Year     : constant Year_Range := 2006;
   Leaps_At_Ref_Year : constant Integer := Leaps_Through_Start_Of(2006);

   function Weekday_From_Year_Day(Year        : in Year_Range;
                                  Day_Of_Year : in Day_Of_Year_Range)
                                  return Weekday_Range is
   begin
      if (Day_Of_Year > Days_In_Year(Year)) then
         raise Invalid_Date_Info;
      end if;

      -- Calculate the day of the week based upon the reference year (whose
      -- first day is Sunday).
      return ((((Year - Week_Ref_Year) * 365)
               + (Leaps_Through_Start_Of(Year)
                  - Leaps_At_Ref_Year)
               + (Day_Of_Year - 1))
              mod 7) + 1;
   end Weekday_From_Year_Day;

   function Weekday_From_Month_Day(Year         : in Year_Range;
                                   Month        : in Month_Range;
                                   Day_Of_Month : in Day_Of_Month_Range)
                                   return Weekday_Range is
   begin
      if (Day_Of_Month > Days_In_Month(Year, Month)) then
         raise Invalid_Date_Info;
      end if;

      return Weekday_From_Year_Day(Year,
                                   Month_Day_To_Year_Day(Year,
                                                         Month,
                                                         Day_Of_Month));
   end Weekday_From_Month_Day;

   function Week_Of_Year_From_Year_Day
     (Year          : in Year_Range;
      Day_Of_Year   : in Day_Of_Year_Range;
      First_Weekday : in First_Weekday_Range)
      return Week_Of_Year_Range
   is
      C_Weekday : Weekday_Range;
      V         : Natural;
   begin
      if (Day_Of_Year > Days_In_Year(Year)) then
         raise Invalid_Date_Info;
      end if;

      C_Weekday := Weekday_From_Year_Day(Year, Day_Of_Year);
      V := ((Day_Of_Year + 3 +
             ((7 - C_Weekday - (First_Weekday - 1)) mod 7))
            / 7);
      if (V = 0) then
         -- This means the week goes with the previous year, so recalculate
         -- in the previous year.
         V := ((Day_Of_Year + 3 + Days_In_Year(Year - 1)
                + ((7 - C_Weekday - (First_Weekday - 1)) mod 7))
               / 7);
      end if;
      return V;
   end Week_Of_Year_From_Year_Day;

   function Week_Of_Year_From_Month_Day
     (Year          : in Year_Range;
      Month         : in Month_Range;
      Day_Of_Month  : in Day_Of_Month_Range;
      First_Weekday : in First_Weekday_Range)
      return Weekday_Range is
   begin
      if (Day_Of_Month > Days_In_Month(Year, Month)) then
         raise Invalid_Date_Info;
      end if;

      return Week_Of_Year_From_Year_Day(Year,
                                        Month_Day_To_Year_Day(Year,
                                                              Month,
                                                              Day_Of_Month),
                                        First_Weekday);
   end Week_Of_Year_From_Month_Day;

end Asl.Date_Time;
