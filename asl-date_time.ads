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

-- A set of base types used by the time routines, and a bunch of routine
-- to handle all the nasty time calculations.

-- Date and time is very complicated.  Here's a quote from the GNU date
-- manual (which is a quote from something else):
--
--   Our units of temporal measurement, from seconds on up to months,
--   are so complicated, asymmetrical and disjunctive so as to make
--   coherent mental reckoning in time all but impossible.  Indeed, had
--   some tyrannical god contrived to enslave our minds to time, to
--   make it all but impossible for us to escape subjection to sodden
--   routines and unpleasant surprises, he could hardly have done
--   better than handing down our present system.  It is like a set of
--   trapezoidal building blocks, with no vertical or horizontal
--   surfaces, like a language in which the simplest thought demands
--   ornate constructions, useless particles and lengthy
--   circumlocutions.  Unlike the more successful patterns of language
--   and science, which enable us to face experience boldly or at least
--   level-headedly, our system of temporal calculation silently and
--   persistently encourages our terror of time.
--
--   ...  It is as though architects had to measure length in feet,
--   width in meters and height in ells; as though basic instruction
--   manuals demanded a knowledge of five different languages.  It is
--   no wonder then that we often look into our own immediate past or
--   future, last Tuesday or a week from Sunday, with feelings of
--   helpless confusion.  ...
--
--   -- Robert Grudin, `Time and the Art of Living'.
--
-- And this guy didn't even have to deal with leap seconds.

with Ada.Calendar;
with Interfaces.C;

package Asl.Date_Time is

   subtype Year_Range is Integer;

   subtype Month_Range is Integer range 1 .. 12;

   subtype Week_Of_Year_Range is Integer range 1 .. 53;

   subtype Week_Of_Month_Range is Integer range 1 .. 5;

   subtype Day_Of_Year_Range is Integer range 1 .. 366;

   subtype Day_Of_Month_Range is Integer range 1 .. 31;

   -- 1 is Sunday, 2 is Monday, etc.
   subtype Weekday_Range is Integer range 1 .. 7;

   -- Either Sunday or Monday may be the first day of the week.
   subtype First_Weekday_Range is Weekday_Range range 1 .. 2;

   subtype Hour_Of_Day_Range is Integer range 0 .. 23;

   subtype Hour_Range is Integer range 1 .. 12;

   subtype Minute_Range is Integer range 0 .. 59;

   -- This is not 0 .. 59 to account for leap seconds.  Occasionally, there
   -- may be a 61st second in a minute.
   subtype Second_Range is Integer range 0 .. 60;

   type AM_PM_Range is (AM, PM);

   -- Various time types.
   type Time_Types is (Wall,       -- The local time on the clock, with DST
                       Standard,   -- The local time without DST
                       Universal); -- UTC

   subtype Sub_Second_Duration is Duration range 0.0 .. 1.0 - Duration'Delta;

   -- Number of microseconds in a second.
   subtype Microseconds_Range is Integer range 0 .. 1_000_000-1;

   -- Max number of seconds in a year.  The "+3" is there instead of "-1"
   -- to allow up to 4 leap seconds in a year.
   subtype Seconds_In_Year_Range is Integer
     range 0 .. (366 * 24 * 60 * 60) + 3;

   -- This time is specified in UTC and can be portably processed on different
   -- systems.  The second of the year is zero based.
   type Portable_Time is record
      Year           : Year_Range;
      Second_Of_Year : Seconds_In_Year_Range;
      Microsecond    : Microseconds_Range;
   end record;

   -- The number of seconds since Jan 1, 1970
   type Unix_Time is record
      Second      : Interfaces.C.long;
      Microsecond : Interfaces.C.long;
   end record;
   pragma Convention(C, Unix_Time);

   type Long_Weekday is (Sunday, Monday, Tuesday, Wednesday, Thursday,
                         Friday, Saturday);

   type Short_Weekday is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

   function Weekday_Range_To_Long_Weekday(W : in Weekday_Range)
                                          return Long_Weekday;
   function Weekday_Range_To_Short_Weekday(W : in Weekday_Range)
                                           return Short_Weekday;
   function Long_Weekday_To_Weekday_Range(W : in Long_Weekday)
                                          return Weekday_Range;
   function Short_Weekday_To_Weekday_Range(W : in Short_Weekday)
                                           return Weekday_Range;

   type Long_Month is (January, February, March, April, May, June, July,
                       August, September, October, November, December);

   type Short_Month is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct,
                        Nov, Dec);

   function Month_Range_To_Long_Month(W : in Month_Range)
                                      return Long_Month;
   function Month_Range_To_Short_Month(W : in Month_Range)
                                       return Short_Month;
   function Long_Month_To_Month_Range(W : in Long_Month)
                                      return Month_Range;
   function Short_Month_To_Month_Range(W : in Short_Month)
                                       return Month_Range;

   Invalid_Date_Info : exception;

   -- Returns if the year is a leap year.
   function Is_Leap_Year(Year : in Year_Range) return Boolean;

   -- Return the number of days in the given year.
   function Days_In_Year(Year : in Year_Range) return Day_Of_Year_Range;

   -- Return the number of days between the start day of the years,
   -- taking in to account leap years.  If Y2 > Y1, the value will
   -- be negative.
   function Days_Between_Years(Y1, Y2 : in Year_Range) return Integer;

   -- Return the number of days in the given month.
   function Days_In_Month(Year  : in Year_Range;
                          Month : in Month_Range)
                          return Day_Of_Month_Range;

   -- Converts a year/month/day into a day counted from the beginning of
   -- the year.
   function Month_Day_To_Year_Day(Year         : in Year_Range;
                                  Month        : in Month_Range;
                                  Day_Of_Month : in Day_Of_Month_Range)
                                  return Day_Of_Year_Range;

   -- Converts a year/day of the year into a month and day of the month.
   procedure Year_Day_To_Month_Day(Year         : in Year_Range;
                                   Day_Of_Year  : in Day_Of_Year_Range;
                                   Month        : out Month_Range;
                                   Day_Of_Month : out Day_Of_Month_Range);

   -- Calculate the day of the week from a year and a day of the year.
   function Weekday_From_Year_Day(Year        : in Year_Range;
                                  Day_Of_Year : in Day_Of_Year_Range)
                                  return Weekday_Range;

   -- Calculate the day of the week from a year/month/day
   function Weekday_From_Month_Day(Year         : in Year_Range;
                                   Month        : in Month_Range;
                                   Day_Of_Month : in Day_Of_Month_Range)
                                   return Weekday_Range;

   -- Calculate the week of the year from a year and day of the year.
   function Week_Of_Year_From_Year_Day
     (Year          : in Year_Range;
      Day_Of_Year   : in Day_Of_Year_Range;
      First_Weekday : in First_Weekday_Range)
      return Week_Of_Year_Range;

   -- Calculate the week of the year from a year/month/day.
   function Week_Of_Year_From_Month_Day
     (Year          : in Year_Range;
      Month         : in Month_Range;
      Day_Of_Month  : in Day_Of_Month_Range;
      First_Weekday : in First_Weekday_Range)
      return Weekday_Range;

end Asl.Date_Time;
