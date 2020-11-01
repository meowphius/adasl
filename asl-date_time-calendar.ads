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


-- An implementation of a calendar.  This implementation understands various
-- things about years, months, days, weeks, timezones, daylight savings,
-- and other nasty things you have to deal with when handling time.
--
-- IMPORTANT: Unlike other calendar packages in other languages, this package
-- will not normally let you move into invalid times.  So, for instance, if
-- you set the time to Jan 31, 2001 then add a month, this package will raise
-- an exception, because Feb 31 is invalid.  If you set the time to April 1 2001
-- 2:30AM CST, it will raise an exception because daylight savings time has
-- skipped over this particular time.  Leap seconds can also cause problems.
-- You can avoid the exceptions by setting the "Sloppy" flag in the time
-- calculations, however, the time will be moved to a valid time.  It handles
-- them the following ways:
--
--   Invalid day of month - Move to the last day of the month.
--   Invalid time due to DST - Add the daylight savings offset (usually an hour).
--   Invalid time due to leap seconds - move to the first second of the next minute.
--
-- In this case, for instance, if you set the time to April 1 2001 2:30AM CST,
-- the time will magically migrate to April 1 2001 3:30AM CDT.
--
-- Note: DST is an acronym for Daylight Savings Time throughout this
-- code.

with Ada.Calendar; use Ada.Calendar;
with Asl.Date_Time.Timezone; use Asl.Date_Time.Timezone;

package Asl.Date_Time.Calendar is

   -- The central class, this holds a full calendar date/time in a
   -- specific timezone.
   type Calendar_Time is tagged private;

   -- The following set and get the various fields in the time.  You can
   -- set any field at any time, but setting some fields invalidates other
   -- fields.  The time is not completely recalculated on every change,
   -- because that would be inefficient if several changes were made and
   -- only one result was required.  Instead, change the values then call
   -- Compute_Fields() to compute the values of all the fields.

   -- The following is the list of items that when set, invalidate other
   -- fields, and the list of fields they invalidate:

   -- Year: Week_Of_Year, Week_Of_Month, Day_Of_Year, Weekday
   -- Month: Week_Of_Year, Week_Of_Month, Day_Of_Year, Weekday
   -- Day_Of_Year: Month, Week_Of_Year, Week_Of_Month, Weekday, Day_Of_Month
   -- Day_Of_Month: Week_Of_Year, Week_Of_Month, Weekday, Day_Of_Year
   -- Week_Of_Year: Month, Day_Of_Year, Weekday, Day_Of_Month, Week_Of_Month
   -- Week_Of_Month: Day_Of_Year, Weekday, Day_Of_Month, Week_Of_Year
   -- Weekday: Day_Of_Month, Week_Of_Year, Week_Of_Month, Day_Of_Year

   -- Setting just about anything invalidates the ability to perform
   -- calculations on the data and invalidates the DST info.

   -- Trying to get a field that is invalid will cause Invalid_Date_Info
   -- to be raised.

   procedure Set_Year(Cal  : in out Calendar_Time;
                      Year : in Year_Range);
   function Get_Year(Cal : in Calendar_Time) return Year_Range;

   procedure Set_Month(Cal   : in out Calendar_Time;
                       Month : in Month_Range);
   function Get_Month(Cal : in Calendar_Time) return Month_Range;

   procedure Set_Short_Month(Cal   : in out Calendar_Time;
                             Month : in Short_Month);
   function Get_Short_Month(Cal : in Calendar_Time) return Short_Month;

   procedure Set_Long_Month(Cal   : in out Calendar_Time;
                            Month : in Long_Month);
   function Get_Long_Month(Cal : in Calendar_Time) return Long_Month;

   procedure Set_Day_Of_Year(Cal : in out Calendar_Time;
                             Day : in Day_Of_Year_Range);
   function Get_Day_Of_Year(Cal : in Calendar_Time) return Day_Of_Year_Range;

   procedure Set_Hour_Of_Day(Cal  : in out Calendar_Time;
                             Hour : in Hour_Of_Day_Range);
   function Get_Hour_Of_Day(Cal : in Calendar_Time) return Hour_Of_Day_Range;

   procedure Set_Minute(Cal    : in out Calendar_Time;
                        Minute : in Minute_Range);
   function Get_Minute(Cal : in Calendar_Time) return Minute_Range;

   procedure Set_Second(Cal    : in out Calendar_Time;
                        Second : in Second_Range);
   function Get_Second(Cal : in Calendar_Time) return Second_Range;

   procedure Set_Sub_Second(Cal        : in out Calendar_Time;
                            Sub_Second : in Sub_Second_Duration);
   function Get_Sub_Second(Cal : in Calendar_Time) return Sub_Second_Duration;

   procedure Set_Week_Of_Year(Cal          : in out Calendar_Time;
                              Week_Of_Year : in Week_Of_Year_Range);
   function Get_Week_Of_Year(Cal : in Calendar_Time) return Week_Of_Year_Range;

   procedure Set_Week_Of_Month(Cal           : in out Calendar_Time;
                               Week_Of_Month : in Week_Of_Month_Range);
   function Get_Week_Of_Month(Cal : in Calendar_Time)
                              return Week_Of_Month_Range;

   procedure Set_Day_Of_Month(Cal : in out Calendar_Time;
                              Day : in Day_Of_Month_Range);
   function Get_Day_Of_Month(Cal : in Calendar_Time) return Day_Of_Month_Range;

   procedure Set_Weekday(Cal     : in out Calendar_Time;
                         Weekday : in Weekday_Range);
   function Get_Weekday(Cal : in Calendar_Time) return Weekday_Range;

   procedure Set_Short_Weekday(Cal     : in out Calendar_Time;
                               Weekday : in Short_Weekday);
   function Get_Short_Weekday(Cal : in Calendar_Time) return Short_Weekday;

   procedure Set_Long_Weekday(Cal     : in out Calendar_Time;
                              Weekday : in Long_Weekday);
   function Get_Long_Weekday(Cal : in Calendar_Time) return Long_Weekday;

   procedure Set_AM_PM(Cal   : in out Calendar_Time;
                       AM_PM : in AM_PM_Range);
   function Get_AM_PM(Cal : in Calendar_Time) return AM_PM_Range;

   procedure Set_Hour(Cal  : in out Calendar_Time;
                      Hour : in Hour_Range);
   function Get_Hour(Cal : in Calendar_Time) return Hour_Range;

   -- Setting DST is a little wierd.  If you recompute after calling
   -- this, it will only have an effect if you are in the period of
   -- time where you move the clock back, it choses whether you are
   -- in the time before the clock is moved back (DST active) or
   -- after the clock is moved back (DST inactive).  Setting this
   -- without recomputing the fields can have some very strange
   -- effects.
   procedure Set_DST_Active(Cal    : in out Calendar_Time;
                            Active : in Boolean);
   function Get_DST_Active(Cal : in Calendar_Time) return Boolean;

   -- Set/get if leap seconds are handled.  When setting the value, if the
   -- calendar time can be calculated with, then the time will be adjusted
   -- to the new value with leap seconds on/off.
   procedure Set_Do_Leap_Seconds(Cal : in out Calendar_Time;
                                 Val : in Boolean);
   function Get_Do_Leap_Seconds(Cal : in Calendar_Time)
                                return Boolean;


   -- Set the time to completely invalid.
   procedure Invalidate_Time(Cal : in out Calendar_Time);


   -- Perform various calculations on time.  This lets you add/subtract
   -- various time units.  Note that you can add any amount of time units
   -- you like, these are not restricted by the limits.  So, for instance,
   -- you can add 3600 seconds and it will add an hour.  Also, the
   -- calculations at the hour and below level are done without respect
   -- to DST, so adding an hour really adds an hour of real time.

   -- Adding days adds whole day periods, and it doesn't matter how long
   -- the day is, so DST effects don't really do anything.  Adding one
   -- day will put you at the same wall clock time on the next day.

   -- Adding months moves the month without moving the day.  Note that if
   -- you end up on an invalid day of the month (like adding one month to
   -- Jan 31), this will raise an exception.  If Sloppy is set to
   -- True, then it will not raise an exception, it will instead
   -- move the day back to the last day of the month.  Also, sloppy
   -- move will cause an invalid hour of the day (like when the time that
   -- doesn't exist) to move to the next hour and an invalid second (like
   -- a leap second) to move to the next valid second.

   -- Normally, DST information is recalculated on every calculation and
   -- data is validate. To avoid doing this (if you are doing multiple
   -- calculations), Set "Recalc" to false.  Note that you must either do
   -- this on the last calculation or call Compute_Fields to set the DST
   -- info properly.

   -- Performing calculation will generally invalidate the following fields:
   -- Week_Of_Year, Week_Of_Month, Weekday.

   -- Calculating on a time that doesn't have all the fields set properly
   -- will result in Invalid_Date_Info being raised.

   procedure Add_To_Year(Cal    : in out Calendar_Time;
                         To_Add : in Integer;
                         Recalc : in Boolean := True;
                         Sloppy : in Boolean := False);
   procedure Add_To_Month(Cal    : in out Calendar_Time;
                          To_Add : in Integer;
                          Recalc : in Boolean := True;
                          Sloppy : in Boolean := False);
   procedure Add_To_Day_Of_Year(Cal    : in out Calendar_Time;
                                To_Add : in Integer;
                                Recalc : in Boolean := True;
                                Sloppy : in Boolean := False);
   procedure Add_To_Hour_Of_Day(Cal    : in out Calendar_Time;
                                To_Add : in Integer;
                                Recalc : in Boolean := True;
                                Sloppy : in Boolean := False);
   procedure Add_To_Minute(Cal    : in out Calendar_Time;
                           To_Add : in Integer;
                           Recalc : in Boolean := True;
                           Sloppy : in Boolean := False);
   procedure Add_To_Second(Cal    : in out Calendar_Time;
                           To_Add : in Integer;
                           Recalc : in Boolean := True);
   procedure Add_To_Sub_Second(Cal    : in out Calendar_Time;
                               To_Add : in Duration;
                               Recalc : in Boolean := True);

   function "+"(Cal : in Calendar_Time; To_Add : in Duration)
                return Calendar_Time;
   function "-"(Cal : in Calendar_Time; To_Add : in Duration)
                return Calendar_Time;
   function "-"(Cal1, Cal2 : in Calendar_Time)
                return Duration;

   function "="(P_Cal1, P_Cal2 : in Calendar_Time) return Boolean;
   function ">"(P_Cal1, P_Cal2 : in Calendar_Time) return Boolean;
   function "<"(P_Cal1, P_Cal2 : in Calendar_Time) return Boolean;
   function ">="(P_Cal1, P_Cal2 : in Calendar_Time) return Boolean;
   function "<="(P_Cal1, P_Cal2 : in Calendar_Time) return Boolean;

   -- Set the time from the Ada calendar.  This assumes that leap
   -- seconds are not in the ada time.  Note that Cal's timezone
   -- must be set to the same timezone as "T" for this to be accurate.
   procedure Set_Time(Cal : in out Calendar_Time;
                      T   : in Ada.Calendar.Time);

   -- Get the Ada calendar time from the calendar time.  Note that Cal's
   -- timezone must be set to the local timezone for this to be accurate.
   function Get_Time(Cal : in Calendar_Time) return Ada.Calendar.Time;

   -- Set the time from a portable time.
   procedure Set_Time(Cal : in out Calendar_Time;
                      T   : in Portable_Time);

   -- Get the portable time from the calendar time.
   function Get_Time(Cal : in Calendar_Time) return Portable_Time;

   -- Set the time from a portable time
   procedure Set_Time(Cal : in out Calendar_Time;
                      T   : in Unix_Time);

   -- Get the portable time from the calendar time.
   function Get_Time(Cal : in Calendar_Time) return Unix_Time;

   -- Recompute all the fields in the time.  If the calendar does not
   -- have enough information valid to compute the fields, it will
   -- raise Invalid_Date_Info.  To compute the fields, it must have
   -- the following set:
   --   Year
   -- and
   --   Hour_Of_Day, Minute, Second
   --             or
   --   Hour, AM_PM, Minute, Second
   -- and
   --   Month, Day_Of_Month
   --          or
   --   Week_Of_Year, Weekday
   --          or
   --   Month, Week_Of_Month, Weekday
   --
   -- If the time is not a valid time (in a place where DST is set forward,
   -- a leap second, an invalid day of the month) then this will raise
   -- an Invalid_Date_Info exception unless Sloppy is set to True.  If
   -- Sloppy is True, then the time will move to the next valid hour or
   -- second or to the last day of the month.
   procedure Compute_Fields(Cal    : in out Calendar_Time;
                            Sloppy : in Boolean := False);

   -- Set whether Sunday or Monday is used as the first day of the week
   -- when calculating the week numbers.  The default is Sunday.
   procedure Set_First_Weekday(Cal : in out Calendar_Time;
                               Val : in First_Weekday_Range);
   function Get_First_Weekday(Cal : in Calendar_Time)
                                  return First_Weekday_Range;

   -- Modify the timezone of the calendar.  If the current time information
   -- can be calculated with, the time will be adjusted to the new
   -- timezone's time.
   procedure Set_Timezone(Cal  : in out Calendar_Time;
                          Zone : in Time_Zone_Class);

   Invalid_Format : exception;

   -- Convert the calendar time to a string using the given format.
   -- The format operators in the string are like the one that
   -- the *nix "date" command uses, they start with '%' and have
   -- one character after them giving the field in the string.
   -- Everything else is put into the output string as-is.
   -- The format operators are:
   --   %%     a literal %
   --   %a     abbreviated weekday name (Sun..Sat)
   --   %A     full  weekday name, variable length (Sunday..Saturday)
   --   %b     abbreviated month name (Jan..Dec)
   --   %B     full month name, variable  length (January..December)
   --   %d     day of month (01..31)
   --   %D     date (mm/dd/yy)
   --   %e     day of month, blank padded ( 1..31)
   --   %f     milliseconds (000..999)
   --   %F     microseconds (000000..999999)
   --   %H     hour (00..23)
   --   %I     hour (01..12)
   --   %j     day of year (001..366)
   --   %k     hour ( 0..23)
   --   %l     hour ( 1..12)
   --   %m     month (01..12)
   --   %M     minute (00..59)
   --   %p     AM or PM
   --   %r     time, 12-hour (hh:mm:ss [AP]M)
   --   %S     second (00..60)
   --   %T     time, 24-hour (hh:mm:ss)
   --   %U     week number of year with Sunday as first day of week (01..53)
   --   %w     day of week (1..7); 1 represents Sunday
   --   %W     week number of year with Monday as first day of week (01..53)
   --   %y     last two digits of year (00..99)
   --   %Y     year (1970...)
   --   %Z     time zone (e.g., EDT)
   function Image(Cal    : in Calendar_Time;
                  Format : in String := "%e %b %Y %H:%M:%S.%F %Z")
                  return String;

private

   type Calendar_Time is tagged record
      Can_Calculate : Boolean := False;

      Year          : Year_Range;
      Month         : Month_Range;
      Week_Of_Year  : Week_Of_Year_Range;
      Week_Of_Month : Week_Of_Month_Range;
      Day_Of_Year   : Day_Of_Year_Range;
      Day_Of_Month  : Day_Of_Month_Range;
      Weekday       : Weekday_Range;
      Hour_Of_Day   : Hour_Of_Day_Range;
      AM_PM         : AM_PM_Range;
      Hour          : Hour_Range;
      Minute        : Minute_Range;
      Second        : Second_Range;
      Sub_Second    : Sub_Second_Duration;
      DST_Active    : Boolean;

      Year_Valid          : Boolean := False;
      Month_Valid         : Boolean := False;
      Week_Of_Year_Valid  : Boolean := False;
      Week_Of_Month_Valid : Boolean := False;
      Day_Of_Year_Valid   : Boolean := False;
      Day_Of_Month_Valid  : Boolean := False;
      Weekday_Valid       : Boolean := False;
      Hour_Of_Day_Valid   : Boolean := False;
      AM_PM_Valid         : Boolean := False;
      Hour_Valid          : Boolean := False;
      Minute_Valid        : Boolean := False;
      Second_Valid        : Boolean := False;
      Sub_Second_Valid    : Boolean := False;
      DST_Active_Valid    : Boolean := False;

      DST_Offset      : Integer;
      UTC_Offset      : Integer;

      -- Is this calendar doing leap seconds?
      Do_Leap_Seconds     : Boolean := False;

      -- We start out with Sunday as the first day of the week.
      First_Weekday : First_Weekday_Range := 1;

      -- Default the zone to the current default zone.
      Zone : Time_Zone_Class := Get_Default_Timezone;
   end record;

end Asl.Date_Time.Calendar;
