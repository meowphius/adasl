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

package Asl.Date_Time.Timezone.Simple is

   -- A simple timezone.  Name_Length is propigated back to the superclass,
   -- On_Length is the length of the string that hold the name when DST is
   -- on (like CDT), Off_Length is the length of the name when DST is off
   -- (like CST).
   type Simple_Zone(Name_Length, On_Length, Off_Length : Integer) is
     new Time_Zone with private;
   type Simple_Zone_Ptr is access all Simple_Zone;

   function Get_UTC_Offset(Z           : in Simple_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer;

   procedure Daylight_Savings_Active(DST_Active  : out DST_State;
                                     DST_Offset  : out Integer;
                                     Z           : in Simple_Zone;
                                     Year        : in Integer;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range);

   function Get_Id(Z           : in Simple_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String;

   function Duplicate_Zone(Name   : in String;
                           Source : in Simple_Zone)
                           return Time_Zone_Class;



   -- Create a timeone.
   function Allocate_Simple_Zone(Name      : in String;
                                 On_Id     : in String;
                                 Off_Id    : in String;
                                 Offset    : in Integer;
                                 Start_Day : in Zone_Mode_Class;
                                 End_Day   : in Zone_Mode_Class)
                                 return Time_Zone_Class;

private

   type Simple_Zone(Name_Length, On_Length, Off_Length : Integer) is
     new Time_Zone(Name_Length) with record
      On_Id      : String(1 .. On_Length);
      Off_Id     : String(1 .. Off_Length);
      Offset     : Integer;
      Start_Day  : Zone_Mode_Class;
      End_Day    : Zone_Mode_Class;
      DST_Offset : Integer := 3600;
   end record;

end Asl.Date_Time.Timezone.Simple;
