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

with Asgc.Btree.Dynamic;

-- This is the base package for timezone handling timezones.  It defines
-- an abstract type that other timezones descend from.  It also defines
-- some zone modes that are useful for handling all the wierd ways the
-- start and end of DST are specified.
package Asl.Date_Time.Timezone is

   -- An abstract timezone.  The Name_Length is the length of the string
   -- holding the long name.
   type Time_Zone(Name_Length : Integer) is abstract tagged private;
   type Time_Zone_Class is access all Time_Zone'Class;

   -- Unfortunately, the state of daylight savings is not a straightforward
   -- thing based upon just the wall clock time.
   type DST_State is (Off,
                      No_Mans_Land, -- Time is in the area that doesn't exist
                                    -- due to DST skipping forward.
                      On,
                      Overlay); -- Time is in the area that is duplicated
                                -- due to DST skipping back.

   -- Return the offset in seconds from UTC of the time zone.  This requires a
   -- time because some areas changed their UTC offset in the past, and thus
   -- if you are working in times past you have to handle the UTC offset
   -- differently.
   function Get_UTC_Offset(Z           : in Time_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer
      is abstract;

   -- Get if daylight savings is active and the current daylight savings
   -- offset, given the current time.  Unfortunately, daylight savings is
   -- not a binary thing, there are times that don't exist (in the range
   -- where the daylight savings causes the time to skip forward) and times
   -- that are duplicated (where the daylight savings causes the same times
   -- to be gone over twice due to the time skipping back).  So you have to
   -- interpret the results yourself.  Note that DST_Offset will always be
   -- set if DST_Active is not set to Off, it's up to you to figure out if
   -- you really need to use DST_Offset or not.
   procedure Daylight_Savings_Active(DST_Active  : out DST_State;
                                     DST_Offset  : out Integer;
                                     Z           : in Time_Zone;
                                     Year        : in Integer;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range)
      is abstract;

   -- Return the id for the zone at the given time, things like "CDT" or
   -- "CST".  Since this depends on if DST is active and possibly on the
   -- particular time, all this info must be present.  The Is_Active
   -- field is only used if the time falls within the "fall-back" time
   -- when the time is set back from DST, it tells if the time is in the
   -- DST range or after DST is deactivated.
   function Get_Id(Z           : in Time_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String
      is abstract;

   -- Duplicate the given zone information with a new name.
   function Duplicate_Zone(Name   : in String;
                           Source : in Time_Zone)
                           return Time_Zone_Class
      is abstract;

   -- Return a name for the time zone.
   function Get_Zone_Name(Z : in Time_Zone) return String;

   -- Set the name of the timezone.  Note that once the name is set, it cannot
   -- be changed.
   procedure Set_Name(Z : in out Time_Zone; Name : in String);

   -- The timezone name was already registered.
   Duplicate_Timezone : exception;


   -- This is a simple timezone that doesn't to daylight savings, it only has a
   -- UTC offset.
   type No_DST_Zone(Name_Length, Id_Length : Integer) is
     new Time_Zone with private;
   type No_DST_Zone_Ptr is access all No_DST_Zone;

   function Get_UTC_Offset(Z           : in No_DST_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer;

   procedure Daylight_Savings_Active(DST_Active  : out DST_State;
                                     DST_Offset  : out Integer;
                                     Z           : in No_DST_Zone;
                                     Year        : in Integer;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range);

   function Get_Id(Z           : in No_DST_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String;

   function Duplicate_Zone(Name   : in String;
                           Source : in No_DST_Zone)
                           return Time_Zone_Class;

   -- Create a no daylight savings timeone.  The id is the short name of
   -- the timezone.  The offset is the UTC offset.
   function Allocate_No_DST_Zone(Name   : in String;
                                 Id     : in String;
                                 Offset : in Integer)
                                 return Time_Zone_Class;

   -- This represents the time where daylight savings starts/ends.
   type Zone_Change_Time is record
      Day_Of_Year  : Day_Of_Year_Range;
      Hour_Of_Day  : Hour_Of_Day_Range;
      Minute       : Minute_Range;
      Second       : Second_Range;
      Is_Wall_Time : Boolean;
   end record;

   function "="(T1, T2 : in Zone_Change_Time) return Boolean;
   function ">"(T1, T2 : in Zone_Change_Time) return Boolean;
   function "<"(T1, T2 : in Zone_Change_Time) return Boolean;
   function ">="(T1, T2 : in Zone_Change_Time) return Boolean;
   function "<="(T1, T2 : in Zone_Change_Time) return Boolean;
   function "+"(T1 : in Zone_Change_Time; P_Seconds : in Integer)
                return Zone_Change_Time;

   -- The mode of daylight savings time, meaning how to tell what
   -- day and time to change on.
   type Zone_Mode is abstract tagged private;
   type Zone_Mode_Class is access all Zone_Mode'Class;

   function Get_Zone_Change(Z       : in Zone_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time
      is abstract;

   -- The occurence is on a specific day of a month (ie October 25).
   type Day_Of_Month_Mode(Month       : Short_Month;
                          Day         : Day_Of_Month_Range;
                          Hour_Of_Day : Hour_Of_Day_Range;
                          Minute      : Minute_Range;
                          Second      : Second_Range;
                          Time_Type   : Time_Types)
   is new Zone_Mode with private;

   function Get_Zone_Change(Z       : in Day_Of_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time;

   -- The day occurs on a specific day of the week, ie the third sunday
   -- of October.  Day specifies the occurrence of the day, ie 3 for
   -- the previous example.
   type Weekday_In_Month_Mode(Month       : Short_Month;
                              Weekday     : Short_Weekday;
                              Day         : Day_Of_Month_Range;
                              Hour_Of_Day : Hour_Of_Day_Range;
                              Minute      : Minute_Range;
                              Second      : Second_Range;
                              Time_Type   : Time_Types)
   is new Zone_Mode with private;

   function Get_Zone_Change(Z       : in Weekday_In_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time;

   -- The day occurs on a specific day of the week counted from the end of
   -- the month, ie the second-to-last sunday of October.  Day
   -- specifies the occurrence of the day, ie 2 for the previous example.
   type Weekday_From_End_Month_Mode(Month       : Short_Month;
                                    Weekday     : Short_Weekday;
                                    Day         : Day_Of_Month_Range;
                                    Hour_Of_Day : Hour_Of_Day_Range;
                                    Minute      : Minute_Range;
                                    Second      : Second_Range;
                                    Time_Type   : Time_Types)
   is new Zone_Mode with private;

   function Get_Zone_Change(Z       : in Weekday_From_End_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time;

   -- The next day of the week on after a specific day of the month, like
   -- the first thursday on or after October 11.
   type Weekday_On_Or_After_Day_Of_Month_Mode
     (Month       : Short_Month;
      Weekday     : Short_Weekday;
      Day         : Day_Of_Month_Range;
      Hour_Of_Day : Hour_Of_Day_Range;
      Minute      : Minute_Range;
      Second      : Second_Range;
      Time_Type   : Time_Types)
   is new Zone_Mode with private;

   function Get_Zone_Change(Z       : in Weekday_On_Or_After_Day_Of_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time;

   -- The next day of the week on after a specific day of the month, like
   -- the thursday on or before October 11.
   type Weekday_On_Or_Before_Day_Of_Month_Mode
     (Month       : Short_Month;
      Weekday     : Short_Weekday;
      Day         : Day_Of_Month_Range;
      Hour_Of_Day : Hour_Of_Day_Range;
      Minute      : Minute_Range;
      Second      : Second_Range;
      Time_Type   : Time_Types)
   is new Zone_Mode with private;

   function Get_Zone_Change(Z      : in Weekday_On_Or_Before_Day_Of_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time;

   type Zone_Factory is tagged limited private;
   type Zone_Factory_Ptr is access all Zone_Factory;

   -- Find a timezone by its name;
   function Find_Timezone(Factory : in Zone_Factory;
                          Name    : in String)
                          return Time_Zone_Class;

   -- Add a timezone to the list of registered timezones.
   procedure Register_Timezone(Factory : in out Zone_Factory;
                               Zone    : in Time_Zone_Class);

   Default_Timezones : Zone_Factory_Ptr;

   procedure Set_Default_Timezone(Zone : in Time_Zone_Class);

   function Get_Default_Timezone return Time_Zone_Class;

private

   type Time_Zone(Name_Length : Integer) is abstract tagged record
      Name : String(1 .. Name_Length);
   end record;

   type No_DST_Zone(Name_Length, Id_Length : Integer) is
     new Time_Zone(Name_Length) with record
      Offset : Integer;
      Id     : String(1 .. Id_Length);
   end record;

   type Zone_Mode is abstract tagged null record;

   type Day_Of_Month_Mode(Month       : Short_Month;
                          Day         : Day_Of_Month_Range;
                          Hour_Of_Day : Hour_Of_Day_Range;
                          Minute      : Minute_Range;
                          Second      : Second_Range;
                          Time_Type   : Time_Types)
   is new Zone_Mode with null record;

   type Weekday_In_Month_Mode(Month       : Short_Month;
                              Weekday     : Short_Weekday;
                              Day         : Day_Of_Month_Range;
                              Hour_Of_Day : Hour_Of_Day_Range;
                              Minute      : Minute_Range;
                              Second      : Second_Range;
                              Time_Type   : Time_Types)
   is new Zone_Mode with null record;

   type Weekday_From_End_Month_Mode(Month       : Short_Month;
                                    Weekday     : Short_Weekday;
                                    Day         : Day_Of_Month_Range;
                                    Hour_Of_Day : Hour_Of_Day_Range;
                                    Minute      : Minute_Range;
                                    Second      : Second_Range;
                                    Time_Type   : Time_Types)
   is new Zone_Mode with null record;

   type Weekday_On_Or_After_Day_Of_Month_Mode
     (Month       : Short_Month;
      Weekday     : Short_Weekday;
      Day         : Day_Of_Month_Range;
      Hour_Of_Day : Hour_Of_Day_Range;
      Minute      : Minute_Range;
      Second      : Second_Range;
      Time_Type   : Time_Types)
   is new Zone_Mode with null record;

   type Weekday_On_Or_Before_Day_Of_Month_Mode
     (Month       : Short_Month;
      Weekday     : Short_Weekday;
      Day         : Day_Of_Month_Range;
      Hour_Of_Day : Hour_Of_Day_Range;
      Minute      : Minute_Range;
      Second      : Second_Range;
      Time_Type   : Time_Types)
   is new Zone_Mode with null record;

   function Zone_Equal(V1, V2 : in Time_Zone_Class) return Boolean;
   function Zone_GT(V1, V2 : in Time_Zone_Class) return Boolean;
   function Zone_LT(V1, V2 : in Time_Zone_Class) return Boolean;
   function Zone_GE(V1, V2 : in Time_Zone_Class) return Boolean;
   function Zone_LE(V1, V2 : in Time_Zone_Class) return Boolean;

   package Asgc_Zone is new Asgc(Time_Zone_Class, Zone_Equal);
   package Asgc_Zone_Btree1 is new Asgc_Zone.Btree
     (Zone_GT, Zone_LT, Zone_GE, Zone_LE);
   package Asgc_Zone_Btree is new Asgc_Zone_Btree1.Dynamic;
   use Asgc_Zone_Btree;

   type Zone_Factory is tagged limited record
      Zones : Asgc_Zone_Btree.Object_Class
        := new Asgc_Zone_Btree.Object(Allow_Duplicates => False,
                                      Node_Size        => 5);
   end record;

end Asl.Date_Time.Timezone;
