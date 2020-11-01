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

package Asl.Date_Time.Timezone.Complex is

   type Complex_Zone(Name_Length, Entry_Count : Integer) is
     new Time_Zone with private;
     type Complex_Zone_Ptr is access all Complex_Zone;

   function Get_UTC_Offset(Z           : in Complex_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer;

   procedure Daylight_Savings_Active(DST_Active  : out DST_State;
                                     DST_Offset  : out Integer;
                                     Z           : in Complex_Zone;
                                     Year        : in Integer;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range);

   function Get_Id(Z           : in Complex_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String;

   function Duplicate_Zone(Name   : in String;
                           Source : in Complex_Zone)
                           return Time_Zone_Class;

   function Allocate_Complex_Zone(Name        : in String;
                                  Entry_Count : in Integer)
                                  return Complex_Zone_Ptr;

   -- An entry with no DST.
   procedure Add_Entry(Zone        : in out Complex_Zone;
                       UTC_Offset  : in Integer;
                       Format      : in String;
                       Has_Ends_On : in Boolean;
                       Ends_Year   : in Year_Range;
                       Ends_Mode   : in Zone_Mode_Class);

   -- An entry using rules.
   procedure Add_Entry(Zone        : in out Complex_Zone;
                       UTC_Offset  : in Integer;
                       Format      : in String;
                       Rules       : in String;
                       Has_Ends_On : in Boolean;
                       Ends_Year   : in Year_Range;
                       Ends_Mode   : in Zone_Mode_Class);

   -- An entry using a direct offset.
   procedure Add_Entry(Zone        : in out Complex_Zone;
                       UTC_Offset  : in Integer;
                       Format      : in String;
                       Save_Offset : in Integer;
                       Has_Ends_On : in Boolean;
                       Ends_Year   : in Year_Range;
                       Ends_Mode   : in Zone_Mode_Class);

   type Zone_Rules(Name_Length : Integer;
                   Num_Rules   : Integer) is private;
   type Zone_Rules_Ptr is access all Zone_Rules;

   function Allocate_Rule(Name      : in String;
                          Num_Rules : in Integer)
                          return Zone_Rules_Ptr;

   procedure Add_Rule(Rules   : in out Zone_Rules;
                      From    : in Integer;
                      To      : in Integer;
                      Save    : in Integer;
                      Mode    : in Zone_Mode_Class;
                      Letters : in String);

   function Find_Rule(Name : in String) return Zone_Rules_Ptr;

private

   type Zone_Rule_Range_Info is record
      From  : Year_Range; -- Start year
      To    : Year_Range; -- End year
      Save  : Integer;    -- Amount of time to add.  Zero means no DST.

      Mode : Zone_Mode_Class;

      Letters     : String(1 .. 4);
      Letters_Len : Integer;
   end record;
   type Zone_Rule_Range_Array is array(Integer range <>)
     of Zone_Rule_Range_Info;

   type Zone_Rules(Name_Length : Integer;
                   Num_Rules   : Integer) is record
      Name       : String(1 .. Name_Length);
      Rules      : Zone_Rule_Range_Array(1 .. Num_Rules);
      Last_Rule  : Integer := 0;
   end record;

   type Zone_Entry_Kind is (None, Offset, Rule);

   type Zone_Entry is record
      UTC_Offset : Integer;
      Kind       : Zone_Entry_Kind;
      Offset     : Integer;
      Rules      : Zone_Rules_Ptr;
      Format     : String(1 .. 10);
      Format_Len : Integer;
      Has_End    : Boolean;
      Ends_Year  : Year_Range;
      Ends_On    : Zone_Mode_Class;
   end record;
   type Zone_Entry_Array is array(Integer range <>) of Zone_Entry;

   type Complex_Zone(Name_Length, Entry_Count : Integer) is
     new Time_Zone(Name_Length) with record
      Entries    : Zone_Entry_Array(1 .. Entry_Count);
      Last_Entry : Integer := 0;
   end record;

end Asl.Date_Time.Timezone.Complex;
