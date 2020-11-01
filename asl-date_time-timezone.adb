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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Asl.Strings; use Asl.Strings;

package body Asl.Date_Time.Timezone is

   procedure Set_Name(Z : in out Time_Zone; Name : in String) is
   begin
      Z.Name := Name;
   end Set_Name;

   function Get_Zone_Name(Z : in Time_Zone) return String is
   begin
      return Z.Name;
   end Get_Zone_Name;

   function Zone_Equal(V1, V2 : in Time_Zone_Class) return Boolean is
      Str1 : String := Get_Zone_Name(V1.all);
      Str2 : String := Get_Zone_Name(V2.all);
   begin
      return Asl.Strings.Compare(Str1, Str2) = 0;
   end Zone_Equal;

   function Zone_GT(V1, V2 : in Time_Zone_Class) return Boolean is
      Str1 : String := Get_Zone_Name(V1.all);
      Str2 : String := Get_Zone_Name(V2.all);
   begin
      return Asl.Strings.Compare(Str1, Str2) > 0;
   end Zone_GT;

   function Zone_LT(V1, V2 : in Time_Zone_Class) return Boolean is
      Str1 : String := Get_Zone_Name(V1.all);
      Str2 : String := Get_Zone_Name(V2.all);
   begin
      return Asl.Strings.Compare(Str1, Str2) < 0;
   end Zone_LT;

   function Zone_GE(V1, V2 : in Time_Zone_Class) return Boolean is
      Str1 : String := Get_Zone_Name(V1.all);
      Str2 : String := Get_Zone_Name(V2.all);
   begin
      return Asl.Strings.Compare(Str1, Str2) >= 0;
   end Zone_GE;

   function Zone_LE(V1, V2 : in Time_Zone_Class) return Boolean is
      Str1 : String := Get_Zone_Name(V1.all);
      Str2 : String := Get_Zone_Name(V2.all);
   begin
      return Asl.Strings.Compare(Str1, Str2) <= 0;
   end Zone_LE;

   procedure Register_Timezone(Factory : in out Zone_Factory;
                               Zone    : in Time_Zone_Class) is
   begin
      Add(Factory.Zones.all, Zone);
   exception
      when Asgc_Zone.Item_Already_Exists =>
         Raise_Exception(Duplicate_Timezone'Identity,
                         Zone.Name);
   end Register_Timezone;

   function Find_Timezone(Factory : in Zone_Factory;
                          Name    : in String)
                          return Time_Zone_Class is
      It    : Asgc_Zone_Btree.Iterator := New_Iterator(Factory.Zones);
      Found : Boolean;
      Rv    : Time_Zone_Class := null;
      Zone  : aliased No_DST_Zone(Name_Length => Name'Length,
                                  Id_Length   => 0);
   begin
      Set_Name(Zone, Name);
      Search(It, Zone'Unchecked_Access, Found);
      if Found then
         Rv := Get(It);
      end if;

      return Rv;
   end Find_Timezone;


   function Get_UTC_Offset(Z           : in No_DST_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer is
   begin
      return Z.Offset;
   end Get_UTC_Offset;

   procedure Daylight_Savings_Active(DST_Active  : out DST_State;
                                     DST_Offset  : out Integer;
                                     Z           : in No_DST_Zone;
                                     Year        : in Year_Range;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range) is
   begin
      DST_Active := Off;
      DST_Offset := 0;
   end Daylight_Savings_Active;

   function Get_Id(Z           : in No_DST_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String is
   begin
      return Z.Id;
   end Get_Id;

   function Duplicate_Zone(Name   : in String;
                           Source : in No_DST_Zone)
                           return Time_Zone_Class is
      Rv : No_DST_Zone_Ptr := new No_DST_Zone(Name'Length, Source.Id'Length);
   begin
      Set_Name(Rv.all, Name);
      Rv.Id :=  Source.Id;
      Rv.Offset := Source.Offset;
      return Time_Zone_Class(Rv);
   end Duplicate_Zone;

   function Allocate_No_DST_Zone(Name   : in String;
                                 Id     : in String;
                                 Offset : in Integer)
                                 return Time_Zone_Class is
      Rv : No_DST_Zone_Ptr := new No_DST_Zone(Name'Length, Id'Length);
   begin
      Rv.Offset := Offset;
      Set_Name(Rv.all, Name);
      Rv.Id := Id;
      return Time_Zone_Class(Rv);
   end Allocate_No_DST_Zone;

   function "="(T1, T2 : in Zone_Change_Time) return Boolean is
   begin
      return ((T1.Day_Of_Year = T2.Day_Of_Year)
              and (T1.Hour_Of_Day = T2.Hour_Of_Day)
              and (T1.Minute = T2.Minute)
              and (T1.Second  = T2.Second));
   end "=";

   function ">"(T1, T2 : in Zone_Change_Time) return Boolean is
   begin
      if (T1.Day_Of_Year /= T2.Day_Of_Year) then
         return T1.Day_Of_Year > T2.Day_Of_Year;
      end if;

      if (T1.Hour_Of_Day /= T2.Hour_Of_Day) then
         return T1.Hour_Of_Day > T2.Hour_Of_Day;
      end if;

      if (T1.Minute /= T2.Minute) then
         return T1.Minute > T2.Minute;
      end if;

      return T1.Second > T2.Second;
   end ">";

   function "<"(T1, T2 : in Zone_Change_Time) return Boolean is
   begin
      if (T1.Day_Of_Year /= T2.Day_Of_Year) then
         return T1.Day_Of_Year < T2.Day_Of_Year;
      end if;

      if (T1.Hour_Of_Day /= T2.Hour_Of_Day) then
         return T1.Hour_Of_Day < T2.Hour_Of_Day;
      end if;

      if (T1.Minute /= T2.Minute) then
         return T1.Minute < T2.Minute;
      end if;

      return T1.Second < T2.Second;
   end "<";

   function ">="(T1, T2 : in Zone_Change_Time) return Boolean is
   begin
      if (T1.Day_Of_Year /= T2.Day_Of_Year) then
         return T1.Day_Of_Year > T2.Day_Of_Year;
      end if;

      if (T1.Hour_Of_Day /= T2.Hour_Of_Day) then
         return T1.Hour_Of_Day > T2.Hour_Of_Day;
      end if;

      if (T1.Minute /= T2.Minute) then
         return T1.Minute > T2.Minute;
      end if;

      return T1.Second >= T2.Second;
   end ">=";

   function "<="(T1, T2 : in Zone_Change_Time) return Boolean is
   begin
      if (T1.Day_Of_Year /= T2.Day_Of_Year) then
         return T1.Day_Of_Year < T2.Day_Of_Year;
      end if;

      if (T1.Hour_Of_Day /= T2.Hour_Of_Day) then
         return T1.Hour_Of_Day < T2.Hour_Of_Day;
      end if;

      if (T1.Minute /= T2.Minute) then
         return T1.Minute < T2.Minute;
      end if;

      return T1.Second <= T2.Second;
   end "<=";

   function "+"(T1 : in Zone_Change_Time; P_Seconds : in Integer)
                return Zone_Change_Time is
      Seconds : Integer := T1.Second;
      Minutes : Integer := T1.Minute;
      Hours   : Integer := T1.Hour_Of_Day;
      Days    : Integer := T1.Day_Of_Year;

      procedure Add_To_Hours(H : in Integer) is
      begin
         Hours := Hours + H;
         if (Hours >= 24) then
            Days := Days + (Hours / 24);
            Hours := Hours mod 24;
         elsif (Hours < 0) then
            Days := Days + ((Hours-23) / 24);
            Hours := Hours mod 24;
         end if;
      end Add_To_Hours;

      procedure Add_To_Minutes(M : in Integer) is
      begin
         Minutes := Minutes + M;
         if (Minutes >= 60) then
            Add_To_Hours (Minutes / 60);
            Minutes := Minutes mod 60;
         elsif (Minutes < 0) then
            Add_To_Hours ((Minutes-59) / 60);
            Minutes := Minutes mod 60;
         end if;
      end Add_To_Minutes;

      procedure Add_To_Seconds(S : in Integer) is
      begin
         Seconds := Seconds + S;
         if (Seconds >= 60) then
            Add_To_Minutes (Seconds / 60);
            Seconds := Seconds mod 60;
         elsif (Seconds < 0) then
            Add_To_Minutes ((Seconds-59) / 60);
            Seconds := Seconds mod 60;
         end if;
      end Add_To_Seconds;
   begin
      Add_To_Seconds(P_Seconds);
      return(Days, Hours, Minutes, Seconds, T1.Is_Wall_Time);
   end "+";

   function Day_Of_Year_From_Month_Week(Year          : in Year_Range;
                                        Month         : in Month_Range;
                                        Weekday       : in Short_Weekday;
                                        Which_Weekday : in Integer;
                                        Monthday      : in Day_Of_Month_Range)
                                        return Integer is
      Target_Weekday : Integer := Short_Weekday'Pos(Weekday) + 1;
      Day_Of_Year    : Integer := Month_Day_To_Year_Day(Year, Month, Monthday);
      Tmp_Weekday    : Integer := Weekday_From_Year_Day(Year, Day_Of_Year);
   begin
      if (Tmp_Weekday <= Target_Weekday) then
         Day_Of_Year := (Day_Of_Year
                         + ((Target_Weekday - Tmp_Weekday)
                            + (7 * (Which_Weekday - 1))));
      else
         Day_Of_Year := (Day_Of_Year
                         + ((Target_Weekday - Tmp_Weekday)
                            + (7 * Which_Weekday)));
      end if;
      return Day_Of_Year;
   end Day_Of_Year_From_Month_Week;

   procedure Update_Time(Rv      : in out Zone_Change_Time;
                         TT      : in Time_Types;
                         UTC_Off : in Integer) is
   begin
      case TT is
         when Wall =>
            Rv.Is_Wall_Time := True;

         when Standard =>
            Rv.Is_Wall_Time := False;

         when Universal =>
            Rv := Rv + UTC_Off;
            Rv.Is_Wall_Time := False;
      end case;
   end Update_Time;

   function Get_Zone_Change(Z       : in Day_Of_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Year_Range)
                            return Zone_Change_Time is
      Day_Of_Year : Integer;
      Month       : Month_Range := Short_Month_To_Month_Range(Z.Month);
      Rv          : Zone_Change_Time;
   begin
      Day_Of_Year := Month_Day_To_Year_Day(Year, Month, Z.Day);
      Rv := (Day_Of_Year  => Day_Of_Year,
             Hour_Of_Day  => Z.Hour_Of_Day,
             Minute       => Z.Minute,
             Second       => Z.Second,
             Is_Wall_Time => True);
      Update_Time(Rv, Z.Time_Type, UTC_Off);
      return Rv;
   end Get_Zone_Change;

   function Get_Zone_Change(Z       : in Weekday_In_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Integer)
                            return Zone_Change_Time is
      Day_Of_Year : Integer;
      Month       : Month_Range := Short_Month_To_Month_Range(Z.Month);
      Rv          : Zone_Change_Time;
   begin
      Day_Of_Year := Day_Of_Year_From_Month_Week
        (Year, Month, Z.Weekday, Z.Day, 1);
      Rv := (Day_Of_Year  => Day_Of_Year,
             Hour_Of_Day  => Z.Hour_Of_Day,
             Minute       => Z.Minute,
             Second       => Z.Second,
             Is_Wall_Time => True);
      Update_Time(Rv, Z.Time_Type, UTC_Off);
      return Rv;
   end Get_Zone_Change;

   function Get_Zone_Change(Z       : in Weekday_From_End_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Year_Range)
                            return Zone_Change_Time is
      Target_Day  : Integer := Short_Weekday'Pos(Z.Weekday) + 1;
      Day_Of_Year : Integer;
      Tmp_Day     : Integer;
      Month       : Month_Range := Short_Month_To_Month_Range(Z.Month);
      Rv          : Zone_Change_Time;
   begin
      Day_Of_Year := Month_Day_To_Year_Day(Year, Month,
                                           Days_In_Month(Year, Month));
      Tmp_Day := Weekday_From_Year_Day(Year, Day_Of_Year);

      if (Tmp_Day >= Target_Day) then
         Day_Of_Year := (Day_Of_Year
                         - ((Tmp_Day - Target_Day) + (7 * (Z.Day - 1))));
      else
         Day_Of_Year := (Day_Of_Year
                         - ((Tmp_Day - Target_Day) + (7 * Z.Day)));
      end if;
      Rv := (Day_Of_Year  => Day_Of_Year,
             Hour_Of_Day  => Z.Hour_Of_Day,
             Minute       => Z.Minute,
             Second       => Z.Second,
             Is_Wall_Time => True);
      Update_Time(Rv, Z.Time_Type, UTC_Off);
      return Rv;
   end Get_Zone_Change;

   function Get_Zone_Change(Z       : in Weekday_On_Or_After_Day_Of_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Year_Range)
                            return Zone_Change_Time is
      Day_Of_Year : Integer;
      Month       : Month_Range := Short_Month_To_Month_Range(Z.Month);
      Rv          : Zone_Change_Time;
   begin
      Day_Of_Year := Day_Of_Year_From_Month_Week
        (Year, Month, Z.Weekday, 1, Z.Day);
      Rv := (Day_Of_Year  => Day_Of_Year,
             Hour_Of_Day  => Z.Hour_Of_Day,
             Minute       => Z.Minute,
             Second       => Z.Second,
             Is_Wall_Time => True);
      Update_Time(Rv, Z.Time_Type, UTC_Off);
      return Rv;
   end Get_Zone_Change;

   function Get_Zone_Change(Z      : in Weekday_On_Or_Before_Day_Of_Month_Mode;
                            UTC_Off : in Integer;
                            Year    : in Year_Range)
                            return Zone_Change_Time is
      Target_Day  : Integer := Short_Weekday'Pos(Z.Weekday) + 1;
      Day_Of_Year : Integer;
      Tmp_Day     : Integer;
      Month       : Month_Range := Short_Month_To_Month_Range(Z.Month);
      Rv          : Zone_Change_Time;
   begin
      Day_Of_Year := Month_Day_To_Year_Day(Year, Month,
                                           Days_In_Month(Year, Month));
      Tmp_Day := Weekday_From_Year_Day(Year, Day_Of_Year);

      if (Tmp_Day >= Target_Day) then
         Day_Of_Year := (Day_Of_Year - (Tmp_Day - Target_Day));
      else
         Day_Of_Year := (Day_Of_Year - ((Tmp_Day - Target_Day) + 7));
      end if;
      Rv := (Day_Of_Year  => Day_Of_Year,
             Hour_Of_Day  => Z.Hour_Of_Day,
             Minute       => Z.Minute,
             Second       => Z.Second,
             Is_Wall_Time => True);
      Update_Time(Rv, Z.Time_Type, UTC_Off);
      return Rv;
   end Get_Zone_Change;

   Default_Zone : Time_Zone_Class := null;

   procedure Set_Default_Timezone(Zone : in Time_Zone_Class) is
   begin
      Default_Zone := Zone;
   end Set_Default_Timezone;

   function Get_Default_Timezone return Time_Zone_Class is
   begin
      return Default_Zone;
   end Get_Default_Timezone;

   UTC_Zone : Time_Zone_Class := Allocate_No_DST_Zone("UTC", "UTC", 0);

begin
   Default_Timezones := new Zone_Factory;
   Default_Zone := UTC_Zone;
end Asl.Date_Time.Timezone;
