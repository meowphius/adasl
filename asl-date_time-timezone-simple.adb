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
with Asl.Strings; use Asl.Strings;

package body Asl.Date_Time.Timezone.Simple is

   function Get_UTC_Offset(Z           : in Simple_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer is
   begin
      return Z.Offset;
   end Get_UTC_Offset;

   procedure Daylight_Savings_Active(DST_Active  : out Dst_State;
                                     DST_Offset  : out Integer;
                                     Z           : in Simple_Zone;
                                     Year        : in Year_Range;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range) is
      -- We get the start time with zero DST offset, because when DST
      -- starts there will not be any DST active.
      Start_D : Zone_Change_Time := Get_Zone_Change(Z.Start_Day.all,
                                                    Z.Offset,
                                                    Year);
      -- We get the start time with DST offset, because when DST
      -- ends the DST offset will be in the time.
      End_D   : Zone_Change_Time := Get_Zone_Change(Z.End_Day.all,
                                                    Z.Offset,
                                                    Year);
      Tmp_D   : Zone_Change_Time := (Day_Of_Year, Hour_Of_Day, Minute,
                                     Second, True);
      Active  : Boolean;
   begin
      if (not End_D.Is_Wall_Time) then
         -- If the end time is not in wall time, then add the DST value
         -- to it to move it to wall time.
         End_D := End_D + Z.DST_Offset;
         End_D.Is_Wall_Time := True;
      end if;
      -- Start time is always wall time, because DST is not active.
      Start_D.Is_Wall_Time := True;

      -- If the start is after the end, then we are south of the equator,
      -- so the zones pass over the new year.
      if (Start_D > End_D) then
         Active :=  (Tmp_D >= Start_D) or (Tmp_D < End_D);
      else
         Active := (Tmp_D >= Start_D) and (Tmp_D < End_D);
      end if;

      if (Active) then
         DST_Offset := Z.DST_Offset;
         DST_Active := On;
         if (Tmp_D.Day_Of_Year = Start_D.Day_Of_Year) then
            Start_D := Start_D + DST_Offset;
            if (Tmp_D < Start_D) then
               DST_Active := No_Mans_Land;
            end if;
         elsif (Tmp_D.Day_Of_Year = End_D.Day_Of_Year) then
            End_D := End_D + (-DST_Offset);
            if (Tmp_D >= End_D) then
               DST_Active := Overlay;
            end if;
         end if;
      else
         DST_Offset := 0;
         DST_Active := Off;
      end if;
   end Daylight_Savings_Active;

   function Get_Id(Z           : in Simple_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String is
      Active : DST_State;
      Offset : Integer;
   begin
      Daylight_Savings_Active(Active, Offset,
                              Z, Year, Day_Of_Year,
                              Hour_Of_Day, Minute, Second);
      case Active is
         when Off =>
            return Z.Off_Id;

         when On | No_Mans_Land =>
            -- FIXME - should No_Mans_Land raise an exception?
            return Z.On_Id;

         when Overlay =>
            if Is_Active then
               return Z.On_Id;
            else
               return Z.Off_Id;
            end if;
      end case;
   end Get_Id;

   function Duplicate_Zone(Name   : in String;
                           Source : in Simple_Zone)
                           return Time_Zone_Class is
      Rv : Simple_Zone_Ptr := new Simple_Zone(Name'Length,
                                              Source.On_Id'Length,
                                              Source.Off_Id'Length);
   begin
      Set_Name(Rv.all, Name);
      Rv.On_Id := Source.On_Id;
      Rv.Off_Id := Source.Off_Id;
      Rv.Offset := Source.Offset;
      Rv.Start_Day := Source.Start_Day;
      Rv.End_Day := Source.End_Day;
      return Time_Zone_Class(Rv);
   end Duplicate_Zone;


   function Allocate_Simple_Zone(Name      : in String;
                                 On_Id     : in String;
                                 Off_Id    : in String;
                                 Offset    : in Integer;
                                 Start_Day : in Zone_Mode_Class;
                                 End_Day   : in Zone_Mode_Class)
                                 return Time_Zone_Class is
      Rv : Simple_Zone_Ptr := new Simple_Zone(Name'Length,
                                              On_Id'Length,
                                              Off_Id'Length);
   begin
      Rv.Offset := Offset;
      Rv.Start_Day := Start_Day;
      Rv.End_Day := End_Day;
      Set_Name(Rv.all, Name);
      Rv.On_Id := On_Id;
      Rv.Off_Id := Off_Id;
      return Time_Zone_Class(Rv);
   end Allocate_Simple_Zone;

end Asl.Date_Time.Timezone.Simple;
