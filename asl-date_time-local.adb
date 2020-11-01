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

with Asl.Date_Time.Timezone; use Asl.Date_Time.Timezone;
with Ada.Calendar; use Ada.Calendar;
with Asl.Date_Time.Timezone.Simple; use Asl.Date_Time.Timezone.Simple;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Asl.Strings; use Asl.Strings;

package body Asl.Date_Time.Local is

   procedure Findlocalzone;
   pragma Import(C, Findlocalzone, "findlocalzone");

   function Try_Zone(C_Zone       : Chars_Ptr;
                     C_Dst_Active : Int;
                     C_UTC_Offset : Long;
                     C_Off_Id     : Chars_Ptr;
                     C_On_Id      : Chars_Ptr)
                     return Int;
   pragma Export(C, Try_Zone, "try_zone");

   function Try_Zone(C_Zone       : Chars_Ptr;
                     C_Dst_Active : Int;
                     C_UTC_Offset : Long;
                     C_Off_Id     : Chars_Ptr;
                     C_On_Id      : Chars_Ptr)
                     return Int is
      Zone       : Time_Zone_Class;
      Zone_Name  : String := Value(C_Zone);
      Off_Id     : String := Value(C_Off_Id);
      On_Id      : String := Value(C_On_Id);
      Cal        : Calendar_Time;
      UTC_Offset : Integer;
      DST        : DST_State;
      DST_Offset : Integer;
      Found_Off  : Boolean := False;
      Found_On   : Boolean := False;
      Year       : Year_Range;
      Day        : Day_Of_Year_Range;
   begin
      Zone := Find_Timezone(Default_Timezones.all, Zone_Name);
      if (Zone = null) then
         return 0;
      end if;

      Set_Timezone(Cal, Zone);
      Set_Time(Cal, Clock);
      Compute_Fields(Cal);
      UTC_Offset := Get_UTC_Offset(Zone.all,
                                   Get_Year(Cal),
                                   Get_Day_Of_Year(Cal),
                                   Get_Hour_Of_Day(Cal),
                                   Get_Minute(Cal),
                                   Get_Second(Cal));

      if (UTC_Offset /= Integer(C_UTC_Offset)) then
         return 0;
      end if;

      Year := Get_Year(Cal);
      for I in Month_Range'Range loop
         Day := Month_Day_To_Year_Day(Year, I, 1);
         Daylight_Savings_Active(DST, DST_Offset, Zone.all, Year, Day, 0, 0, 0);
         if (DST = On) then
            if (not Found_On) then
               Found_On := True;
               declare
                  Str : String := Get_Id(Zone.all,
                                         True,
                                         Year,
                                         Day, 0, 0, 0);
               begin
                  if (Compare(Str, On_Id) /= 0) then
                     -- The strings don't match, so no match.
                     return 0;
                  end if;
               end;
            end if;
         elsif (DST = Off) then
            if (not Found_Off) then
               Found_Off := True;
               declare
                  Str : String := Get_Id(Zone.all,
                                         True,
                                         Year,
                                         Day, 0, 0, 0);
               begin
                  if (Compare(Str, Off_Id) /= 0) then
                     -- The strings don't match, so no match.
                     return 0;
                  end if;
               end;
            end if;
         end if;
         exit when (Found_On and Found_Off);
      end loop;

      -- We have to both have DST or both not have DST.
      if ((C_DST_Active = 1) = Found_On) then
         Set_Default_Timezone(Zone);
         return 1;
      else
         return 0;
      end if;
   end Try_Zone;

   procedure Set_Local_Timezone is
   begin
      Findlocalzone;
   end Set_Local_Timezone;

   type C_Portable_Time is record
      Year           : Long;
      Second_Of_Year : Long;
      Microsecond    : Long;
   end record;
   pragma Convention(C, C_Portable_Time);
   type C_Portable_Time_Ptr is access all C_Portable_Time;
   pragma Convention(C, C_Portable_Time_Ptr);

   procedure Asl_Date_Time_Get_Curr_Portable_Time
     (Pt : in C_Portable_Time_Ptr);
   pragma Import(C, Asl_Date_Time_Get_Curr_Portable_Time,
                 "asl_date_time_get_curr_portable_time");

   function Get_Local_Time return Calendar_Time is
      CPt    : aliased C_Portable_Time;
      Pt     : Portable_Time;
      Rv     : Calendar_Time;
   begin
      Asl_Date_Time_Get_Curr_Portable_Time(CPt'Unchecked_Access);
      Pt.Year := Integer(Cpt.Year);
      Pt.Second_Of_Year := Integer(Cpt.Second_Of_Year);
      Pt.Microsecond := Integer(Cpt.Microsecond);
      Set_Time(Rv, Pt);
      return Rv;
   end Get_Local_Time;

   function Get_UTC_Time return Calendar_Time is
      CPt    : aliased C_Portable_Time;
      Pt     : Portable_Time;
      Rv     : Calendar_Time;
   begin
      Asl_Date_Time_Get_Curr_Portable_Time(CPt'Unchecked_Access);
      Pt.Year := Integer(Cpt.Year);
      Pt.Second_Of_Year := Integer(Cpt.Second_Of_Year);
      Pt.Microsecond := Integer(Cpt.Microsecond);
      Set_Timezone(Rv, Find_Timezone(Default_Timezones.all, "UTC"));
      Set_Time(Rv, Pt);
      return Rv;
   end Get_UTC_Time;

end Asl.Date_Time.Local;
