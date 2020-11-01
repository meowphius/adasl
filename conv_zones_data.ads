-- The Ada Structured Platform - A set of utilities the form the base
-- of a platform.
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

with Asgc.Ordered.Sortable.Alist.Expandable;
with Asgc.Ordered.Sortable.Bubble_Sort;
with Asgc.Btree.Dynamic;
with Asl.Date_Time; use Asl.Date_Time;

-- This is the data associated with the zone conversion utility.  These are
-- like the data structures in asl-date_time-timezones-complex, but are
-- more dynamic.

package Conv_Zones_Data is

   type Zone_HMS is record
      Hour     : Hour_Of_Day_Range;
      Minute   : Minute_Range;
      Second   : Second_Range;
      HMS_Type : Time_Types;
   end record;

   type Zone_Day_Types is
      (Day_Of_Month, -- A specific day of month.
       Last_Weekday, -- The last day of the week in the month
       Weekday_GE,   -- The first day of the week on or after the day of month
       Weekday_LE);  -- The last day of the week on or before the day of month

   type Zone_Day_Choice is record
      Day_Type     : Zone_Day_Types;
      Weekday      : Weekday_Range;
      Day_Of_Month : Day_Of_Month_Range;
   end record;

   type Zone_Date is record
      Year  : Integer;
      Month : Month_Range;
      Day   : Zone_Day_Choice;
      HMS   : Zone_HMS;
   end record;

   type Zone_Rule_Range_Info is record
      From  : Integer; -- Start year
      To    : Integer; -- End year
      -- no type, since it doesn't seem to be used
      Month : Month_Range;     -- Month this occurs in.
      Day   : Zone_Day_Choice; -- How to tell which day of the month to choose.
      Start : Zone_HMS;        -- Time of day when it occurs.
      Save  : Integer;         -- Amount of time to add.  Zero means no DST.

      Letters     : String(1 .. 4);
      Letters_Len : Integer;
   end record;

   function "="(R1, R2 : in Zone_Rule_Range_Info) return Boolean;
   function ">"(R1, R2 : in Zone_Rule_Range_Info) return Boolean;
   function "<"(R1, R2 : in Zone_Rule_Range_Info) return Boolean;
   function ">="(R1, R2 : in Zone_Rule_Range_Info) return Boolean;
   function "<="(R1, R2 : in Zone_Rule_Range_Info) return Boolean;

   package ZR_Container is new Asgc(Zone_Rule_Range_Info);
   package ZR_Ordered is new ZR_Container.Ordered;
   package ZR_Sortable is new ZR_Ordered.Sortable;
   package ZR_Alist1 is new ZR_Sortable.Alist;
   package ZR_Alist is new ZR_Alist1.Expandable;
   package ZR_Sort is new ZR_Sortable.Bubble_Sort;
   use ZR_Alist;

   type Zone_Rules(Name_Length : Integer) is record
      Name  : String(1 .. Name_Length);
      Rules : aliased ZR_Alist.Object(10, 10);
   end record;
   type Zone_Rules_Ptr is access all Zone_Rules;

   function Eq(D1, D2 : in Zone_Rules_Ptr) return Boolean;
   function ">"(D1, D2 : in Zone_Rules_Ptr) return Boolean;
   function "<"(D1, D2 : in Zone_Rules_Ptr) return Boolean;
   function ">="(D1, D2 : in Zone_Rules_Ptr) return Boolean;
   function "<="(D1, D2 : in Zone_Rules_Ptr) return Boolean;

   package Zone_Rules_Container is new Asgc(Zone_Rules_Ptr, Eq);
   package Zone_Rules_Btree1 is new Zone_Rules_Container.Btree;
   package Zone_Rules_Btree is new Zone_Rules_Btree1.Dynamic;

   type Zone_Rule_Kind is (None, Offset, Rule);

   function "="(D1, D2 : in Zone_Date) return Boolean;
   function ">"(D1, D2 : in Zone_Date) return Boolean;
   function "<"(D1, D2 : in Zone_Date) return Boolean;
   function ">="(D1, D2 : in Zone_Date) return Boolean;
   function "<="(D1, D2 : in Zone_Date) return Boolean;

   type String_Ptr is access all String;

   type Zone_Entry is record
      GMT_Offset : Integer;
      Kind       : Zone_Rule_Kind;
      Offset     : Integer;
      Rules      : String_Ptr;
      Format     : String(1 .. 10);
      Format_Len : Integer;
      Has_Until  : Boolean;
      Ends_On    : Zone_Date;
   end record;

   function "="(D1, D2 : in Zone_Entry) return Boolean;
   function ">"(D1, D2 : in Zone_Entry) return Boolean;
   function "<"(D1, D2 : in Zone_Entry) return Boolean;
   function ">="(D1, D2 : in Zone_Entry) return Boolean;
   function "<="(D1, D2 : in Zone_Entry) return Boolean;

   package ZE_Container is new Asgc(Zone_Entry);
   package ZE_Ordered is new ZE_Container.Ordered;
   package ZE_Sortable is new ZE_Ordered.Sortable;
   package ZE_Alist1 is new ZE_Sortable.Alist;
   package ZE_Alist is new ZE_Alist1.Expandable;
   package ZE_Sort is new ZE_Sortable.Bubble_Sort;
   use ZE_Alist;

   type Zone_Link;
   type Zone_Link_Ptr is access all Zone_Link;
   type Zone_Link(Name_Length : Integer) is record
      Name : String(1 .. Name_Length);
      Next : Zone_Link_Ptr;
   end record;

   type Zone;
   type Zone_Ptr is access all Zone;
   type Zone(Name_Length : Integer) is record
      Name      : String(1 .. Name_Length);
      Is_Link   : Boolean := False;
      Real_Link : Zone_Ptr;
      Entries   : aliased ZE_Alist.Object(10, 10);
      Links     : Zone_Link_Ptr;
   end record;

   function "="(D1, D2 : in Zone_Ptr) return Boolean;
   function ">"(D1, D2 : in Zone_Ptr) return Boolean;
   function "<"(D1, D2 : in Zone_Ptr) return Boolean;
   function ">="(D1, D2 : in Zone_Ptr) return Boolean;
   function "<="(D1, D2 : in Zone_Ptr) return Boolean;

   package Zone_Container is new Asgc(Zone_Ptr);
   package Zone_Btree1 is new Zone_Container.Btree;
   package Zone_Btree is new Zone_Btree1.Dynamic;

   type Zone_Weekdays is (Sunday, Monday, Tuesday, Wednesday, Thursday,
                          Friday, Saturday, Sun, Mon, Tue, Wed, Thu, Fri, Sat);

   function Num_From_Zone_Weekday(Z : in Zone_Weekdays) return Weekday_Range;

   type Zone_Months is (January, February, March, April, May, June, July,
                        August, September, October, November, December,
                        Jan, Feb, Mar, Apr, asdf, Jun, Jul, Aug, Sep, Oct,
                        Nov, Dec);

   function Num_From_Zone_Month(Z : in Zone_Months) return Month_Range;

end Conv_Zones_Data;
