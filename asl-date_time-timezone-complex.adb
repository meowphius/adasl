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
with Asgc.Btree.Dynamic;
with Asl.Strings; use Asl.Strings;

package body Asl.Date_Time.Timezone.Complex is

   procedure Local_DST_Active(DST_Active  : out DST_State;
                              DST_Offset  : out Integer;
                              Z           : in Complex_Zone'Class;
                              Year        : in Integer;
                              Cmp_T       : in Zone_Change_Time;
                              Check_DST   : in Boolean);

   -- Find the closes rule whose time comes before the given time.  If no
   -- rule exists that matches the condition, then zero is returned.  This
   -- always works in wall time, if it finds an entry that is standard
   -- time, it will adjust it to wall time.
   procedure Find_Prev_Rule(Rule_Idx    : out Integer;
                            Closest_T   : out Zone_Change_Time;
                            UTC_Off     : in Integer;
                            Zone        : in Complex_Zone'Class;
                            Z           : in Zone_Rules;
                            Year        : in Integer;
                            Cmp_T       : in Zone_Change_Time;
                            Check_DST   : in Boolean) is
      Closest_Idx      : Integer := 0;
      Test_T           : Zone_Change_Time;
   begin
      Closest_T := (1, 0, 0, 0, True); -- get rid of compiler warnings
      for I in reverse 1 .. Z.Last_Rule loop
         -- The rules are sorted by the "To" field, so once we find a "To"
         -- field smaller than ours we are done.
         exit when (Year > Z.Rules(I).To);

         -- We know that year <= To, so that check is not needed.
         if (Year >= Z.Rules(I).From) then
            -- The year is in range, get the actual day and time info.
            Test_T := Get_Zone_Change(Z.Rules(I).Mode.all, UTC_Off, Year);

            if (Check_DST and (not Test_T.Is_Wall_Time)) then
               -- The time is not wall clock time and we need to check it,
               -- so find if daylight savings is active when the entry
               -- starts.

               declare
                  Active : DST_State;
                  Offset : Integer;
                  Tst2_T : Zone_Change_Time;
               begin
                  Tst2_T := Test_T + (-1);
                  Local_DST_Active(Active,
                                   Offset,
                                   Zone,
                                   Year,
                                   Tst2_T,
                                   False);
                  Test_T := Test_T + Offset;
                  Test_T.Is_Wall_Time := True;
               end;
            end if;

            -- We want the closest rule not greater than the input time,
            -- and we also have to account for initial conditions.
            if ((Test_T <= Cmp_T)
                and ((Closest_Idx = 0) or else (Test_T > Closest_T)))
            then
               Closest_T := Test_T;
               Closest_Idx := I;
            end if;
         end if;
      end loop;
      Rule_Idx := Closest_Idx;
   end Find_Prev_Rule;


   -- Find the closes rule whose time comes after the given time.  If no
   -- rule exists that matches the condition, then zero is returned.  This
   -- always works in wall time, if it finds an entry that is standard
   -- time, it will adjust it to wall time.
   procedure Find_Next_Rule(Rule_Idx    : out Integer;
                            Closest_T   : out Zone_Change_Time;
                            UTC_Off     : in Integer;
                            Zone        : in Complex_Zone'Class;
                            Z           : in Zone_Rules;
                            Year        : in Integer;
                            Cmp_T       : in Zone_Change_Time) is
      Closest_Idx      : Integer := 0;
      Test_T           : Zone_Change_Time;
   begin
      Closest_T := (1, 0, 0, 0, True); -- get rid of compiler warnings
      for I in reverse 1 .. Z.Last_Rule loop
         -- The rules are sorted by the "To" field, so once we find a "To"
         -- field smaller than ours we are done.
         exit when (Year > Z.Rules(I).To);

         -- We know that year <= To, so that check is not needed.
         if (Year >= Z.Rules(I).From) then
            -- The year is in range, get the actual day and time info.
            Test_T := Get_Zone_Change(Z.Rules(I).Mode.all, UTC_Off, Year);

            if (not Test_T.Is_Wall_Time) then
               -- The time is not wall clock time and we need to check it,
               -- so find the previous entry for this to tell if there is
               -- some time offset.

               declare
                  Active : DST_State;
                  Offset : Integer;
               begin
                  Local_DST_Active(Active,
                                   Offset,
                                   Zone,
                                   Year,
                                   Test_T + (-1),
                                   False);
                  Test_T := Test_T + Offset;
                  Test_T.Is_Wall_Time := True;
               end;
            end if;

            -- We want the closest rule not greater than the input time,
            -- and we also have to account for initial conditions.
            if ((Test_T > Cmp_T)
                and ((Closest_Idx = 0) or else (Test_T < Closest_T)))
            then
               Closest_T := Test_T;
               Closest_Idx := I;
            end if;
         end if;
      end loop;

      Rule_Idx := Closest_Idx;
   end Find_Next_Rule;

   -- Find the proper entry
   function Find_Entry(Z           : in Complex_Zone'Class;
                       Year        : in Integer;
                       Cmp_T       : in Zone_Change_Time;
                       Check_DST   : in Boolean)
                       return Integer is
      Ent_T    : Zone_Change_Time;
      Tst_T    : Zone_Change_Time;
      Rule_Idx : Integer;
      Rules    : Zone_Rules_Ptr;
   begin
      for I in 1 .. Z.Last_Entry-1 loop
         Ent_T := Get_Zone_Change(Z.Entries(I).Ends_On.all,
                                  Z.Entries(I).UTC_Offset,
                                  Year);
         if (not Ent_T.Is_Wall_Time) then
            -- If the entry's time is specified in standard time, convert
            -- it to wall clock time.  Since the given time references
            -- the end of the period, we only need to use what is inside
            -- this period, we don't have to go to previous ones.
            case Z.Entries(I).Kind is
               when None =>
                  null;

               when Offset =>
                  Ent_T := Ent_T + Z.Entries(I).Offset;

               when Rule =>
                  -- We need to find if we are currently in daylight savings
                  -- using the rules.
                  Rules := Z.Entries(I).Rules;
                  Find_Prev_Rule(Rule_Idx, Tst_T,
                                 Z.Entries(I).UTC_Offset, Z, Rules.all,
                                 Z.Entries(I).Ends_Year, Ent_T, Check_DST);
                  if (Rule_Idx = 0) then
                     -- Didn't find a previous rule in this year, try last
                     -- year.
                     Find_Prev_Rule(Rule_Idx, Tst_T,
                                    Z.Entries(I).UTC_Offset, Z, Rules.all,
                                    Z.Entries(I).Ends_Year - 1,
                                    (Days_In_Year(Z.Entries(I).Ends_Year-1),
                                     23, 59, 59, True), Check_DST);
                  end if;

                  if (Rule_Idx /= 0) then
                     -- We found a rule, so everything's ok.  If we didn't
                     -- find a rule for a year, that means no DST.
                     Ent_T := Ent_T + Rules.Rules(Rule_Idx).Save;
                  end if;
            end case;
         end if;

         if ((Z.Entries(I).Ends_Year > Year)
             or ((Z.Entries(I).Ends_Year > Year) and (Ent_T >= Cmp_T)))
         then
            return I;
         end if;
      end loop;

      return Z.Last_Entry;
   end Find_Entry;

   function Get_UTC_Offset(Z           : in Complex_Zone;
                           Year        : in Integer;
                           Day_Of_Year : in Day_Of_Year_Range;
                           Hour_Of_Day : in Hour_Of_Day_Range;
                           Minute      : in Minute_Range;
                           Second      : in Second_Range)
                           return Integer is
      Ent_Idx : Integer;
      Cmp_T   : Zone_Change_Time := (Day_Of_Year, Hour_Of_Day, Minute,
                                     Second, True);
   begin
      Ent_Idx := Find_Entry(Z, Year, Cmp_T, True);
      return Z.Entries(Ent_Idx).UTC_Offset;
   end Get_UTC_Offset;

   procedure Local_DST_Active(DST_Active  : out DST_State;
                              DST_Offset  : out Integer;
                              Z           : in Complex_Zone'Class;
                              Year        : in Integer;
                              Cmp_T       : in Zone_Change_Time;
                              Check_DST   : in Boolean) is
      Ent_Idx       : Integer;
      Rule_Idx      : Integer;
      Next_Rule_Idx : Integer;
      Rules         : Zone_Rules_Ptr;
      Start_T       : Zone_Change_Time;
      End_T         : Zone_Change_Time;
      Lapped_Year   : Boolean := False;
   begin
      Ent_Idx := Find_Entry(Z, Year, Cmp_T, Check_DST);
      case Z.Entries(Ent_Idx).Kind is
         when None =>
            DST_Active := Off;
            DST_Offset := 0;

         when Offset =>
            DST_Offset := Z.Entries(Ent_Idx).Offset;
            if (DST_Offset /= 0) then
               DST_Active := On;
            else
               DST_Active := Off;
            end if;

         when Rule =>
            Rules := Z.Entries(Ent_Idx).Rules;
            Find_Prev_Rule(Rule_Idx, Start_T, Z.Entries(Ent_Idx).UTC_Offset,
                           Z, Rules.all, Year, Cmp_T, Check_DST);
            if (Rule_Idx = 0) then
               Find_Prev_Rule(Rule_Idx, Start_T, Z.Entries(Ent_Idx).UTC_Offset,
                              Z, Rules.all, Year-1,
                              (Days_In_Year(Year-1), 23, 59, 59, True),
                              Check_DST);
               Lapped_Year := True;
            end if;
            if (Rule_Idx = 0) then
               -- No rule, so assume DST is off.
               DST_Active := Off;
               DST_Offset := 0;
            else
               DST_Offset := Rules.Rules(Rule_Idx).Save;
               if (DST_Offset /= 0) then
                  -- If we currently have an offset, then we need to check
                  -- for no-mans-land and overlays, but only if we were not
                  -- told to avoid this check.
                  if (not Check_DST) then
                     DST_Active := On;
                  elsif ((not Lapped_Year) and (Cmp_T < (Start_T + DST_Offset)))
                  then
                     -- If we found the rule in the previous year, we
                     -- ignore it for this comparison, also, since hopefully
                     -- DST changes never occur over a year, and the
                     -- comparisons get messed up then.
                     DST_Active := No_Mans_Land;
                  else
                     -- We assume that is the next rule is not in the current
                     -- year, then we are not close enough to be in an overlay.
                     Find_Next_Rule(Next_Rule_Idx, End_T,
                                    Z.Entries(Ent_Idx).UTC_Offset,
                                    Z, Rules.all, Year, Cmp_T);
                     if (Next_Rule_Idx = 0) then
                        Dst_Active := On;
                     elsif ((Cmp_T + DST_Offset) >= End_T) then
                        DST_Active := Overlay;
                     else
                        Dst_Active := On;
                     end if;
                  end if;
               else
                  DST_Active := Off;
               end if;
            end if;
      end case;
   end Local_DST_Active;

   procedure Daylight_Savings_Active(DST_Active  : out DST_State;
                                     DST_Offset  : out Integer;
                                     Z           : in Complex_Zone;
                                     Year        : in Integer;
                                     Day_Of_Year : in Day_Of_Year_Range;
                                     Hour_Of_Day : in Hour_Of_Day_Range;
                                     Minute      : in Minute_Range;
                                     Second      : in Second_Range) is
      Cmp_T : Zone_Change_Time := (Day_Of_Year, Hour_Of_Day, Minute,
                                   Second, True);
   begin
      Local_DST_Active(DST_Active, DST_Offset, Z, Year, Cmp_T, True);
   end Daylight_Savings_Active;

   function Get_Format(E      : in Zone_Entry;
                       L      : in String;
                       In_DST : in Boolean)
                       return String is
      Str  : String(1 .. E.Format_Len * (L'Length + 1));
      Last : Integer := E.Format_Len;
      I    : Integer;
   begin
      Str(1 .. E.Format_Len) := E.Format(1 .. E.Format_Len);

      -- If a '/' is in the format, the the part before the '/' is the standard
      -- time string and the part after the '/' is the DST time string.
      for J in 1 .. E.Format_Len loop
         if (Str(J) = '/') then
            if (In_DST) then
               Str(1 .. Last-J) := Str(J+1 .. Last);
               Last := Last - J;
            else
               Last := J - 1;
            end if;
            exit;
         end if;
      end loop;

      I := Str'First;
      while (I < Last) loop
         if (Str(I .. I+1) = "%s") then
            if (L'Length = 0) then
               Last := Last - 2;
               for J in I .. Last loop
                  Str(J) := Str(J+2);
               end loop;
            elsif (L'Length = 1) then
               Str(I) := L(1);
               I := I + 1;
               Last := Last - 1;
               for J in I .. Last loop
                  Str(J) := Str(J+1);
               end loop;
            elsif (L'Length = 2) then
               I := I + 2;
               Str(I .. I+1) := L;
            else
               for J in reverse I+1 .. Last loop
                  Str(J+L'Length-2) := Str(J);
               end loop;
               Str(I .. I+L'Length-1) := L;
               I := I + L'Length;
               Last := Last + L'Length - 2;
            end if;
         else
            I := I + 1;
         end if;
      end loop;

      return Str(1 .. Last);
   end Get_Format;

   function Get_Id(Z           : in Complex_Zone;
                   Is_Active   : in Boolean;
                   Year        : in Integer;
                   Day_Of_Year : in Day_Of_Year_Range;
                   Hour_Of_Day : in Hour_Of_Day_Range;
                   Minute      : in Minute_Range;
                   Second      : in Second_Range)
                   return String is
      Ent_Idx        : Integer;
      Rule_Idx       : Integer;
      Next_Rule_Idx  : Integer;
      Let_Len        : Integer;
      Rules          : Zone_Rules_Ptr;
      Cmp_T          : Zone_Change_Time := (Day_Of_Year, Hour_Of_Day,
                                            Minute, Second, True);
      Start_T        : Zone_Change_Time;
      End_T          : Zone_Change_Time;
      Len            : Integer;
      DST_Offset     : Integer;
      Now_Active     : Boolean;
   begin
      Ent_Idx := Find_Entry(Z, Year, Cmp_T, True);
      case Z.Entries(Ent_Idx).Kind is
         when None | Offset =>
            Len := Z.Entries(Ent_Idx).Format_Len;
            return Z.Entries(Ent_Idx).Format(1 .. Len);

         when Rule =>
            Rules := Z.Entries(Ent_Idx).Rules;
            Find_Prev_Rule(Rule_Idx, Start_T, Z.Entries(Ent_Idx).UTC_Offset,
                           Z, Rules.all, Year, Cmp_T, True);
            if (Rule_Idx = 0) then
               return Get_Format(Z.Entries(Ent_Idx), "", False);
            else
               DST_Offset := Rules.Rules(Rule_Idx).Save;
               if (DST_Offset /= 0) then
                  if (Cmp_T < (Start_T + DST_Offset)) then
                     -- FIXME - should this raise an exception?
                     Now_Active := True;
                  else
                     Find_Next_Rule(Next_Rule_Idx, End_T,
                                    Z.Entries(Ent_Idx).UTC_Offset,
                                    Z, Rules.all, Year, Cmp_T);
                     if (Next_Rule_Idx = 0) then
                        Now_Active := True;
                     elsif ((Cmp_T + DST_Offset) > End_T) then
                        if (Is_Active) then
                           Now_Active := True;
                        else
                           Now_Active := False;
                           Rule_Idx := Next_Rule_Idx;
                        end if;
                     else
                        Now_Active := True;
                     end if;
                  end if;
               else
                  Now_Active := False;
               end if;
            end if;

            Let_Len := Rules.Rules(Rule_Idx).Letters_Len;
            return Get_Format(Z.Entries(Ent_Idx),
                              Rules.Rules(Rule_Idx).Letters(1 .. Let_Len),
                              Now_Active);
      end case;
   end Get_Id;

   function Duplicate_Zone(Name   : in String;
                           Source : in Complex_Zone)
                           return Time_Zone_Class is
      New_Zone : Complex_Zone_Ptr := new Complex_Zone(Name'Length,
                                                      Source.Entry_Count);
   begin
      Set_Name(New_Zone.all, Name);
      New_Zone.Entries := Source.Entries;
      New_Zone.Last_Entry := Source.Last_Entry;
      return Time_Zone_Class(New_Zone);
   end Duplicate_Zone;

   function Allocate_Complex_Zone(Name        : in String;
                                  Entry_Count : in Integer)
                                  return Complex_Zone_Ptr is
      Rv : Complex_Zone_Ptr := new Complex_Zone(Name_Length => Name'Length,
                                                Entry_Count => Entry_Count);
   begin
      Set_Name(Rv.all, Name);
      return Rv;
   end Allocate_Complex_Zone;

   -- An entry with no DST.
   procedure Add_Entry(Zone        : in out Complex_Zone;
                       UTC_Offset  : in Integer;
                       Format      : in String;
                       Has_Ends_On : in Boolean;
                       Ends_Year   : in Year_Range;
                       Ends_Mode   : in Zone_Mode_Class) is
      New_Entry : Zone_Entry;
   begin
      New_Entry.UTC_Offset := UTC_Offset;
      New_Entry.Kind := None;
      New_Entry.Format(1 .. Format'Length) := Format;
      New_Entry.Format_Len := Format'Length;
      New_Entry.Has_End := Has_Ends_On;
      New_Entry.Ends_Year := Ends_Year;
      New_Entry.Ends_On := Ends_Mode;
      Zone.Last_Entry := Zone.Last_Entry + 1;
      Zone.Entries(Zone.Last_Entry) := New_Entry;
   end Add_Entry;

   -- An entry using rules.
   procedure Add_Entry(Zone        : in out Complex_Zone;
                       UTC_Offset  : in Integer;
                       Format      : in String;
                       Rules       : in String;
                       Has_Ends_On : in Boolean;
                       Ends_Year   : in Year_Range;
                       Ends_Mode   : in Zone_Mode_Class) is
      New_Entry : Zone_Entry;
   begin
      New_Entry.UTC_Offset := UTC_Offset;
      New_Entry.Kind := Rule;
      New_Entry.Rules := Find_Rule(Rules);
      New_Entry.Format(1 .. Format'Length) := Format;
      New_Entry.Format_Len := Format'Length;
      New_Entry.Has_End := Has_Ends_On;
      New_Entry.Ends_Year := Ends_Year;
      New_Entry.Ends_On := Ends_Mode;
      Zone.Last_Entry := Zone.Last_Entry + 1;
      Zone.Entries(Zone.Last_Entry) := New_Entry;
   end Add_Entry;

   -- An entry using a direct offset.
   procedure Add_Entry(Zone        : in out Complex_Zone;
                       UTC_Offset  : in Integer;
                       Format      : in String;
                       Save_Offset : in Integer;
                       Has_Ends_On : in Boolean;
                       Ends_Year   : in Year_Range;
                       Ends_Mode   : in Zone_Mode_Class) is
      New_Entry : Zone_Entry;
   begin
      New_Entry.UTC_Offset := UTC_Offset;
      New_Entry.Kind := Offset;
      New_Entry.Offset := Save_Offset;
      New_Entry.Format(1 .. Format'Length) := Format;
      New_Entry.Format_Len := Format'Length;
      New_Entry.Has_End := Has_Ends_On;
      New_Entry.Ends_Year := Ends_Year;
      New_Entry.Ends_On := Ends_Mode;
      Zone.Last_Entry := Zone.Last_Entry + 1;
      Zone.Entries(Zone.Last_Entry) := New_Entry;
   end Add_Entry;

   function Eq(Z1, Z2 : Zone_Rules_Ptr) return Boolean is
   begin
      return Compare(Z1.Name, Z2.Name) = 0;
   end Eq;

   function ">"(Z1, Z2 : Zone_Rules_Ptr) return Boolean is
   begin
      return Compare(Z1.Name, Z2.Name) > 0;
   end ">";

   function "<"(Z1, Z2 : Zone_Rules_Ptr) return Boolean is
   begin
      return Compare(Z1.Name, Z2.Name) < 0;
   end "<";

   function ">="(Z1, Z2 : Zone_Rules_Ptr) return Boolean is
   begin
      return Compare(Z1.Name, Z2.Name) >= 0;
   end ">=";

   function "<="(Z1, Z2 : Zone_Rules_Ptr) return Boolean is
   begin
      return Compare(Z1.Name, Z2.Name) <= 0;
   end "<=";

   package Rules_Container is new Asgc(Zone_Rules_Ptr, Eq);
   package Rules_Btree1 is new Rules_Container.Btree;
   package Rules_Btree is new Rules_Btree1.Dynamic;

   All_Rules : aliased Rules_Btree.Object(False, 10);

   function Allocate_Rule(Name      : in String;
                          Num_Rules : in Integer)
                          return Zone_Rules_Ptr is
      New_Rule : Zone_Rules_Ptr := new Zone_Rules(Name'Length,
                                                  Num_Rules);
   begin
      New_Rule.Name := Name;
      Rules_Btree.Add(All_Rules, New_Rule);
      return New_Rule;
   end Allocate_Rule;

   procedure Add_Rule(Rules   : in out Zone_Rules;
                      From    : in Integer;
                      To      : in Integer;
                      Save    : in Integer;
                      Mode    : in Zone_Mode_Class;
                      Letters : in String) is
      Info : Zone_Rule_Range_Info;
   begin
      Info.From := From;
      Info.To := To;
      Info.Save := Save;
      Info.Mode := Mode;
      if (Letters'Length > 0) then
         Info.Letters(1 .. Letters'Length) := Letters;
      end if;
      Info.Letters_Len := Letters'Length;
      Rules.Last_Rule := Rules.Last_Rule + 1;
      Rules.Rules(Rules.Last_Rule) := Info;
   end Add_Rule;

   function Find_Rule(Name : in String) return Zone_Rules_Ptr is
      It    : Rules_Btree.Iterator;
      Tmp   : aliased Zone_Rules(Name'Length, 0);
      Found : Boolean;
   begin
      Tmp.Name := Name;
      It := Rules_Btree.New_Iterator(All_Rules'Access);
      Rules_Btree.Search(It, Tmp'Unchecked_Access, Found);
      if (Found) then
         return Rules_Btree.Get(It);
      else
         return null;
      end if;
   end Find_Rule;

end Asl.Date_Time.Timezone.Complex;
