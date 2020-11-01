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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Asl.Strings.Tokenizer; use Asl.Strings.Tokenizer;
with Asl.Strings;
with Asl.Date_Time; use Asl.Date_Time;
with Conv_Zones_Data; use Conv_Zones_Data;
use Conv_Zones_Data.Zone_Rules_Container;
use Conv_Zones_Data.Zone_Rules_Btree;
use Conv_Zones_Data.Zone_Container;
use Conv_Zones_Data.Zone_Btree;
use Conv_Zones_Data.ZR_Alist;
use Conv_Zones_Data.ZE_Alist;

-- This procedure parses a standard Unix zone info file as its standard input
-- and generates Ada code that represents all the timezones on its standard
-- output.  It has two modes, simple (which only generates timezone information
-- for the current state of the timezone) and complex (which generates
-- information from the past history of the timezone, and is thus valid in
-- the past).

procedure Conv_Zones is
   Rules   : aliased Zone_Rules_Btree.Object(False, 10);
   Zones   : aliased Zone_Btree.Object(False, 10);
   File    : File_Access := Standard_Input;
   Str     : String(1 .. 256);
   Last    : Natural;
   Tokstr  : Tokenized_String;
   In_Zone : Boolean := False; -- Am I in the middle of processing a zone?
   Curr_Zone : Zone_Ptr;
   Line_Num : Positive := 1;
   Errors  : Natural := 0;

   function Cmp_Str(S1, S2 : in String) return Integer
     renames Asl.Strings.Compare;

   procedure Gen_Err(Err : in String) is
   begin
      Put_Line(Standard_Error.all, "Error on line" & Integer'Image(Line_Num)
               & ": " & Err);
      Errors := Errors + 1;
   end Gen_Err;

   procedure Add_Rule(Name    : in String;
                      From    : in Integer;
                      To      : in Integer;
                      Month   : in Month_Range;
                      Day     : in Zone_Day_Choice;
                      Start   : in Zone_HMS;
                      Save    : in Integer;
                      Letters : in String) is
      Rule_Info : Zone_Rule_Range_Info;
      Tmp       : aliased Zone_Rules(Name'Length);
      Rule      : Zone_Rules_Ptr;
      It        : Conv_Zones_Data.Zone_Rules_Btree.Iterator;
      Found     : Boolean;
   begin
      Rule_Info.From := From;
      Rule_Info.To := To;
      Rule_Info.Month := Month;
      Rule_Info.Day := Day;
      Rule_Info.Start := Start;
      Rule_Info.Save := Save;

      if (Letters'Length > Rule_Info.Letters'Length) then
         Gen_Err("letters is too long, max length is"
                 & Integer'Image(Rule_Info.Letters'Length));
         return;
      end if;
      if ((Letters'Length = 1) and then (Letters = "-")) then
         Rule_Info.Letters_Len := 0;
      else
         Rule_Info.Letters(1 .. Letters'Length) := Letters;
         Rule_Info.Letters_Len := Letters'Length;
      end if;

      Tmp.Name := Name;
      It := New_Iterator(Rules'Unchecked_Access);
      Search(It, Tmp'Unchecked_Access, Found);
      if (not Found) then
         Rule := new Zone_Rules(Name'Length);
         Rule.Name := Name;
         Add(Rules, Rule);
      else
         Rule := Get(It);
      end if;

      Enqueue(Rule.Rules, Rule_Info);
   end Add_Rule;

   function Get_Integer(Line : in Tokenized_String) return Integer is
      V : String := Current_Token(Line);
   begin
      return Integer'Value(V);
   end Get_Integer;

   function Get_Month(Str : in String) return Integer is
      Z : Zone_Months := Zone_Months'Value(Str);
   begin
      return Num_From_Zone_Month(Z);
   end Get_Month;

   function Get_Month(Line : in Tokenized_String) return Integer is
      V : String := Current_Token(Line);
   begin
      return Get_Month(V);
   end Get_Month;

   function Get_Weekday(Str : in String) return Integer is
      Z : Zone_Weekdays := Zone_Weekdays'Value(Str);
   begin
      return Num_From_Zone_Weekday(Z);
   end Get_Weekday;

   function Get_Weekday(Line : in Tokenized_String) return Integer is
      V : String := Current_Token(Line);
   begin
      return Get_Weekday(V);
   end Get_Weekday;

   function Get_Day_Choice(V : in String)
                           return Zone_Day_Choice is
      All_Nums  : Boolean := True;
      Has_Cmp   : Boolean := False;
      Cmp_Start : Integer;
      Cmp_End   : Integer;
      Day       : Zone_Day_Choice;
   begin
      if ((V'Length > 4) and then (V(V'First .. V'First + 3) = "last")) then
         Day.Day_Type := Last_Weekday;
         Day.Weekday := Get_Weekday(V(V'First + 4 .. V'Last));
         Day.Day_Of_Month := 1;
      else
         for I in V'Range loop
            if (Is_Decimal_Digit(V(I))) then
                null;
            elsif ((V(I) = '>') or (V(I) = '<') or (V(I) = '=')) then
               if (V(I) = '>') then
                  Day.Day_Type := Weekday_GE;
               elsif (V(I) = '<') then
                  Day.Day_Type := Weekday_LE;
               end if;

               All_Nums := False;
               if (Has_Cmp) then
                  Cmp_End := I;
               else
                  Has_Cmp := True;
                  Cmp_Start := I;
                  Cmp_End := I;
               end if;
            else
               All_Nums := False;
            end if;
         end loop;

         if (All_Nums) then
            Day.Day_Type := Day_Of_Month;
            Day.Weekday := 1;
            Day.Day_Of_Month := Integer'Value(V);
         elsif (Has_Cmp) then
            Day.Weekday := Get_Weekday(V(V'First .. Cmp_Start-1));
            Day.Day_Of_Month := Integer'Value(V(Cmp_End+1 .. V'Last));
         else
            raise Constraint_Error;
         end if;
      end if;

      return Day;
   end Get_Day_Choice;

   function Get_Day_Choice(Line : in Tokenized_String)
                           return Zone_Day_Choice is
      V : String := Current_Token(Line);
   begin
      return Get_Day_Choice(V);
   end Get_Day_Choice;

   procedure Get_HMS_Type(Str      : in String;
                          End_Str  : in out Integer;
                          HMS_Type : out Time_Types) is
   begin
      case Str(End_Str) is
         when 'w' | 'W' =>
            HMS_Type := Wall;
            End_Str := End_Str - 1;

         when 's' | 'S' =>
            HMS_Type := Asl.Date_Time.Standard;
            End_Str := End_Str - 1;

         when 'u' | 'U' | 'g' | 'G' | 'z' | 'Z' =>
            HMS_Type := Universal;
            End_Str := End_Str - 1;

         when others =>
            HMS_Type := Wall;
      end case;
   end Get_HMS_Type;

   function Get_HMS(Str : in String) return Zone_HMS is
      Start : Integer;
      Last  : Integer;
      S_End : Integer := Str'Last;
      Rv    : Zone_HMS;
   begin
      if (Str'Length = 0) then
         raise Constraint_Error;
      end if;

      Get_HMS_Type(Str, S_End, Rv.HMS_Type);

      Start := Str'First;

      Last := Start;
      while (Last <= S_End) loop
         if (Str(Last) = ':') then
            exit;
         end if;
         Last := Last + 1;
      end loop;
      Rv.Hour := Integer'Value(Str(Start .. Last-1));

      if (Last > S_End) then
         Rv.Minute := 0;
         Rv.Second := 0;
         return Rv;
      end if;

      Start := Last + 1;
      Last := Start;
      while (Last <= S_End) loop
         if (Str(Last) = ':') then
            exit;
         end if;
         Last := Last + 1;
      end loop;
      Rv.Minute := Integer'Value(Str(Start .. Last-1));

      if (Last > S_End) then
         Rv.Second := 0;
         return Rv;
      end if;

      Rv.Second := Integer'Value(Str(Last+1 .. S_End));

      return Rv;
   end Get_HMS;

   function Get_HMS(Line : in Tokenized_String) return Zone_HMS is
      V : String := Current_Token(Line);
   begin
      return Get_HMS(V);
   end Get_HMS;

   function Get_HMS_Seconds(Str : in String) return Integer is
      HMS   : Zone_HMS;
      Sign  : Integer := 1;
      Start : Integer := Str'First;
      Rv    : Integer;
   begin
      if (Str(Str'First) = '-') then
         Sign := -1;
         Start := Start + 1;
      end if;

      HMS := Get_HMS(Str(Start .. Str'Last));
      Rv := Sign * ((HMS.Hour * 3600) + (HMS.Minute * 60) + HMS.Second);
      return Rv;
   end Get_HMS_Seconds;

   function Get_HMS_Seconds(Line : in Tokenized_String) return Integer is
      V : String := Current_Token(Line);
   begin
      return Get_HMS_Seconds(V);
   end Get_HMS_Seconds;

   procedure Handle_Rule(Line : in out Tokenized_String) is
      Name  : String := Current_Token(Line);
      From  : Integer;
      To    : Integer;
      Day   : Zone_Day_Choice;
      Month : Month_Range;
      Start : Zone_HMS;
      Save  : Integer;
   begin
      Move_To_Next_Token(Line);
      declare
         To_Str : String := Current_Token(Line);
      begin
         if (Cmp_Str(To_Str, "min") = 0) then
            From := 0;
         else
            From := Get_Integer(Line);
         end if;
      end;

      Move_To_Next_Token(Line);
      declare
         To_Str : String := Current_Token(Line);
      begin
         if (Cmp_Str(To_Str, "only") = 0) then
            To := From;
         elsif (Cmp_Str(To_Str, "max") = 0) then
            To := Year_Range'Last;
         else
            To := Get_Integer(Line);
         end if;
      end;

      Move_To_Next_Token(Line);
      declare
         Zone_Type : String := Current_Token(Line);
      begin
         if (Cmp_Str(Zone_Type, "-") /= 0) then
            Gen_Err("Zone type was not null");
         end if;
      end;

      Move_To_Next_Token(Line);
      Month := Get_Month(Line);

      Move_To_Next_Token(Line);
      Day := Get_Day_Choice(Line);

      Move_To_Next_Token(Line);
      Start := Get_HMS(Line);

      Move_To_Next_Token(Line);
      Save := Get_HMS_Seconds(Line);

      Move_To_Next_Token(Line);
      declare
         Letters : String := Current_Token(Line);
      begin
         Add_Rule(Name,
                  From,
                  To,
                  Month,
                  Day,
                  Start,
                  Save,
                  Letters);
      end;
   exception
      when No_Tokens_Left =>
         Gen_Err("Not enough information");
      when Constraint_Error =>
         Gen_Err("Invalid data");
   end Handle_Rule;

   procedure Get_Zone_Date(Line : in out Tokenized_String;
                           Rv   : out Zone_Date) is
   begin
      Rv.Year := Get_Integer(Line);

      Move_To_Next_Token(Line);
      if (Current_Token_Length(Line) = 0) then
         Rv.Month := 12;
         Rv.Day.Day_Type := Day_Of_Month;
         Rv.Day.Day_Of_Month := 1;
         Rv.HMS.Hour := 0;
         Rv.HMS.Minute := 0;
         Rv.HMS.Second := 0;
         Rv.HMS.HMS_Type := Wall;
         return;
      end if;
      Rv.Month := Get_Month(Line);

      Move_To_Next_Token(Line);
      if (Current_Token_Length(Line) = 0) then
         Rv.Day.Day_Type := Day_Of_Month;
         Rv.Day.Day_Of_Month := 1;
         Rv.HMS.Hour := 0;
         Rv.HMS.Minute := 0;
         Rv.HMS.Second := 0;
         Rv.HMS.HMS_Type := Wall;
         return;
      end if;
      Rv.Day := Get_Day_Choice(Line);

      Move_To_Next_Token(Line);
      if (Current_Token_Length(Line) = 0) then
         Rv.HMS.Hour := 0;
         Rv.HMS.Minute := 0;
         Rv.HMS.Second := 0;
         Rv.HMS.HMS_Type := Wall;
         return;
      end if;

      Rv.HMS := Get_HMS(Line);
   end Get_Zone_Date;

   procedure Process_Another_Zone_Line(Line : in out Tokenized_String) is
      Ent : Zone_Entry;
   begin
      Ent.GMT_Offset := Get_HMS_Seconds(Line);

      Move_To_Next_Token(Line);
      declare
         Rule_Str : String := Current_Token(Line);
      begin
         if (Cmp_Str(Rule_Str, "-") = 0) then
            Ent.Kind := None;
         else
            begin
               Ent.Kind := Conv_Zones_Data.Offset;
               Ent.Offset := Get_HMS_Seconds(Rule_Str);
            exception
               when Constraint_Error =>
                  Ent.Kind := Conv_Zones_Data.Rule;
                  Ent.Rules := new String'(Rule_Str);
            end;
         end if;
      end;

      Move_To_Next_Token(Line);
      declare
         Format : String := Current_Token(Line);
      begin
         if (Format'Length > Ent.Format'Length) then
            Gen_Err("Format is too long, max is"
                    & Integer'Image(Ent.Format'Length)
                    & " characters");
         else
            Ent.Format(1 .. Format'Length) := Format;
            Ent.Format_Len := Format'Length;
         end if;
      end;

      Move_To_Next_Token(Line);
      Ent.Has_Until := Current_Token_Length(Line) /= 0;
      if (Ent.Has_Until) then
         Get_Zone_Date(Line, Ent.Ends_On);
         In_Zone := True;
      else
         In_Zone := False;
      end if;
      Add(Curr_Zone.Entries, Ent);
   exception
      when No_Tokens_Left =>
         Gen_Err("Not enough information");
      when Constraint_Error =>
         Gen_Err("Invalid data");
   end Process_Another_Zone_Line;

   procedure Handle_Zone(Line : in out Tokenized_String) is
      Name : String := Current_Token(Line);
   begin
      Curr_Zone := new Zone(Name'Length);
      Curr_Zone.Name := Name;
      Add(Zones, Curr_Zone);

      Move_To_Next_Token(Line);
      Process_Another_Zone_Line(Line);
   exception
      when No_Tokens_Left =>
         Gen_Err("Not enough information");
      when Constraint_Error =>
         Gen_Err("Invalid data");
   end Handle_Zone;

   procedure Handle_Link(Line : in out Tokenized_String) is
      Dest  : String := Current_Token(Line);
      Z     : Zone_Ptr;
      Link  : Zone_Link_Ptr;
      Elink : Zone_Link_Ptr;
      It    : Conv_Zones_Data.Zone_Btree.Iterator;
      Tmp   : aliased Conv_Zones_Data.Zone(Dest'Length);
      Found : Boolean;
   begin
      It := New_Iterator(Zones'Unchecked_Access);
      Tmp.Name := Dest;
      Search(It, Tmp'Unchecked_Access, Found);
      if (not Found) then
         Gen_Err("Link destination " & Dest & " not found");
         return;
      end if;
      Z := Get(It);

      while (Z.Is_Link) loop
         Z := Z.Real_Link;
      end loop;

      Move_To_Next_Token(Line);
      declare
         Name : String := Current_Token(Line);
         NZ   : Zone_Ptr;
      begin
         Link := new Zone_Link(Name'Length);
         Link.Name := Name;
         Link.Next := null;

         NZ := new Zone(Name'Length);
         NZ.Name := Name;
         NZ.Is_Link := True;
         NZ.Real_Link := Z;
         begin
            Add(Zones, NZ);
            exception
            when Conv_Zones_Data.Zone_Container.Item_Already_Exists =>
               Gen_Err("Duplicate zone name: " & Name);
         end;
      end;

      if (Z.Links = null) then
         Z.Links := Link;
      else
         Elink := Z.Links;
         while (Elink.Next /= null) loop
            Elink := Elink.Next;
         end loop;
         Elink.Next := Link;
      end if;
   exception
      when No_Tokens_Left =>
         Gen_Err("Not enough information");
      when Constraint_Error =>
         Gen_Err("Invalid data");
   end Handle_Link;

   procedure Process_Line(Line : in out Tokenized_String) is
      Tok : String := Current_Token(Line);
   begin
      Move_To_Next_Token(Line);
      if (Cmp_Str(Tok, "Rule") = 0) then
         Handle_Rule(Line);
      elsif (Cmp_Str(Tok, "Zone") = 0) then
         Handle_Zone(Line);
      elsif (Cmp_Str(Tok, "Link") = 0) then
         Handle_Link(Line);
      else
         Gen_Err("Invalid operation");
      end if;
   end Process_Line;

   procedure Process_Rules is
      It     : Conv_Zones_Data.Zone_Rules_Btree.Iterator;
      Is_End : Conv_Zones_Data.Zone_Rules_Container.End_Marker;
      Rule   : Zone_Rules_Ptr;
   begin
      It := New_Iterator(Rules'Unchecked_Access);
      First(It, Is_End);
      while (Is_End = Not_Past_End) loop
         Rule := Get(It);
         ZR_Sort.Sort(Rule.Rules'Access);
         Next(It, Is_End);
      end loop;
   end Process_Rules;

   procedure Process_Zones is
      It     : Conv_Zones_Data.Zone_Btree.Iterator;
      Is_End : Conv_Zones_Data.Zone_Container.End_Marker;
      Zone   : Zone_Ptr;
   begin
      It := New_Iterator(Zones'Unchecked_Access);
      First(It, Is_End);
      while (Is_End = Not_Past_End) loop
         Zone := Get(It);
         if (not Zone.Is_Link) then
            ZE_Sort.Sort(Zone.Entries'Access);
         end if;
         Next(It, Is_End);
      end loop;
   end Process_Zones;

   function Gen_Format(E      : in Zone_Entry;
                       R      : in Zone_Rule_Range_Info;
                       In_DST : in Boolean)
                       return String is
      Str  : String(1 .. E.Format_Len * (R.Letters_Len + 1));
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
            if (R.Letters_Len = 0) then
               Last := Last - 2;
               for J in I .. Last loop
                  Str(J) := Str(J+2);
               end loop;
            elsif (R.Letters_Len = 1) then
               Str(I) := R.Letters(1);
               I := I + 1;
               Last := Last - 1;
               for J in I .. Last loop
                  Str(J) := Str(J+1);
               end loop;
            elsif (R.Letters_Len = 2) then
               I := I + 2;
               Str(I .. I+1) := R.Letters(1 .. 2);
            else
               for J in reverse I+1 .. Last loop
                  Str(J+R.Letters_Len-2) := Str(J);
               end loop;
               Str(I .. I+R.Letters_Len-1) := R.Letters(1 .. R.Letters_Len);
               I := I + R.Letters_Len;
               Last := Last + R.Letters_Len - 2;
            end if;
         else
            I := I + 1;
         end if;
      end loop;

      return Str(1 .. Last);
   end Gen_Format;

   function Gen_Mode(Month : in Month_Range;
                     Day   : in Zone_Day_Choice;
                     HMS   : in Zone_HMS)
                     return String is
   begin
      case Day.Day_Type is
         when Day_Of_Month =>
            return ("new Day_Of_Month_Mode("
                    & Short_Month'Image(Month_Range_To_Short_Month(Month))
                    & ", " & Integer'Image(Day.Day_Of_Month)
                    & ", " & Integer'Image(HMS.Hour)
                    & ", " & Integer'Image(HMS.Minute)
                    & ", " & Integer'Image(HMS.Second)
                    & ", " & Time_Types'Image(HMS.HMS_Type)
                    & ")");

         when Last_Weekday =>
            return ("new Weekday_From_End_Month_Mode("
                    & Short_Month'Image(Month_Range_To_Short_Month(Month))
                    & ", "
                    & Short_Weekday'Image(Weekday_Range_To_Short_Weekday
                                          (Day.Weekday))
                    & ", 1"
                    & ", " & Integer'Image(HMS.Hour)
                    & ", " & Integer'Image(HMS.Minute)
                    & ", " & Integer'Image(HMS.Second)
                    & ", " & Time_Types'Image(HMS.HMS_Type)
                    & ")");

         when Weekday_GE =>
            return ("new Weekday_On_Or_After_Day_Of_Month_Mode("
                    & Short_Month'Image(Month_Range_To_Short_Month(Month))
                    & ", "
                    & Short_Weekday'Image(Weekday_Range_To_Short_Weekday
                                          (Day.Weekday))
                    & ", " & Integer'Image(Day.Day_Of_Month)
                    & ", " & Integer'Image(HMS.Hour)
                    & ", " & Integer'Image(HMS.Minute)
                    & ", " & Integer'Image(HMS.Second)
                    & ", " & Time_Types'Image(HMS.HMS_Type)
                    & ")");

         when Weekday_LE =>
            return ("new Weekday_On_Or_Before_Day_Of_Month_Mode("
                    & Short_Month'Image(Month_Range_To_Short_Month(Month))
                    & ", "
                    & Short_Weekday'Image(Weekday_Range_To_Short_Weekday
                                          (Day.Weekday))
                    & ", " & Integer'Image(Day.Day_Of_Month)
                    & ", " & Integer'Image(HMS.Hour)
                    & ", " & Integer'Image(HMS.Minute)
                    & ", " & Integer'Image(HMS.Second)
                    & ", " & Time_Types'Image(HMS.HMS_Type)
                    & ")");
      end case;
   end Gen_Mode;

   procedure Gen_License is
   begin
      Put_Line("-- The Ada Structured Library - A set of container classes and general");
      Put_Line("--   tools for use with Ada95.");
      Put_Line("-- Copyright (C) 2001  Corey Minyard (minyard@acm.org)");
      Put_Line("--");
      Put_Line("-- This library is free software; you can redistribute it and/or modify it");
      Put_Line("-- under the terms of the GNU General Public License as published by the");
      Put_Line("-- Free Software Foundation; either version 2 of the License, or (at your");
      Put_Line("-- option) any later version.");
      Put_Line("--");
      Put_Line("-- This library is distributed in the hope that it will be useful, but");
      Put_Line("-- WITHOUT ANY WARRANTY; without even the implied warranty of");
      Put_Line("-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU");
      Put_Line("-- General Public License for more details.");
      Put_Line("--");
      Put_Line("-- You should have received a copy of the GNU General Public License along");
      Put_Line("-- with this library; if not, write to the Free Software Foundation, Inc.,");
      Put_Line("-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA");
      Put_Line("--");
      Put_Line("-- As a special exception, if other files instantiate generics from this");
      Put_Line("-- unit, or you link this unit with other files to produce an executable,");
      Put_Line("-- this unit does not by itself cause the resulting executable to be");
      Put_Line("-- covered by the GNU General Public License.  This exception does not");
      Put_Line("-- however invalidate any other reasons why the executable file might be");
      Put_Line("-- covered by the GNU Public License.");
      Put_Line("--");
      Put_Line("");
   end Gen_License;

   function Find_Rules(Name : in String) return Zone_Rules_Ptr is
      Tmp      : aliased Zone_Rules(Name'Length);
      It       : Conv_Zones_Data.Zone_Rules_Btree.Iterator;
      Found    : Boolean;
   begin
      It := New_Iterator(Rules'Unchecked_Access);
      Tmp.Name := Name;
      Search(It, Tmp'Unchecked_Access, Found);
      if (not Found) then
         return null;
      else
         return Get(It);
      end if;
   end Find_Rules;

   procedure Gen_Simple is
      It     : Conv_Zones_Data.Zone_Btree.Iterator;
      Is_End : Conv_Zones_Data.Zone_Container.End_Marker;
      E      : Zone_Entry;
      Zone   : Zone_Ptr;
      Srule  : Zone_Rule_Range_Info;
      Erule  : Zone_Rule_Range_Info;
      Links  : Zone_Link_Ptr;
      Rules  : Zone_Rules_Ptr;
   begin
      Put_Line("with Asl.Date_Time.Timezone; use Asl.Date_Time.Timezone;");
      Put_Line("with Asl.Date_Time.Timezone.Simple; use Asl.Date_Time.Timezone.Simple;");
      Put_Line("");
      Put_Line("procedure Asl.Date_Time.Register_Simple_Timezones"&
               "(F : in Zone_Factory_Ptr) is");
      Put_Line("   Zone : Time_Zone_Class;");
      Put_Line("");
      Put_Line("   procedure Register_Duplicate(Name   : in String;");
      Put_Line("                                Source : in Time_Zone'Class) is");
      Put_Line("      Zone : Time_Zone_Class;");
      Put_Line("   begin");
      Put_Line("      Zone := Duplicate_Zone(Name, Source);");
      Put_Line("      Register_Timezone(F.all, Zone);");
      Put_Line("   end Register_Duplicate;");
      Put_Line("begin");

      It := New_Iterator(Zones'Unchecked_Access);
      First(It, Is_End);
      while (Is_End = Not_Past_End) loop
         Zone := Get(It);
         if (not Zone.Is_Link) then
            E := Get_At(Zone.Entries, Member_Count(Zone.Entries));
            case E.Kind is
               when None =>
                  Put_Line("   Zone := Allocate_No_DST_Zone(""" & Zone.Name
                           & """, """ & E.Format(1 .. E.Format_Len)
                           & """, " & Integer'Image(E.GMT_Offset) & ");");
               when Offset =>
                  Put_Line("   Zone := Allocate_No_DST_Zone(""" & Zone.Name
                           & """, """ & E.Format(1 .. E.Format_Len)
                           & """, " & Integer'Image(E.GMT_Offset + E.Offset)
                           & ");");
               when Rule =>
                  Rules := Find_Rules(E.Rules.all);
                  Srule := Get_At(Rules.Rules, Member_Count(Rules.Rules));
                  if (Srule.Save = 0) then
                     Erule := Srule;
                     for I in reverse 1 .. Member_Count(Rules.Rules)-1 loop
                        Srule := Get_At(Rules.Rules, I);
                        exit when (Srule.Save /= 0);
                     end loop;
                     if (Srule.Save = 0) then
                        Gen_Err("Start rule not found for entry "
                                & Rules.Name);
                     end if;
                  else
                     for I in reverse 1 .. Member_Count(Rules.Rules)-1 loop
                        Erule := Get_At(Rules.Rules, I);
                        exit when (Erule.Save = 0);
                     end loop;
                     if (Erule.Save /= 0) then
                        Gen_Err("End rule not found for entry "
                                & Rules.Name);
                     end if;
                  end if;

                  Put_Line("   Zone := Allocate_Simple_Zone(""" & Zone.Name
                           & """, """ & Gen_Format(E, Srule, True)
                           & """, """ & Gen_Format(E, Erule, False)
                           & """, " & Integer'Image(E.GMT_Offset)
                           & ",");
                  Put_Line("                                "
                           & Gen_Mode(Srule.Month, Srule.Day, Srule.Start)
                           & ",");
                  Put_Line("                                "
                           & Gen_Mode(Erule.Month, Erule.Day, Erule.Start)
                           & ");");
            end case;
            Put_Line("   Register_Timezone(F.all, Zone);");
            Links := Zone.Links;
            while (Links /= null) loop
               Put_Line("   Register_Duplicate("""
                        & Links.Name & """, Zone.all);");
               Links := Links.Next;
            end loop;
         end if;
         Next(It, Is_End);
      end loop;
      Put_Line("end Asl.Date_Time.Register_Simple_Timezones;");
   end Gen_Simple;

   procedure Gen_Complex_Rules is
      It     : Conv_Zones_Data.Zone_Rules_Btree.Iterator;
      Is_End : Conv_Zones_Data.Zone_Rules_Container.End_Marker;
      Rule   : Zone_Rules_Ptr;
      Info   : Zone_Rule_Range_Info;
   begin
      It := New_Iterator(Rules'Unchecked_Access);
      First(It, Is_End);
      while (Is_End = Not_Past_End) loop
         Rule := Get(It);
         Put_Line("   Rules := Allocate_Rule(""" & Rule.Name
                  & """, " & Integer'Image(Member_Count(Rule.Rules))
                  & ");");
         for I in 1 .. Member_Count(Rule.Rules) loop
            Info := Get_At(Rule.Rules, I);
            Put("   Add_Rule(Rules.all, "
                & Integer'Image(Info.From) & ", ");
            if (Info.To = Year_Range'Last) then
               Put("Year_Range'Last");
            else
               Put(Integer'Image(Info.To));
            end if;
            Put_Line(", " & Integer'Image(Info.Save) & ",");
            Put_Line("                   "
                     & Gen_Mode(Info.Month, Info.Day, Info.Start) & ",");
            Put_Line("                   """
                     & Info.Letters(1 .. Info.Letters_Len)
                     & """);");
         end loop;
         Next(It, Is_End);
      end loop;
   end Gen_Complex_Rules;

   procedure Gen_Complex is
      It     : Conv_Zones_Data.Zone_Btree.Iterator;
      Is_End : Conv_Zones_Data.Zone_Container.End_Marker;
      E      : Zone_Entry;
      Zone   : Zone_Ptr;
      Links  : Zone_Link_Ptr;
      Rules  : Zone_Rules_Ptr;
   begin
      Put_Line("with Asl.Date_Time.Timezone; use Asl.Date_Time.Timezone;");
      Put_Line("with Asl.Date_Time.Timezone.Complex; use Asl.Date_Time.Timezone.Complex;");
      Put_Line("");
      Put_Line("procedure Asl.Date_Time.Register_Complex_Timezones"&
               "(F : in Zone_Factory_Ptr) is");
      Put_Line("   Zone  : Complex_Zone_Ptr;");
      Put_Line("   Rules : Zone_Rules_Ptr;");
      Put_Line("");
      Put_Line("   procedure Register_Duplicate(Name   : in String;");
      Put_Line("                                Source : in Complex_Zone) is");
      Put_Line("      Zone : Time_Zone_Class;");
      Put_Line("   begin");
      Put_Line("      Zone := Duplicate_Zone(Name, Source);");
      Put_Line("      Register_Timezone(F.all, Zone);");
      Put_Line("   end Register_Duplicate;");
      Put_Line("begin");

      Gen_Complex_Rules;

      It := New_Iterator(Zones'Unchecked_Access);
      First(It, Is_End);
      << Gen_Complex_Loop >>
      while (Is_End = Not_Past_End) loop
         Zone := Get(It);
         if (Zone.Is_Link) then
            Next(It, Is_End);
            goto Gen_Complex_Loop;
         end if;
         Put_Line("   Zone := Allocate_Complex_Zone(""" & Zone.Name
                  & """, " & Integer'Image(Member_Count(Zone.Entries))
                  & ");");
         for I in 1 .. Member_Count(Zone.Entries) loop
            E := Get_At(Zone.Entries, I);
            case E.Kind is
               when None =>
                  Put("   Add_Entry(Zone.all, " & Integer'Image(E.GMT_Offset)
                      & ", """ & E.Format(1 .. E.Format_Len)
                      & """ , ");
               when Offset =>
                  Put("   Add_Entry(Zone.all, " & Integer'Image(E.GMT_Offset)
                      & ", """ & E.Format(1 .. E.Format_Len)
                      & """ , " & Integer'Image(E.Offset)
                      & ", ");
               when Rule =>
                  Rules := Find_Rules(E.Rules.all);
                  if (Rules = null) then
                     Gen_Err("Rule " & E.Rules.all & " not found in for zone "
                             & Zone.Name);
                     Next(It, Is_End);
                     goto Gen_Complex_Loop;
                  end if;
                  Put("   Add_Entry(Zone.all, " & Integer'Image(E.GMT_Offset)
                      & ", """ & E.Format(1 .. E.Format_Len)
                      & """ , """ & E.Rules.all
                      & """, ");
            end case;
            if (E.Has_Until) then
               Put_Line("True, " & Integer'Image(E.Ends_On.Year) & ", ");
               Put_Line("             "
                        & Gen_Mode(E.Ends_On.Month,
                                   E.Ends_On.Day,
                                   E.Ends_On.HMS)
                        & ");");
            else
               Put_Line("False, 0, null);");
            end if;
         end loop;
         Put_Line("   Register_Timezone(F.all, Time_Zone_Class(Zone));");
         Links := Zone.Links;
            while (Links /= null) loop
               Put_Line("   Register_Duplicate("""
                        & Links.Name & """, Zone.all);");
               Links := Links.Next;
            end loop;
            Next(It, Is_End);
      end loop;
      Put_Line("end Asl.Date_Time.Register_Complex_Timezones;");
   end Gen_Complex;

   Do_Simple : Boolean := True;
begin
   if (Ada.Command_Line.Argument_Count /= 0) then
      if (Cmp_Str(Ada.Command_Line.Argument(1), "simple") = 0) then
         null;
      elsif (Cmp_Str(Ada.Command_Line.Argument(1), "complex") = 0) then
         Do_Simple := False;
      else
         Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
         Put_Line("Arguments are: simple - gen simple timezones");
         Put_Line("               complex - gen complex timezones");
         return;
      end if;
   end if;

   while (not End_Of_File(File.all)) loop
      Get_Line(File.all, Str, Last);
      for I in Str'First .. Last loop
         if (Str(I) = '#') then
            Last := I-1;
            exit;
         end if;
      end loop;
      if (Last /= 0) then
         Initialize(Tokstr, Str(1 .. Last));
         Move_To_Next_Token(Tokstr);
         if (Current_Token_Length(Tokstr) /= 0) then
            if (In_Zone) then
               Process_Another_Zone_Line(Tokstr);
            else
               Process_Line(Tokstr);
            end if;
         end if;
      end if;
      Line_Num := Line_Num + 1;
   end loop;

   if (In_Zone) then
      Gen_Err("Input ended in the middle of a zone");
   end if;

   if (Errors /= 0) then
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end if;

   Process_Rules;

   if (Errors /= 0) then
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end if;

   Process_Zones;

   if (Errors /= 0) then
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end if;

   if (Do_Simple) then
      Gen_Simple;
   else
      Gen_Complex;
   end if;

   if (Errors /= 0) then
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end if;

end Conv_Zones;
