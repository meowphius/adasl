
with Asl.Strings;

package body Conv_Zones_Data is

   function "="(R1, R2 : in Zone_Rule_Range_Info) return Boolean is
   begin
      return (R1.To = R2.To);
   end "=";

   function ">"(R1, R2 : in Zone_Rule_Range_Info) return Boolean is
   begin
      return (R1.To > R2.To);
   end ">";

   function "<"(R1, R2 : in Zone_Rule_Range_Info) return Boolean is
   begin
      return (R1.To < R2.To);
   end "<";

   function ">="(R1, R2 : in Zone_Rule_Range_Info) return Boolean is
   begin
      return (R1.To >= R2.To);
   end ">=";

   function "<="(R1, R2 : in Zone_Rule_Range_Info) return Boolean is
   begin
      return (R1.To <= R2.To);
   end "<=";

   function "="(D1, D2 : in Zone_Date) return Boolean is
   begin
      return ((D1.Year = D2.Year)
              and (D1.Month = D2.Month)
              and (D1.Day.Day_Of_Month = D2.Day.Day_Of_Month)
              and (D1.HMS.Hour = D2.HMS.Hour)
              and (D1.HMS.Minute = D2.HMS.Minute)
              and (D1.HMS.Second = D2.HMS.Second));
   end "=";

   function ">"(D1, D2 : in Zone_Date) return Boolean is
   begin
      if (D1.Year /= D2.Year) then
         return D1.Year > D2.Year;
      end if;
      if (D1.Month /= D2.Month) then
         return D1.Month > D2.Month;
      end if;
      if (D1.Day.Day_Of_Month /= D2.Day.Day_Of_Month) then
         return D1.Day.Day_Of_Month > D2.Day.Day_Of_Month;
      end if;
      if (D1.HMS.Hour /= D2.HMS.Hour) then
         return D1.HMS.Hour > D2.HMS.Hour;
      end if;
      if (D1.HMS.Minute /= D2.HMS.Minute) then
         return D1.HMS.Minute > D2.HMS.Minute;
      end if;
      return D1.HMS.Second > D2.HMS.Second;
   end ">";

   function "<"(D1, D2 : in Zone_Date) return Boolean is
   begin
      if (D1.Year /= D2.Year) then
         return D1.Year < D2.Year;
      end if;
      if (D1.Month /= D2.Month) then
         return D1.Month < D2.Month;
      end if;
      if (D1.Day.Day_Of_Month /= D2.Day.Day_Of_Month) then
         return D1.Day.Day_Of_Month < D2.Day.Day_Of_Month;
      end if;
      if (D1.HMS.Hour /= D2.HMS.Hour) then
         return D1.HMS.Hour < D2.HMS.Hour;
      end if;
      if (D1.HMS.Minute /= D2.HMS.Minute) then
         return D1.HMS.Minute < D2.HMS.Minute;
      end if;
      return D1.HMS.Second < D2.HMS.Second;
   end "<";

   function ">="(D1, D2 : in Zone_Date) return Boolean is
   begin
      if (D1.Year /= D2.Year) then
         return D1.Year > D2.Year;
      end if;
      if (D1.Month /= D2.Month) then
         return D1.Month > D2.Month;
      end if;
      if (D1.Day.Day_Of_Month /= D2.Day.Day_Of_Month) then
         return D1.Day.Day_Of_Month > D2.Day.Day_Of_Month;
      end if;
      if (D1.HMS.Hour /= D2.HMS.Hour) then
         return D1.HMS.Hour > D2.HMS.Hour;
      end if;
      if (D1.HMS.Minute /= D2.HMS.Minute) then
         return D1.HMS.Minute > D2.HMS.Minute;
      end if;
      return D1.HMS.Second >= D2.HMS.Second;
   end ">=";

   function "<="(D1, D2 : in Zone_Date) return Boolean is
   begin
      if (D1.Year /= D2.Year) then
         return D1.Year < D2.Year;
      end if;
      if (D1.Month /= D2.Month) then
         return D1.Month < D2.Month;
      end if;
      if (D1.Day.Day_Of_Month /= D2.Day.Day_Of_Month) then
         return D1.Day.Day_Of_Month < D2.Day.Day_Of_Month;
      end if;
      if (D1.HMS.Hour /= D2.HMS.Hour) then
         return D1.HMS.Hour < D2.HMS.Hour;
      end if;
      if (D1.HMS.Minute /= D2.HMS.Minute) then
         return D1.HMS.Minute < D2.HMS.Minute;
      end if;
      return D1.HMS.Second <= D2.HMS.Second;
   end "<=";

   function "="(D1, D2 : in Zone_Entry) return Boolean is
   begin
      if (D1.Has_Until and D2.Has_Until) then
         return D1.Ends_On = D2.Ends_On;
      else
         return D1.Has_Until = D2.Has_Until;
      end if;
   end "=";

   function ">"(D1, D2 : in Zone_Entry) return Boolean is
   begin
      if (D1.Has_Until and D2.Has_Until) then
         return D1.Ends_On > D2.Ends_On;
      elsif ((not D1.Has_Until) and (not D2.Has_Until)) then
         return False;
      elsif D1.Has_Until then
         return False;
      else
         return True;
      end if;
   end ">";

   function "<"(D1, D2 : in Zone_Entry) return Boolean is
   begin
      if (D1.Has_Until and D2.Has_Until) then
         return D1.Ends_On < D2.Ends_On;
      elsif ((not D1.Has_Until) and (not D2.Has_Until)) then
         return False;
      elsif D1.Has_Until then
         return True;
      else
         return False;
      end if;
   end "<";

   function ">="(D1, D2 : in Zone_Entry) return Boolean is
   begin
      if (D1.Has_Until and D2.Has_Until) then
         return D1.Ends_On >= D2.Ends_On;
      elsif ((not D1.Has_Until) and (not D2.Has_Until)) then
         return True;
      elsif D1.Has_Until then
         return False;
      else
         return True;
      end if;
   end ">=";

   function "<="(D1, D2 : in Zone_Entry) return Boolean is
   begin
      if (D1.Has_Until and D2.Has_Until) then
         return D1.Ends_On <= D2.Ends_On;
      elsif ((not D1.Has_Until) and (not D2.Has_Until)) then
         return True;
      elsif D1.Has_Until then
         return True;
      else
         return False;
      end if;
   end "<=";

   function Eq(D1, D2 : in Zone_Rules_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) = 0;
   end Eq;

   function ">"(D1, D2 : in Zone_Rules_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) > 0;
   end ">";

   function "<"(D1, D2 : in Zone_Rules_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) < 0;
   end "<";

   function ">="(D1, D2 : in Zone_Rules_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) >= 0;
   end ">=";

   function "<="(D1, D2 : in Zone_Rules_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) <= 0;
   end "<=";

   function "="(D1, D2 : in Zone_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) = 0;
   end "=";

   function ">"(D1, D2 : in Zone_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) > 0;
   end ">";

   function "<"(D1, D2 : in Zone_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) < 0;
   end "<";

   function ">="(D1, D2 : in Zone_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) >= 0;
   end ">=";

   function "<="(D1, D2 : in Zone_Ptr) return Boolean is
   begin
      return Asl.Strings.Compare(D1.Name, D2.Name) <= 0;
   end "<=";

   function Num_From_Zone_Weekday(Z : in Zone_Weekdays) return Weekday_Range is
      Rv : Integer := Zone_Weekdays'Pos(Z) + 1;
   begin
      if (Rv > 7) then
         Rv := Rv - 7;
      end if;

      return Rv;
   end Num_From_Zone_Weekday;

   function Num_From_Zone_Month(Z : in Zone_Months) return Month_Range is
      Rv : Integer := Zone_Months'Pos(Z) + 1;
   begin
      if (Rv > 12) then
         Rv := Rv - 12;
      end if;

      return Rv;
   end Num_From_Zone_Month;

end Conv_Zones_Data;
