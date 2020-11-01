
package body Asu_Components is

   function "=" (V1, V2 : in Word_Counter_Ptr) return Boolean is
   begin
      return V1.Str = V2.Str;
   end;

   function ">" (V1, V2 : in Word_Counter_Ptr) return Boolean is
   begin
      return V1.Str > V2.Str;
   end;

   function "<" (V1, V2 : in Word_Counter_Ptr) return Boolean is
   begin
      return V1.Str < V2.Str;
   end;

   function ">=" (V1, V2 : in Word_Counter_Ptr) return Boolean is
   begin
      return V1.Str >= V2.Str;
   end;

   function "<=" (V1, V2 : in Word_Counter_Ptr) return Boolean is
   begin
      return V1.Str <= V2.Str;
   end;

   function Count_Eq (V1, V2 : in Word_Counter) return Boolean is
   begin
      return (V1.Count = V2.Count) and (V1.Str = V2.Str);
   end Count_Eq;

   function Count_Gt (V1, V2 : in Word_Counter) return Boolean is
   begin
      if (V1.Count = V2.Count) then
         return V1.Str > V2.Str;
      else
         return V1.Count > V2.Count;
      end if;
   end Count_Gt;

   function Count_Lt (V1, V2 : in Word_Counter) return Boolean is
   begin
      if (V1.Count = V2.Count) then
         return V1.Str < V2.Str;
      else
         return V1.Count < V2.Count;
      end if;
   end Count_Lt;

   function Count_Ge (V1, V2 : in Word_Counter) return Boolean is
   begin
      if (V1.Count = V2.Count) then
         return V1.Str >= V2.Str;
      else
         return V1.Count > V2.Count;
      end if;
   end Count_Ge;

   function Count_Le (V1, V2 : in Word_Counter) return Boolean is
   begin
      if (V1.Count = V2.Count) then
         return V1.Str <= V2.Str;
      else
         return V1.Count < V2.Count;
      end if;
   end Count_Le;

end Asu_Components;
