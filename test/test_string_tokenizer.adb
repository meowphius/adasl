
with Asl.Strings.Tokenizer; use Asl.Strings.Tokenizer;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_String_Tokenizer is
   Ts1  : Tokenized_String;
   Ts2  : Tokenized_String;
   S1   : String(1 .. 1);
   Test_Err : exception;
begin
   -- Initial tests, just get stuff and make sure all the errors happen
   -- correctly.
   Initialize(Ts1, "a bc def");

   begin
      S1 := Current_Token(Ts1);
      raise Test_Err;
   exception
      when Token_Not_Processed => null;
   end;

   Move_To_Next_Token(Ts1);

   if (Current_Token_Length(Ts1) /= 1) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "a") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts1);
   if (Current_Token_Length(Ts1) /= 2) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "bc") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts1);
   if (Current_Token_Length(Ts1) /= 3) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "def") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts1);

   if (Current_Token_Length(Ts1) /= 0) then
      raise Test_Err;
   end if;

   begin
      S1 := Current_Token(Ts1);
      raise Test_Err;
   exception
      when No_Tokens_Left => null;
   end;

   -- Test separators.
   Initialize(Ts1, "123 456:789&10");

   Move_To_Next_Token(Ts1);

   if (Current_Token_Length(Ts1) /= 3) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "123") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts1);
   Recalc_Current_Token_End(Ts1, ":&");
   if (Current_Token_Length(Ts1) /= 3) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "456") then
      raise Test_Err;
   end if;

   -- Make a copy to test Adjust().
   Ts2 := Ts1;

   Move_To_Next_Token(Ts1, "&:");
   if (Current_Token_Length(Ts1) /= 3) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "789") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts1, "&:");
   if (Current_Token_Length(Ts1) /= 2) then
      raise Test_Err;
   end if;
   if (Current_Token(Ts1) /= "10") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts2, "&:");
   Recalc_Current_Token_End(Ts2, "");

   -- Now test the copy.
   if (Current_Token_Length(Ts2) /= 6) then
      Put_Line("tok: '" & Current_Token(Ts2) & "'");
      raise Test_Err;
   end if;
   if (Current_Token(Ts2) /= "789&10") then
      raise Test_Err;
   end if;

   Move_To_Next_Token(Ts2, "&");
   if (Current_Token_Length(Ts2) /= 0) then
      raise Test_Err;
   end if;

   Put_Line("Tests passed");
end Test_String_Tokenizer;
