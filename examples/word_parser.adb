
with Ada.Characters.Handling;

package body Word_Parser is

   Big_A_Pos   : Integer := Character'Pos('A');
   Small_A_Pos : Integer := Character'Pos('a');

   procedure Get_Next_Word
     (File       : in File_Type;
      Word       : out Ada.Strings.Unbounded.Unbounded_String;
      Word_Found : out Boolean;
      File_Done  : out Boolean) is

      Tmp_Str    : String(1 .. 10);
      Word_Pos   : Positive := Tmp_Str'First;
      Input_Char : Character;
      In_Word    : Boolean := False;
   begin
      -- Start with an empty word.
      Word := Ada.Strings.Unbounded.To_Unbounded_String("");

      File_Done := False;
      Word_Found := False;

      if (End_Of_File(File)) then
         Word_Found := False;
         File_Done := True;
      else
         loop
            Get(File, Input_Char);
            Input_Char := Ada.Characters.Handling.To_Lower(Input_Char);

            if (not In_Word) then
               if (Input_Char in 'a' .. 'z') then
                  In_Word := True;
                  Word_Found := True;
                  Tmp_Str(Word_Pos) := Input_Char;
                  Word_Pos := Word_Pos + 1;
               end if;
            elsif (Input_Char in 'a' .. 'z') then
               Tmp_Str(Word_Pos) := Input_Char;
               if (Word_Pos = Tmp_Str'Last) then
                  Word := Word & Tmp_Str;
                  Word_Pos := Tmp_Str'First;
               else
                  Word_Pos := Word_Pos + 1;
               end if;
            else
               exit;
            end if;

            if (End_Of_File(File)) then
               File_Done := True;
               exit;
            elsif (End_Of_Line(File) and In_Word) then
               exit;
            end if;
         end loop;

         if (Word_Pos /= Tmp_Str'First) then
            -- If we have some stuff left in the temporary string, put it into
            -- the word.
            Word := Word & Tmp_Str(Tmp_Str'First .. Word_Pos - 1);
         end if;
      end if;
   end Get_Next_Word;

end Word_Parser;
