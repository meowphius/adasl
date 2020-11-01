-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
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

with Unchecked_Deallocation;

package body Asl.Strings.Tokenizer is

   procedure Free is new Unchecked_Deallocation(String, String_Ptr);

   function Is_Sep(C   : in Character;
                   Sep : in String) return Boolean is
   begin
      for I in Sep'Range loop
         if (C = Sep(I)) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Sep;

   procedure Skip_Seps(T   : in out Tokenized_String;
                       Sep : in String) is
   begin
      if (T.Curr_Pos > T.Str'Length) then
         return;
      end if;

      while Is_Sep(T.Str(T.Curr_Pos), Sep) loop
         T.Curr_Pos := T.Curr_Pos + 1;
         exit when T.Curr_Pos > T.Str'Length;
      end loop;
   end Skip_Seps;

   procedure Initialize(T   : in out Tokenized_String;
                        Str : in String;
                        Sep : in String := (" "
                                            & Ada.Characters.Latin_1.HT
                                            & Ada.Characters.Latin_1.LF
                                            & Ada.Characters.Latin_1.CR)) is
   begin
      if (T.Str /= null) then
         Free(T.Str);
      end if;
      if (T.Sep /= null) then
         Free(T.Sep);
      end if;

      T.Str := new String'(Str);
      T.Curr_Pos := T.Str'First;
      T.Token_End_Valid := False;
      T.Sep := new String'(Sep);

      Skip_Seps(T, T.Sep.all);
   end Initialize;

   procedure Find_Token_End(T         : in Tokenized_String;
                            Sep       : in String;
                            Token_End : out Integer) is
   begin
      for I in T.Curr_Pos .. T.Str'Last loop
         if Is_Sep(T.Str(I), Sep) then
            Token_End := I - 1;
            return;
         end if;
      end loop;

      Token_End := T.Str'Last;
   end Find_Token_End;

   function Current_Token_Length(T : in Tokenized_String) return Integer is
   begin
      if not T.Token_End_Valid then
         raise Token_Not_Processed;
      end if;

      return T.Token_End - T.Curr_Pos + 1;
   end Current_Token_Length;

   procedure Move_To_Next_Token(T   : in out Tokenized_String;
                                Sep : in String) is
   begin
      if T.Token_End_Valid then
         T.Curr_Pos := T.Token_End + 1;
         Skip_Seps(T, Sep);
      end if;
      Find_Token_End(T, Sep, T.Token_End);
      T.Token_End_Valid := True;
   end Move_To_Next_Token;

   procedure Move_To_Next_Token(T : in out Tokenized_String) is
   begin
      Move_To_Next_Token(T, T.Sep.all);
   end Move_To_Next_Token;

   procedure Recalc_Current_Token_End(T   : in out Tokenized_String;
                                      Sep : in String) is
   begin
      Find_Token_End(T, Sep, T.Token_End);
      T.Token_End_Valid := True;
   end Recalc_Current_Token_End;

   procedure Recalc_Current_Token_End(T : in out Tokenized_String) is
   begin
      Recalc_Current_Token_End(T, T.Sep.all);
   end Recalc_Current_Token_End;

   function Current_Token(T : in Tokenized_String) return String is
   begin
      if not T.Token_End_Valid then
         raise Token_Not_Processed;
      end if;
      if (T.Curr_Pos > T.Token_End) then
         raise No_Tokens_Left;
      end if;
      return T.Str(T.Curr_Pos .. T.Token_End);
   end Current_Token;

   function Get_Whole_String(T : in Tokenized_String) return String is
   begin
      return T.Str.all;
   end Get_Whole_String;

   procedure Adjust(T : in out Tokenized_String) is
   begin
      T.Str := new String'(T.Str.all);
      T.Sep := new String'(T.Sep.all);
   end Adjust;

   procedure Finalize(T : in out Tokenized_String) is
   begin
      Free(T.Str);
      Free(T.Sep);
   end Finalize;

end Asl.Strings.Tokenizer;
