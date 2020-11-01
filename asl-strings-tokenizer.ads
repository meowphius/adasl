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
--

-- This package provides a string tokenizer that can be used to take a
-- string and separate it out using character separators.
with Ada.Characters.Latin_1;
with Ada.Finalization; use Ada.Finalization;

package Asl.Strings.Tokenizer is

   -- Raised when a token retrieval is attempted but no more tokens
   -- are left.
   No_Tokens_Left : exception;

   -- Raised by Current_Token if the token is not processed.
   Token_Not_Processed : exception;

   -- A tokenized string (duh).  The Length specifies the maximum length
   -- of the input string, the Num_Seps specifies the maximum number of
   -- separators allowed to the Initialize call (but not to the next
   -- token calls).
   type Tokenized_String is new Ada.Finalization.Controlled with private;

   -- Initialize or re-initialize the string tokenizer with the given
   -- string.  The Str value must be less than or equal to the length
   -- specified when the string tokenizer was created.  If Sep is specified,
   -- it sets the separater characters (the ones that go between the tokens),
   -- otherwise the default ones are set.  This call will skip any separators
   -- at the beginning of the string.
   procedure Initialize(T   : in out Tokenized_String;
                        Str : in String;
                        Sep : in String := (" "
                                            & Ada.Characters.Latin_1.HT
                                            & Ada.Characters.Latin_1.LF
                                            & Ada.Characters.Latin_1.CR));

   -- In the following calls, a Sep parameter may be specified for
   -- the separator.  This defines the beginning of the next token
   -- and the end of the next token.  If you want a token to have
   -- a different beginning and end separator, call Move_To_Next_Token()
   -- with the initial separator and Recalc_Current_Token with the
   -- end separator.


   -- Figure out where the next token is.  This must be called before
   -- Current_Token is called.
   procedure Move_To_Next_Token(T   : in out Tokenized_String;
                                Sep : in String);
   procedure Move_To_Next_Token(T   : in out Tokenized_String);

   -- Recalculate the current token with a new separator.  This separator
   -- will only be used to find the end of the token, the beginning of the
   -- token is already established by Move_To_Next_Token.
   procedure Recalc_Current_Token_End(T   : in out Tokenized_String;
                                      Sep : in String);
   procedure Recalc_Current_Token_End(T   : in out Tokenized_String);

   -- Return the current token.  Move_To_Next_Token must be called before
   -- this.  This will return the same value each time if Move_To_Next_Token
   -- is not called between.  This will raise Token_Not_Processed if
   -- called before Move_To_Next_Token is called.
   function Current_Token(T : in Tokenized_String) return String;

   -- Return the length of the current token.  It returns zero
   -- if no tokens are left.
   function Current_Token_Length(T   : in Tokenized_String) return Integer;

   -- Return the entire string passed in to the tokenizer.
   function Get_Whole_String(T : in Tokenized_String) return String;

   -- From the controlled type
   procedure Adjust(T : in out Tokenized_String);
   procedure Finalize(T : in out Tokenized_String);

private

   type String_Ptr is access String;

   type Tokenized_String is new Ada.Finalization.Controlled with record
      Str       : String_Ptr := null;
      Curr_Pos  : Integer;

      Token_End : Integer;
      Token_End_Valid : Boolean := False;

      Sep : String_Ptr := null;
   end record;

end Asl.Strings.Tokenizer;
