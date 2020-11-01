-- The Ada telnet processor - A set of packages for dealing with telnet
--   processing under Ada95
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

------------------------------------------------------------------------
-- This package provides an abstract file, that works much like a normal
-- file, except that it's a tagged type and it's abstract.  Real I/O
-- streams can extend from this; this package (and the sub-packages) do
-- the dirty work of special I/O.
--
-- Note that this works much like Ada.Text_IO, but there are subtle
-- differences.  The end-of-line concept works as a character, and
-- it has no concept of a page.  It's primarily designed for interactive
-- and stream I/O, not for record-oriented file I/O.

package Asl.Abstract_IO is

   -- An abstract file.
   type Abstract_File is abstract tagged limited private;
   type Abstract_File_Class is access all Abstract_File'Class;

   -- Same definitions as GNAT in Ada.Text_IO.
   subtype Field       is Integer range 0 .. 255;
   subtype Number_Base is Integer range 2 .. 16;
   type Type_Set is (Lower_Case, Upper_Case);

   -- Put a single character to the output.
   procedure Put(File : in out Abstract_File;
                 Item : in Character)
      is abstract;

   -- Put a string to the output.
   procedure Put(File : in out Abstract_File;
                 Item : in String)
      is abstract;

   -- Put a string followed by a newline to the output.
   procedure Put_Line(File : in out Abstract_File;
                      Item : in String)
      is abstract;

   -- Move to a new line.
   procedure New_Line(File : in out Abstract_File)
      is abstract;

   -- Get a character from the input.
   procedure Get(File : in out Abstract_File;
                 Item : out Character)
      is abstract;

   -- Get a string from the input.  Note that the entire string will
   -- be read in and the call will block until this happens.
   procedure Get(File : in out Abstract_File;
                 Item : out String)
      is abstract;

   -- Get a line from the input
   procedure Get_Line(File : in out Abstract_File;
                      Item : out String;
                      Last   : out Natural)
      is abstract;

   -- Is the given character an end-of-line character?  For this
   -- package, end of line is always a single character and will be
   -- translated to this if necessary by the implementation.
   function End_Of_Line(File : in Abstract_File;
                        Ch   : in Character)
                        return Boolean
      is abstract;

   -- Add a character back into the input stream.  The user should
   -- not generally use this, it is used by the various input routines
   -- that read in integers and the like.
   procedure Putback_Char(File : in out Abstract_File;
                          Item : in Character)
      is abstract;

   -- Return the current column position of the output, which is the
   -- location that next character will go into.
   function Next_Column(File : in Abstract_File)
                        return Positive
      is abstract;

   -- Return the width of the output device, in characters.  If the device
   -- has no known width, this should return 80.
   function IO_Width(File : in Abstract_File)
                     return Positive
      is abstract;

   -- Return the heigth of the output device, in characters.  If the
   -- device has no known height, this should return 0.
   function IO_Height(File : in Abstract_File)
                      return Positive
      is abstract;

   -- Output a string, word-wrapping and putting in new lines as
   -- necessary.  Trailing spaces in the item are ignored, leading
   -- spaces are ignored if the item starts at the beginning of
   -- a line.  Note also that a newline is NOT put at the end of
   -- the I/O, you must do that yourself.  This way, multiple
   -- calls can just continue appending wrapped data as long as
   -- the second and later calls have the spaces between the words
   -- at the start of the item.
   procedure Put_Wrapped(File         : in out Abstract_File;
                         Item         : in String;
                         Left_Indent  : in Natural := 0;
                         Right_Indent : in Natural := 0);


   -- The following packages provide get and put operations for the various
   -- numeric and enumeration types in Ada.  They work much like the
   -- Ada.Text_IO versions, except for the following:
   --
   --  * To support wrapped I/O, every Put takes a Wrapped boolean.  If
   --    Wrapped is True, then Put_Wrapped is used to output the
   --    number, and the Left_Indent and Right_Indent are passed to
   --    it.  A Prefix string is provided that is always prepended
   --    to the number before output.
   --  * Get takes a Base, which specifies the base of the input number.
   --  * Numeric I/O is done without the <base>#<number>#, just the number
   --    is input/output.


   generic
      type Num is range <>;
   package Integer_IO is

      Default_Width : Field := Num'Width;
      Default_Base  : Number_Base := 10;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in Field := 0;
                    Base  : in Number_Base := Default_Base);

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Width   : in Field := Default_Width;
                    Base    : in Number_Base := Default_Base;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "");
   end Integer_IO;

   generic
      type Num is mod <>;
   package Modular_IO is

      Default_Width : Field := Num'Width;
      Default_Base  : Number_Base := 10;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in Field := 0;
                    Base  : in Number_Base := Default_Base);

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Width   : in Field := Default_Width;
                    Base    : in Number_Base := Default_Base;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "");
   end Modular_IO;

   -- Generic packages for Input-Output of Real Types

   generic
      type Num is digits <>;
   package Float_IO is

      Default_Fore : Field := 2;
      Default_Aft  : Field := Num'Digits-1;
      Default_Exp  : Field := 3;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in  Field := 0);

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Fore    : in Field := Default_Fore;
                    Aft     : in Field := Default_Aft;
                    Exp     : in Field := Default_Exp;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "");
   end Float_IO;

   generic
      type Num is delta <>;
   package Fixed_IO is

      Default_Fore : Field := Num'Fore;
      Default_Aft  : Field := Num'Aft;
      Default_Exp  : Field := 0;

      procedure Get(File  : in out Abstract_File;
                    Item  : out Num;
                    Width : in  Field := 0);

      procedure Put(File    : in out Abstract_File;
                    Item    : in Num;
                    Fore    : in Field := Default_Fore;
                    Aft     : in Field := Default_Aft;
                    Exp     : in Field := Default_Exp;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "");
   end Fixed_IO;

--     generic
--        type Num is delta <> digits <>;
--     package Decimal_IO is

--        Default_Fore : Field := Num'Fore;
--        Default_Aft  : Field := Num'Aft;
--        Default_Exp  : Field := 0;

--        procedure Get(File  : in out Abstract_File;
--                      Item  : out Num;
--                      Width : in  Field := 0);

--        procedure Put(File    : in out Abstract_File;
--                      Item    : in Num;
--                      Fore    : in Field := Default_Fore;
--                      Aft     : in Field := Default_Aft;
--                      Exp     : in Field := Default_Exp;
--                      Wrapped : in Boolean := False;
--                      Left_Indent  : in Natural := 0;
--                      Right_Indent : in Natural := 0;
--                      Prefix  : in String := "");
--     end Decimal_IO;

   generic
      type Enum is (<>);
   package Enumeration_IO is

      Default_Width   : Field := 0;
      Default_Setting : Type_Set := Upper_Case;

      procedure Get(File : in out Abstract_File;
                    Item : out Enum);

      procedure Put(File    : in out Abstract_File;
                    Item    : in Enum;
                    Width   : in Field    := Default_Width;
                    Set     : in Type_Set := Default_Setting;
                    Wrapped : in Boolean := False;
                    Left_Indent  : in Natural := 0;
                    Right_Indent : in Natural := 0;
                    Prefix  : in String := "");
   end Enumeration_IO;

private

   type Abstract_File is abstract tagged limited null record;

end Asl.Abstract_IO;
