-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
--
-- This code is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This code is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--

with Asgc.Setops;
with Asl.Leak_Detect_Pool;

package Test_P is

   My_Pool : Asl.Leak_Detect_Pool.Leak_Pool;

   function Hash_Integer (Val : Integer) return Natural;

   package Test1_Con is new Asgc(Contained_Type => Integer);
   package Test1_Setops is new Test1_Con.Setops;
   use type Test1_Con.End_Marker;
   use type Test1_Con.Iterator;

   type Integer_Array is array (Positive range <>) of Integer;

   type String_Ptr is access all String;

   type Print_Val (Name : String_Ptr) is
     new Test1_Con.Callbacks with null record;
   type Print_Val_Ptr is access all Print_Val;
   procedure Added (Cb  : access Print_Val;
                    O   : in Test1_Con.Object'Class;
                    Val : in out Integer);

   procedure Copied (Cb  : access Print_Val;
                     O   : in Test1_Con.Object'Class;
                     Val : in out Integer);

   procedure Deleted (Cb  : access Print_Val;
                      O   : in Test1_Con.Object'Class;
                      Val : in out Integer);

   Add_Not_Found : exception;
   procedure Verify_Add (Val : in Integer);

   Delete_Not_Found : exception;
   procedure Verify_Delete (Val : in Integer);

   Copied_Not_Found : exception;
   procedure Verify_Copied (Val : in Integer);

   Not_Empty : exception;
   procedure Verify_Empty;

   procedure Verify_Add_List (Vals : in Integer_Array);

   procedure Verify_Delete_List (Vals : in Integer_Array);

   procedure Verify_Copied_List (Vals : in Integer_Array);

   Incorrect_Contents : exception;
   procedure Verify_Contents_Ordered (Vals : in Integer_Array;
                                      Iter : in out Test1_Con.Iterator'Class;
                                      O    : in Test1_Con.Object'Class);

   procedure Verify_Contents_Unordered (Vals : in Integer_Array;
                                        Iter : in out Test1_Con.Iterator'Class;
                                        O    : in Test1_Con.Object'Class);

   Leak_Found : exception;
   procedure Check_Leaks;

end Test_P;
