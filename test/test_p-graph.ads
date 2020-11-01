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

with Asgc.Graph.Dynamic;
with Asgc.Graph.Fixed;
with Asgc.Graph.Expandable;
with Asgc.Graph.Links.Dynamic_Managed;
with Asgc.Graph.Links.Fixed;
with Asgc.Graph.Links.Expandable_Managed;
with Ada.Unchecked_Deallocation;

package Test_P.Graph is

   package Test1_Graph is new Test1_Con.Graph
     (Do_Hash             => Hash_Integer,
      Link_Contained_Type => Integer);

   type Print_Val (Name : String_Ptr) is
     new Test1_Graph.Link_Callbacks with null record;
   type Print_Val_Ptr is access all Print_Val;
   procedure Added (Cb  : access Print_Val;
                    O   : in Test1_Graph.Object'Class;
                    Val : in out Integer);

   procedure Copied (Cb  : access Print_Val;
                     O   : in Test1_Graph.Object'Class;
                     Val : in out Integer);

   procedure Deleted (Cb  : access Print_Val;
                      O   : in Test1_Graph.Object'Class;
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

   type IAR is record
      Link : Integer;
      Val  : Integer;
   end record;
   type IA is array (Positive range <>) of IAR;
   type IAP is access all IA;

   type Node_Entry is record
      Val   : Integer;
      Links : IAP;
   end record;
   type Node_Entry_Array is array (Positive range <>) of Node_Entry;

   procedure Free is new Ada.Unchecked_Deallocation(IA, IAP);

   Invalid_Contents : exception;
   procedure Verify_Contents (Vals  : in Node_Entry_Array;
                              Iter1 : in out Test1_Graph.Iterator'Class;
                              Iter2 : in out Test1_Graph.Iterator'Class;
                              O     : in Test1_Graph.Object'Class);



   package Test1_Graph_Dyn is new Test1_Graph.Dynamic;

   package Test1_Dyn_Graph_Dyn is new Test1_Graph_Dyn.Links.Dynamic_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Dyn_Graph_Fix is
     new Test1_Graph_Dyn.Links.Fixed(Size => 100);

   package Test1_Dyn_Graph_Exp is
     new Test1_Graph_Dyn.Links.Expandable_Managed
     (Initial_Size => 10,
      Increment    => 10,
      Pool_Type    => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool      => My_Pool);


   package Test1_Graph_Fix is new Test1_Graph.Fixed;

   package Test1_Fix_Graph_Dyn is new Test1_Graph_Fix.Links.Dynamic_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Fix_Graph_Fix is
     new Test1_Graph_Fix.Links.Fixed(Size => 100);

   package Test1_Fix_Graph_Exp is
     new Test1_Graph_Fix.Links.Expandable_Managed
     (Initial_Size => 10,
      Increment    => 10,
      Pool_Type    => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool      => My_Pool);


   package Test1_Graph_Exp is new Test1_Graph.Expandable;

   package Test1_Exp_Graph_Dyn is new Test1_Graph_Exp.Links.Dynamic_Managed
     (Pool_Type => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool   => My_Pool);

   package Test1_Exp_Graph_Fix is
     new Test1_Graph_Exp.Links.Fixed(Size => 100);

   package Test1_Exp_Graph_Exp is
     new Test1_Graph_Exp.Links.Expandable_Managed
     (Initial_Size => 10,
      Increment    => 10,
      Pool_Type    => Asl.Leak_Detect_Pool.Leak_Pool,
      My_Pool      => My_Pool);

end Test_P.Graph;
