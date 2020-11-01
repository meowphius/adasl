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

package Asl.Tk.Grid is

   subtype Layout_Parent is Asl.Tk.Layout;
   type Layout is new Layout_Parent with private;
   type Layout_Ptr is access all Layout;
   type Layout_Class is access all Layout'Class;

   type Fill_Types is (None, Both, Horizontal, Vertical, Default_Fill);

   type Anchor_Positions is (North, Northeast, East, Southeast, South,
                             Southwest, West, Northwest, Center,
                             Default_Anchor);

   subtype Positive_Defaultable is Integer range 0 .. Integer'Last;
   subtype Natural_Defaultable is Integer range -1 .. Integer'Last;
   Positive_Default : Positive_Defaultable := 0;
   Natural_Default : Natural_Defaultable := -1;

   subtype Constraint_Parent is Asl.Tk.Constraint;
   type Constraint is new Constraint_Parent with record
      Row        : Positive_Defaultable := Positive_Default;
      Column     : Positive_Defaultable := Positive_Default;
      Xpad       : Natural_Defaultable  := Positive_Default;
      Ypad       : Natural_Defaultable  := Positive_Default;
      Rowspan    : Positive_Defaultable := Positive_Default;
      Columnspan : Positive_defaultable := Positive_Default;
      Fill       : Fill_Types           := Default_Fill;
      Anchor     : Anchor_Positions     := Default_Anchor;
   end record;
   type Constraint_Class is access all Constraint'Class;
   type Constraint_Ptr is access all Constraint;


   -- Routines that affect entire columns or rows.
   procedure Set_Row_Weight (The_Layout : in out Layout;
                             Row        : in Positive;
                             Weight     : in Natural);

   procedure Set_Column_Weight (The_Layout : in out Layout;
                                Column     : in Positive;
                                Weight     : in Natural);


   -- These are only for containers to call.
   procedure Set_Layout_Master (Manager : in out Layout;
                                Master  : in Managed_Class);

   procedure Child_Added (Manager : in out Layout;
                          Child   : in Widget_Class);

   procedure Child_Removed (Manager : in out Layout;
                            Child   : in Widget_Class);

   procedure Set_Default_Constraints (Manager     : in out Layout;
                                      Child       : in Asl.Tk.Widget_Class);

   procedure Set_Constraints (Manager     : in out Layout;
                              Child       : in Asl.Tk.Widget_Class;
                              Constraints : in Asl.Tk.Constraint'Class);

   procedure Unmap (Manager : in out Layout;
                    W       : in out Asl.Tk.Widget'Class);

   procedure Map (Manager : in out Layout;
                  W       : in out Asl.Tk.Widget'Class);

private

   type Layout_Info is new Asl.Tk.Layout_Info with record
      Curr_Fill   : Fill_Types;
      Curr_Anchor : Anchor_Positions;
   end record;
   type Layout_Info_Ptr is access all Layout_Info;

   type Layout is new Layout_Parent with record
      Master : Asl.Tk.Managed_Class;
   end record;

end Asl.Tk.Grid;
