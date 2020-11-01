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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Asl.Tk.Grid is

   procedure Free is new Ada.Unchecked_Deallocation (Layout_Info,
                                                     Layout_Info_Ptr);

   procedure Set_Layout_Master (Manager : in out Layout;
                                Master  : in Managed_Class) is
   begin
      Manager.Master := Master;
   end Set_Layout_Master;


   procedure Set_Default_Constraints (Manager     : in out Layout;
                                      Child       : in Asl.Tk.Widget_Class) is
      Rv : C.Int;
   begin
      Rv := Asl.Tcl.Tcl_Eval
        (Interp, C.To_C("grid " & ASU.To_String(Child.Name)));

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Set_Default_Constraints;

  type Anchor_Fill_Conv_Array is array (Fill_Types, Anchor_Positions)
    of ASU.String_Access;

   Anchor_Fill_Conv : Anchor_Fill_Conv_Array :=
     (None         => (North          => new String'(" -sticky n"),
                       Northeast      => new String'(" -sticky ne"),
                       East           => new String'(" -sticky e"),
                       Southeast      => new String'(" -sticky se"),
                       South          => new String'(" -sticky s"),
                       Southwest      => new String'(" -sticky sw"),
                       West           => new String'(" -sticky w"),
                       Northwest      => new String'(" -sticky nw"),
                       Center         => new String'(" -sticky {}"),
                       Default_Anchor => null),
      Both         => (North          => new String'(" -sticky nsew"),
                       Northeast      => new String'(" -sticky nsew"),
                       East           => new String'(" -sticky nsew"),
                       Southeast      => new String'(" -sticky nsew"),
                       South          => new String'(" -sticky nsew"),
                       Southwest      => new String'(" -sticky nsew"),
                       West           => new String'(" -sticky nsew"),
                       Northwest      => new String'(" -sticky nsew"),
                       Center         => new String'(" -sticky nsew"),
                       Default_Anchor => null),
      Horizontal   => (North          => new String'(" -sticky new"),
                       Northeast      => new String'(" -sticky new"),
                       East           => new String'(" -sticky ew"),
                       Southeast      => new String'(" -sticky sew"),
                       South          => new String'(" -sticky sew"),
                       Southwest      => new String'(" -sticky sew"),
                       West           => new String'(" -sticky ew"),
                       Northwest      => new String'(" -sticky new"),
                       Center         => new String'(" -sticky ew"),
                       Default_Anchor => null),
      Vertical     => (North          => new String'(" -sticky ns"),
                       Northeast      => new String'(" -sticky nse"),
                       East           => new String'(" -sticky nse"),
                       Southeast      => new String'(" -sticky nse"),
                       South          => new String'(" -sticky ns"),
                       Southwest      => new String'(" -sticky nsw"),
                       West           => new String'(" -sticky nsw"),
                       Northwest      => new String'(" -sticky nsw"),
                       Center         => new String'(" -sticky ns"),
                       Default_Anchor => null),
      Default_Fill => (North          => null,
                       Northeast      => null,
                       East           => null,
                       Southeast      => null,
                       South          => null,
                       Southwest      => null,
                       West           => null,
                       Northwest      => null,
                       Center         => null,
                       Default_Anchor => null));


   procedure Set_Constraints (Manager     : in out Layout;
                              Child       : in Asl.Tk.Widget_Class;
                              Constraints : in Asl.Tk.Constraint'Class) is
      C        : Constraint := Constraint(Constraints);
      Gridstr  : ASU.Unbounded_String
        := ASU.To_Unbounded_String("grid ") & Child.Name;
      My_Layout_Info : Layout_Info_Ptr;
   begin
      if (C.Row /= Positive_Default) then
         Gridstr := Gridstr & " -row " & IntToStr_Chop_Space(C.Row);
      end if;
      if (C.Column /= Positive_Default) then
         Gridstr := Gridstr & " -column " & IntToStr_Chop_Space(C.Column);
      end if;
      if (C.Xpad /= Natural_Default) then
         Gridstr := Gridstr & " -padx " & IntToStr_Chop_Space(C.Xpad);
      end if;
      if (C.Ypad /= Natural_Default) then
         Gridstr := Gridstr & " -pady " & IntToStr_Chop_Space(C.Ypad);
      end if;
      if (C.Rowspan /= Positive_Default) then
         Gridstr := Gridstr & " -rowspan " & IntToStr_Chop_Space(C.Rowspan);
      end if;
      if (C.Columnspan /= Positive_Default) then
         Gridstr := (Gridstr & " -columnspan "
                     & IntToStr_Chop_Space(C.Columnspan));
      end if;
      if ((C.Fill /= Default_Fill)
          or (C.Anchor /= Default_Anchor))
      then
         My_Layout_Info := Layout_Info_Ptr(Child.For_Layout);

         if (C.Fill /= Default_Fill) then
            My_Layout_Info.Curr_Fill := C.Fill;
         end if;
         if (C.Anchor /= Default_Anchor) then
            My_Layout_Info.Curr_Anchor := C.Anchor;
         end if;

         Gridstr := Gridstr & Anchor_Fill_Conv(C.Fill, C.Anchor).all;
      end if;
   end Set_Constraints;

   procedure Set_Row_Weight (The_Layout : in out Layout;
                             Row        : in Positive;
                             Weight     : in Natural) is
      Rv : C.Int;
   begin
      Rv := Asl.Tcl.Tcl_Eval
        (Interp, C.To_C("grid rowconfigure "
                        & ASU.To_String(The_Layout.Master.Name)
                        & " "
                        & IntToStr_Chop_Space(Row)
                        & " -weight "
                        & IntToStr_Chop_Space(Weight)));

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Set_Row_Weight;


   procedure Set_Column_Weight (The_Layout : in out Layout;
                                Column     : in Positive;
                                Weight     : in Natural) is
      Rv : C.Int;
   begin
      Rv := Asl.Tcl.Tcl_Eval
        (Interp, C.To_C("grid columnconfigure "
                        & ASU.To_String(The_Layout.Master.name)
                        & " "
                        & IntToStr_Chop_Space(Column)
                        & " -weight "
                        & IntToStr_Chop_Space(Weight)));

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Set_Column_Weight;


   procedure Child_Added (Manager : in out Layout;
                          Child   : in Widget_Class) is
   begin
      Child.For_Layout := new Layout_Info'(Asl.Tk.Layout_Info with
                                           Curr_Fill   => None,
                                           Curr_Anchor => Center);
   end Child_Added;


   procedure Child_Removed (Manager : in out Layout;
                            Child   : in Widget_Class) is
      Rv      : C.Int;
      To_Free : Layout_Info_Ptr;
   begin
      if (not In_Shutdown) then
         Rv := Asl.Tcl.Tcl_Eval
           (Interp, C.To_C("grid forget "
                           & ASU.To_String(Child.Name)));

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;
      end if;

      To_Free := Layout_Info_Ptr(Child.For_Layout);
      Child.For_Layout := null;
      Child.Parent := null;
      Free(To_Free);
   end Child_Removed;


   procedure Unmap (Manager : in out Layout;
                    W       : in out Asl.Tk.Widget'Class) is
      Rv : C.Int;
   begin
      Rv := Asl.Tcl.Tcl_Eval
        (Interp, C.To_C("grid remove "
                        & ASU.To_String(W.Name)));

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Unmap;

   procedure Map (Manager : in out Layout;
                  W       : in out Asl.Tk.Widget'Class) is
      Rv : C.Int;
   begin
      Rv := Asl.Tcl.Tcl_Eval
        (Interp, C.To_C("grid "
                        & ASU.To_String(W.Name)));

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Map;

end Asl.Tk.Grid;
