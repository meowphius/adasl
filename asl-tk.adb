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
with Asl.Tk.Frame;

package body Asl.Tk is

   ------------------------------------------------------------------------
   function IntToStr_Chop_Space (I : in Integer)
                                 return String is
      Str : String := Integer'Image(I);
   begin
      if (Str(Str'First) = ' ') then
         return Str(Str'First + 1 .. Str'Last);
      else
         return Str;
      end if;
   end IntToStr_Chop_Space;


   ------------------------------------------------------------------------
   procedure Name_Widget (W : in out Widget'Class) is
      Rv : C.Int;
   begin
      if (ASU.Length(W.Name) = 0) then
         if (W.Parent = null) then
            W.Name := ASU.To_Unbounded_String
              (".widget"
               & IntToStr_Chop_Space(Base_Widget_Count));
            Base_Widget_Count := Base_Widget_Count + 1;
         else
            W.Name := W.Parent.Name
              & ASU.To_Unbounded_String
                  (".widget"
                   & IntToStr_Chop_Space(W.Parent.Child_Num));
            W.Parent.Child_Num := W.Parent.Child_Num + 1;
         end if;
      end if;

      Rv := Asl.Tcl.Tcl_Eval(Interp,
                             W.Cmd
                             & Space_ASU
                             & W.Name
                             & Space_ASU
                             & W.Attributes);

      W.Attributes := ASU.To_Unbounded_String("");

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Name_Widget;


   ------------------------------------------------------------------------
   procedure Apply_Attribute (W : in out Widget'Class) is
      Rv : C.Int;
   begin
      if (W'Unchecked_Access = Widget_Class(Root_Frame)) then
         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                "." & Configure_ASU
                                & W.Attributes);
         W.Attributes := ASU.To_Unbounded_String("");

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;
      elsif (W.Name /= ASU.Null_Unbounded_String) then
         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                W.Name & Configure_ASU
                                & W.Attributes);
         W.Attributes := ASU.To_Unbounded_String("");

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;
      end if;
   end Apply_Attribute;


   ------------------------------------------------------------------------
   function Get_Attribute (W         : in Widget'Class;
                           Attr_Name : in String)
                           return String is
      Rv : C.Int;
   begin
      if (W.Name /= ASU.Null_Unbounded_String) then
         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                W.Name & CGet_ASU & Attr_Name);

         if (Rv /= Asl.Tcl.TCL_OK) then
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
         end if;

         return CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp));
      else
         -- FIXME - need to search the attribute list if it hasn't been
         -- displayed yet.
         return "";
      end if;
   end Get_Attribute;


   ------------------------------------------------------------------------
   procedure Add_Attribute (W         : in out Widget'Class;
                            Attr_Name : in String) is
   begin
      W.Attributes :=
        W.Attributes
        & Space_ASU
        & ASU.To_Unbounded_String(Attr_Name);

      Apply_Attribute(W);
   end Add_Attribute;


   ------------------------------------------------------------------------
   procedure Add_Attribute (W         : in out Widget'Class;
                            Attr_Name : in String;
                            Attr_Val  : in String) is
   begin
      W.Attributes :=
        W.Attributes
        & Space_ASU
        & ASU.To_Unbounded_String(Attr_Name & " " & Attr_Val);

      Apply_Attribute(W);
   end Add_Attribute;


   ------------------------------------------------------------------------
   procedure Display (W : in out Widget) is
   begin
      if (not W.Displayed) then
         Map(Managed_Class(W.Parent).Manager.all, W);
      end if;
   end Display;


   ------------------------------------------------------------------------
   procedure Undisplay (W : in out Widget) is
   begin
      if (W.Displayed) then
         Unmap(Managed_Class(W.Parent).Manager.all, W);
      end if;
   end Undisplay;


   ------------------------------------------------------------------------
   function Get_Background (W : in Widget)
                            return Color_Attr is
      Retval : Color_Attr;
   begin
      return (Name => ASU.To_Unbounded_String
              (Get_Attribute(W, "-background")));
   end Get_Background;


   ------------------------------------------------------------------------
   function Get_Foreground (W : in Widget)
                            return Color_Attr is
   begin
      return (Name => ASU.To_Unbounded_String
              (Get_Attribute(W, "-foreground")));
   end Get_Foreground;


   ------------------------------------------------------------------------
   function Get_Size (W : in Widget)
                      return Size_Attr is
   begin
      return (Width => Natural'Value(Get_Attribute(W, "-width")),
              Height => Natural'Value(Get_Attribute(W, "-height")));
   end Get_Size;


   ------------------------------------------------------------------------
   procedure Set_Layout_Manager (W       : in out Managed;
                                 Manager : in Layout_Class) is
   begin
      W.Manager := Manager;
      Set_Layout_Master(Manager.all, W'Unchecked_Access);
   end Set_Layout_Manager;


   ------------------------------------------------------------------------
   procedure Internal_Set_Parent (W      : in out Widget'Class;
                                  Parent : in Managed_Class) is
   begin
      if (W.Displayed) then
         raise Already_Displayed;
      end if;
      if (W.Name /= ASU.Null_Unbounded_String) then
         raise Already_Named;
      end if;

      W.Displayed := True;

      if (Parent = null) then
         W.Parent := Container_Class(Root_Frame);
      else
         W.Parent := Container_Class(Parent);
      end if;

      Name_Widget(W);
   end Internal_Set_Parent;


   ------------------------------------------------------------------------
   procedure Set_Parent (W      : in Widget_Class;
                         Parent : in Managed_Class) is
   begin
      Internal_Set_Parent(W.all, Parent);
      Add_Child(Managed'Class(W.Parent.all), W);
      Set_Default_Constraints(Managed_Class(W.Parent).Manager.all, W);
   end Set_Parent;


   ------------------------------------------------------------------------
   procedure Set_Parent (W           : in Widget_Class;
                         Parent      : in Managed_Class;
                         Constraints : in Constraint'Class) is
   begin
      Internal_Set_Parent(W.all, Parent);
      Add_Child(Managed'Class(W.Parent.all), W);
      Set_Constraints(Managed_Class(W.Parent).Manager.all, W, Constraints);
   end Set_Parent;


   ------------------------------------------------------------------------
   function Get_Parent (W : in Widget)
                        return Container_Class is
   begin
      return W.Parent;
   end Get_Parent;


   ------------------------------------------------------------------------
   procedure Set_Constraints (W           : in Widget_Class;
                              Constraints : in Constraint'Class) is
   begin
      if (W.Name = ASU.Null_Unbounded_String) then
         W.Name :=
           ASU.To_Unbounded_String(".widget"
                                   & IntToStr_Chop_Space(Base_Widget_Count));
         Base_Widget_Count := Base_Widget_Count + 1;
      end if;

      Set_Constraints(Managed_Class(W.Parent).Manager.all, W, Constraints);
   end Set_Constraints;


   ------------------------------------------------------------------------
   procedure Set_Background (W     : in out Widget;
                             Color : in Color_Attr) is
   begin
      Add_Attribute(W, "-background", ASU.To_String(Color.Name));
   end Set_Background;


   ------------------------------------------------------------------------
   procedure Set_Foreground (W     : in out Widget;
                             Color : in Color_Attr) is
   begin
      Add_Attribute(W, "-foreground", ASU.To_String(Color.Name));
   end Set_Foreground;


   ------------------------------------------------------------------------
   procedure Set_Size (W      : in out Widget;
                       Size   : in Size_Attr) is
   begin
      Add_Attribute(W, "-height", IntToStr_Chop_Space(Size.Height));
      Add_Attribute(W, "-width", IntToStr_Chop_Space(Size.Width));
   end Set_Size;


   ------------------------------------------------------------------------
   function To_Color (Name  : in String)
                      return Color_Attr is
   begin
      return (Name => ASU.To_Unbounded_String(Name));
   end To_Color;


   ------------------------------------------------------------------------
   function To_Color (Red   : in RGB_Value;
                      Green : in RGB_Value;
                      Blue  : in RGB_Value)
                      return Color_Attr is

      function RGB_Value_To_String (Val : in RGB_Value)
                                    return String is
         Retval  : String(1 .. 4);
         Loc_Val : RGB_Value := Val;
      begin
         for I in reverse Retval'Range loop
            Retval(I) := Character'Val(Loc_Val mod 16);
            Loc_Val := Loc_Val / 16;
         end loop;
         return Retval;
      end RGB_Value_To_String;

   begin
      return (Name => ASU.To_Unbounded_String("#"
                                              & RGB_Value_To_String(Red)
                                              & RGB_Value_To_String(Green)
                                              & RGB_Value_To_String(Blue)));
   end To_Color;


   ------------------------------------------------------------------------
   function To_Font (Name : in String)
                     return Font_Attr is
   begin
      return (Name => ASU.To_Unbounded_String(Name));
   end To_Font;


   ------------------------------------------------------------------------
   procedure Destroy (W : in out Widget) is
      Rv : C.Int;
   begin
      if (W.Displayed) then
         if (W.Parent /= null) then
            Remove_Child(Container'Class(W.Parent.all), W'Unchecked_Access);
         end if;

         if (not In_Shutdown) then
            Rv := Asl.Tcl.Tcl_Eval(Interp, Destroy_ASU & W.Name);

            if (Rv /= Asl.Tcl.TCL_OK) then
               Ada.Exceptions.Raise_Exception
                 (Widget_Error'Identity,
                  CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
            end if;
         end if;
      end if;
   end Destroy;


   ------------------------------------------------------------------------
   procedure Add_Event (Event_List : in out Event_Class;
                        Event      : in Event_Class) is
   begin
      Event.Next := Event_List;
      Event_List := Event;
   end Add_Event;


   ------------------------------------------------------------------------
   procedure Remove_Event (Event_List : in out Event_Class;
                           Event      : in Event_Class) is
      Curr : Event_Class;
   begin
      if (Event_List = Event) then
         Event_List := Event_List.Next;
      else
         Curr := Event_List;
         while ((Curr.Next /= null) and then (Curr.Next /= Event)) loop
            Curr := Curr.Next;
         end loop;

         if (Curr.Next = null) then
            raise Event_Not_Found;
         else
            Curr.Next := Curr.Next.Next;
         end if;
      end if;
   end Remove_Event;


   ------------------------------------------------------------------------
   procedure Initialize is
      function Init (Interp : in Asl.Tcl.Tcl_Interp)
                     return C.Int;
      pragma Import (C, Init, "Tk_Init");
   begin
      Interp := Asl.Tcl.Tcl_CreateInterp;
      if (Asl.Tcl.Tcl_Init(Interp) = Asl.Tcl.TCL_ERROR) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      if (Init(Interp) = Asl.Tcl.TCL_ERROR) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      -- Create a dummy frame to hold the children of the root, but don't
      -- really map it or anything.
      Root_Frame := new Asl.Tk.Frame.Widget;
      Root_Frame.Parent := null;
      Root_Frame.Name := ASU.To_Unbounded_String("");
   end Initialize;


   ------------------------------------------------------------------------
   procedure Mainloop is
      procedure MainLoop;
      pragma Import (C, MainLoop, "Tk_MainLoop");
   begin
      MainLoop;
   end Mainloop;


   ------------------------------------------------------------------------
   procedure Shutdown is
      procedure Free is new Ada.Unchecked_Deallocation
        (Asl.Tk.Frame.Widget, Asl.Tk.Frame.Widget_Ptr);
      Container : Container_Class;
   begin
      In_Shutdown := True;
      for I in 1 .. Container_List.Member_Count(All_Containers) loop
         Container := Container_List.Get_At(All_Containers, I);
         Shutdown(Container.all);
      end loop;
      Free(Asl.Tk.Frame.Widget_Ptr(Root_Frame));
   end Shutdown;

end Asl.Tk;

