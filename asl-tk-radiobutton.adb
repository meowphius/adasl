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

with CArgv;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Asl.Tk.Radiobutton is

   procedure Free is new Ada.Unchecked_Deallocation (Group_Iterator,
                                                     Group_Iterator_Ptr);

   function Handle_Radiobutton_Press (Data            : Asl.Tcl.ClientData;
                                      Interp          : Asl.Tcl.Tcl_Interp;
                                      Argc            : C.Int;
                                      Argv            : CArgv.Chars_Ptr_Ptr)
                                      return C.Int;
   pragma Export (C, Handle_Radiobutton_Press, "Handle_Radiobutton_Press");


   ------------------------------------------------------------------------
   function Handle_Radiobutton_Press (Data            : Asl.Tcl.ClientData;
                                      Interp          : Asl.Tcl.Tcl_Interp;
                                      Argc            : C.Int;
                                      Argv            : CArgv.Chars_Ptr_Ptr)
                                      return C.Int is
      W       : Widget_Ptr := Widget_Ptr(Data);
      List    : Asl.Tk.Event_Class := W.On_Press;
      Changed : Boolean := True;
   begin
      if (W.Group.Current = Widget_Class(W)) then
         Changed := False;
      end if;

      W.Group.Current := Widget_Class(W);

      while (List /= null) loop
         Handle(Press_Event_Class(List).all, W.all);
         List := List.Next;
      end loop;

      if (Changed) then
         List := W.Group.On_Change;
         while (List /= null) loop
            Handle(Change_Event_Class(List).all, W.Group.all);
            List := List.Next;
         end loop;
      end if;

      return Asl.Tcl.TCL_OK;
   end Handle_Radiobutton_Press;


   ------------------------------------------------------------------------
   function Get_Text (W : in Widget)
                      return String is
   begin
      return Get_Attribute(W, "-text");
   end Get_Text;


   ------------------------------------------------------------------------
   procedure Initialize (W : in out Widget) is
      Client_Parm : Asl.Tcl.ClientData := W'Unchecked_Access;
   begin
      W.Cmd := Radiobutton_ASU;
      W.Press_Cmd := ASU.To_Unbounded_String
        ("press_event"
         & IntToStr_Chop_Space(Event_Handler_Count));

      Event_Handler_Count := Event_Handler_Count + 1;

      Asl.Tcl.Tcl_CreateCommand (Interp,
                                 C.To_C(ASU.To_String(W.Press_Cmd)),
                                 Handle_Radiobutton_Press'Access,
                                 Client_Parm,
                                 null);

      Add_Attribute (W, "-command", ASU.To_String(W.Press_Cmd));
   end Initialize;


   ------------------------------------------------------------------------
   procedure Finalize (W : in out Widget) is
   begin
      Asl.Tcl.Tcl_DeleteCommand (Interp,
                                 C.To_C(ASU.To_String(W.Press_Cmd)));

      -- Handle destroying widget if displayed
      Destroy(W);
   end Finalize;


   ------------------------------------------------------------------------
   procedure Set_Text (W    : in out Widget;
                       Name : in String) is
   begin
      Add_Attribute(W, "-text", """" & Name & """");
   end Set_Text;


   ------------------------------------------------------------------------
   procedure Set_Font (W    : in out Widget;
                       Font : in Font_Attr) is
   begin
      Add_Attribute(W, "-font", """" & ASU.To_String(Font.Name) & """");
   end Set_Font;


   ------------------------------------------------------------------------
   function Get_Font (W : in Widget)
                      return Font_Attr is
      Retval : Font_Attr;
   begin
      Retval.Name := Retval.Name & Get_Attribute(W, "-font");

      return Retval;
   end Get_Font;


   ------------------------------------------------------------------------
   procedure Set_Image (W     : in out Widget;
                        Image : in Asl.Tk.Image.Object) is
   begin
      Add_Attribute(W, "-image", Asl.Tk.Image.Attribute(Image));
   end Set_Image;


   ------------------------------------------------------------------------
   function Is_Set (W : in Widget)
                    return Boolean is
   begin
      if ((W.Group.Current /= null)
          and then (W.Group.Current.Value = W.Value))
      then
         return True;
      else
         return False;
      end if;
   end Is_Set;


   ------------------------------------------------------------------------
   procedure Set_Value (W     : in out Widget;
                        Value : in Boolean) is
      Rv      : C.Int;
      List    : Asl.Tk.Event_Class;
      Changed : Boolean := False;
   begin
      if (W.Group.Current = W'Unchecked_Access) then
         if (not Value) then
            Rv := Asl.Tcl.Tcl_Eval(Interp, "set "
                                   & ASU.To_String(W.Group.Varname)
                                   & " 0");
            W.Group.Current := null;
            Changed := True;
         end if;
      else
         if (Value) then
            Rv := Asl.Tcl.Tcl_Eval(Interp, "set "
                                   & ASU.To_String(W.Group.Varname)
                                   & " "
                                   & IntToStr_Chop_Space(W.Value));
            W.Group.Current := W'Unchecked_Access;
            Changed := True;
         end if;
      end if;

      if (Changed) then
         List := W.Group.On_Change;
         while (List /= null) loop
            Handle(Change_Event_Class(List).all, W.Group.all);
            List := List.Next;
         end loop;
      end if;
   end Set_Value;


   ------------------------------------------------------------------------
   procedure Display (W : in out Widget) is
   begin
      if (W.Group = null) then
         raise Radiobutton_Not_In_Group;
      end if;

      Display(Widget_Parent(W));
   end Display;


   ------------------------------------------------------------------------
   procedure Add_Event (W     : in out Widget;
                        Event : in Press_Event_Class) is
   begin
      Add_Event(W.On_Press, Asl.Tk.Event_Class(Event));
   end Add_Event;

   ------------------------------------------------------------------------
   procedure Remove_Event (W     : in out Widget;
                           Event : in Press_Event_Class) is
   begin
      Remove_Event(W.On_Press, Asl.Tk.Event_Class(Event));
   end Remove_Event;


   ------------------------------------------------------------------------
   procedure Add_To_Group (G : in out Group;
                           W : in Widget_Class) is
   begin
      W.Value := G.Group_Value;
      G.Group_Value := G.Group_Value + 1;
      W.Group := G'Unchecked_Access;
      Group_List.Enqueue(G.Members.all, W);
      Add_Attribute (W.all, "-variable", ASU.To_String(G.Varname));
      Add_Attribute (W.all, "-value", IntToStr_Chop_Space(W.Value));
   end Add_To_Group;


   ------------------------------------------------------------------------
   procedure Remove_From_Group (W : in out Widget) is
   begin
      Group_List.Delete(W.Group.Members.all, W'Unchecked_Access);
      W.Group := null;
      Add_Attribute(W, "-variable", "dummyvar");
   end Remove_From_Group;


   ------------------------------------------------------------------------
   procedure Initialize (W : in out Group) is
   begin
      W.Varname := (ASU.To_Unbounded_String("tkrbvar")
                    & IntToStr_Chop_Space(Curr_Varnum));
      Curr_Varnum := Curr_Varnum + 1;
   end Initialize;


   ------------------------------------------------------------------------
   procedure Finalize (W : in out Group) is
   begin
      null;
   end Finalize;


   ------------------------------------------------------------------------
   function Get_Current_Button (W : in Group)
                                return Widget_Class is
   begin
      return W.Current;
   end Get_Current_Button;


   ------------------------------------------------------------------------
   procedure Add_Event (W     : in out Group;
                        Event : in Change_Event_Class) is
   begin
      Add_Event(W.On_Change, Asl.Tk.Event_Class(Event));
   end Add_Event;

   ------------------------------------------------------------------------
   procedure Remove_Event (W     : in out Group;
                           Event : in Change_Event_Class) is
   begin
      Remove_Event(W.On_Change, Asl.Tk.Event_Class(Event));
   end Remove_Event;


   ------------------------------------------------------------------------
   function Get_Iterator (W : in Group)
                          return Group_Iterator_Class is
      Retval   : Group_Iterator_Ptr;
   begin
      Retval := new Group_Iterator;
      Group_List.Set_Container(Retval.Iterator,
                               Group_List_Base.Object_Class(W.Members));

      return Group_Iterator_Class(Retval);
   end Get_Iterator;


   ------------------------------------------------------------------------
   procedure Free_Iterator (Iter : access Group_Iterator) is
      To_Free : Group_Iterator_Ptr := Group_Iterator_Ptr(Iter);
   begin
      Free(To_Free);
   end Free_Iterator;


   ------------------------------------------------------------------------
   procedure First (Iter   : in out Group_Iterator;
                    At_End : out Boolean) is
      Is_End : Group_List_Base.End_Marker;
   begin
      Group_List.First(Iter.Iterator, Is_End);
      if (Is_End = Group_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end First;


   ------------------------------------------------------------------------
   procedure Last (Iter   : in out Group_Iterator;
                   At_End : out Boolean) is
      Is_End : Group_List_Base.End_Marker;
   begin
      Group_List.Last(Iter.Iterator, Is_End);
      if (Is_End = Group_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end Last;


   ------------------------------------------------------------------------
   procedure Next (Iter   : in out Group_Iterator;
                   At_End : out Boolean) is
      Is_End : Group_List_Base.End_Marker;
   begin
      Group_List.Next(Iter.Iterator, Is_End);
      if (Is_End = Group_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end Next;


   ------------------------------------------------------------------------
   procedure Prev (Iter   : in out Group_Iterator;
                   At_End : out Boolean) is
      Is_End : Group_List_Base.End_Marker;
   begin
      Group_List.Prev(Iter.Iterator, Is_End);
      if (Is_End = Group_List_Base.Past_End) then
         At_End := True;
      else
         At_End := False;
      end if;
   end Prev;


   ------------------------------------------------------------------------
   function Get (Iter : in Group_Iterator)
                 return Widget_Class is
   begin
      return Group_List.Get(Iter.Iterator);
   end Get;


end Asl.Tk.Radiobutton;

