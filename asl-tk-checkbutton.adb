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

package body Asl.Tk.Checkbutton is

   function Handle_Checkbutton_Press (Data            : Asl.Tcl.ClientData;
                                      Interp          : Asl.Tcl.Tcl_Interp;
                                      Argc            : C.Int;
                                      Argv            : CArgv.Chars_Ptr_Ptr)
                                      return C.Int;
   pragma Export (C, Handle_Checkbutton_Press, "Handle_Checkbutton_Press");


   ------------------------------------------------------------------------
   function Handle_Checkbutton_Press (Data            : Asl.Tcl.ClientData;
                                      Interp          : Asl.Tcl.Tcl_Interp;
                                      Argc            : C.Int;
                                      Argv            : CArgv.Chars_Ptr_Ptr)
                                      return C.Int is
      W    : Widget_Ptr := Widget_Ptr(Data);
      List : Asl.Tk.Event_Class := W.On_Press;
   begin
      while (List /= null) loop
         Handle(Press_Event_Class(List).all, W.all);
         List := List.Next;
      end loop;

      return Asl.Tcl.TCL_OK;
   end Handle_Checkbutton_Press;


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
      W.Cmd := Checkbutton_ASU;
      W.Varname := (ASU.To_Unbounded_String("tkcbvar")
                    & IntToStr_Chop_Space(Curr_Varnum));
      Curr_Varnum := Curr_Varnum + 1;
      W.Press_Cmd := ASU.To_Unbounded_String
        ("press_event"
         & IntToStr_Chop_Space(Event_Handler_Count));

      Event_Handler_Count := Event_Handler_Count + 1;

      Asl.Tcl.Tcl_CreateCommand (Interp,
                                 C.To_C(ASU.To_String(W.Press_Cmd)),
                                 Handle_Checkbutton_Press'Access,
                                 Client_Parm,
                                 null);

      Add_Attribute (W, "-command", ASU.To_String(W.Press_Cmd));
      Add_Attribute (W, "-variable", Asu.To_String(W.Varname));
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
      Rv : C.Int;
   begin
      Rv := Asl.Tcl.Tcl_Eval(Interp, "set " & ASU.To_String(W.Varname));

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      declare
         Val : String := CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp));
      begin
         if (Val = "0") then
            return False;
         elsif (Val = "1") then
            return True;
         else
            Ada.Exceptions.Raise_Exception
              (Widget_Error'Identity,
               "Checkbutton not 0 or 1");
         end if;
      end;
   end Is_Set;


   ------------------------------------------------------------------------
   procedure Set_Value (W     : in out Widget;
                        Value : in Boolean) is
      Rv : C.Int;
   begin
      if (Value) then
         Rv := Asl.Tcl.Tcl_Eval(Interp, "set " & ASU.To_String(W.Varname)
                                & " 1");
      else
         Rv := Asl.Tcl.Tcl_Eval(Interp, "set " & ASU.To_String(W.Varname)
                                & " 0");
      end if;

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;
   end Set_Value;


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

end Asl.Tk.Checkbutton;

