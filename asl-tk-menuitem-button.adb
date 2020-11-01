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

package body Asl.Tk.Menuitem.Button is

   function Handle_Menu_Press (Data            : Asl.Tcl.ClientData;
                               Interp          : Asl.Tcl.Tcl_Interp;
                               Argc            : C.Int;
                               Argv            : CArgv.Chars_Ptr_Ptr)
                               return C.Int;
   pragma Export (C, Handle_Menu_Press, "Handle_menu_Press");


   ------------------------------------------------------------------------
   function Handle_Menu_Press (Data            : Asl.Tcl.ClientData;
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
   end Handle_Menu_Press;


   ------------------------------------------------------------------------
   procedure Initialize (W : in out Widget) is
      Client_Parm : Asl.Tcl.ClientData := W'Unchecked_Access;
   begin
      W.Cmd := Button_ASU;
      W.Press_Cmd := ASU.To_Unbounded_String
        ("press_event"
         & IntToStr_Chop_Space(Event_Handler_Count));

      Event_Handler_Count := Event_Handler_Count + 1;

      Asl.Tcl.Tcl_CreateCommand (Interp,
                                 C.To_C(ASU.To_String(W.Press_Cmd)),
                                 Handle_Menu_Press'Access,
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

end Asl.Tk.Menuitem.Button;

