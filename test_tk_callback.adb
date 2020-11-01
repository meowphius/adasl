
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Tk_Callback is

   procedure Handle (Event : in out Button1_Callback;
                     W     : in GUI.Button.Widget'Class) is
   begin
      Put_Line("Button pressed");
   end Handle;

   procedure Handle (Event : in out Checkbutton1_Callback;
                     W     : in GUI.Checkbutton.Widget'Class) is
   begin
      Put_Line("Checkbutton pressed, value is "
               & Boolean'Image(GUI.Checkbutton.Is_Set(W)));
   end Handle;

   procedure Handle (Event : in out Menu1_Callback;
                     W     : in Asl.Tk.Menuitem.Button.Widget'Class) is
   begin
      GUI.Checkbutton.Set_Value(Menu_To_Set.all, True);
   end Handle;

   procedure Handle (Event : in out Menu2_Callback;
                     W     : in Asl.Tk.Menuitem.Button.Widget'Class) is
   begin
      GUI.Checkbutton.Set_Value(Menu_To_Set.all, False);
   end Handle;

   procedure Handle (Event : in out Radio1_Callback;
                     W     : in GUI.Radiobutton.Group'Class) is
      Button : Radio_Button_Type_Ptr := Radio_Button_Type_Ptr
        (GUI.Radiobutton.Get_Current_Button(W));
   begin
      if (Button = null) then
         Put_Line("No button selected");
      else
         Put_Line("Button" & Integer'Image(Button.Button_Num) & " selected");
      end if;
   end Handle;

end Test_Tk_Callback;
