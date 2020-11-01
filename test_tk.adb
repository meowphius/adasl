
with GUI;
with GUI.Button;
with GUI.Checkbutton;
with GUI.Radiobutton;
with GUI.Grid;
with GUI.Frame;
with GUI.Label;
with GUI.Image;
with Asl.Tk.Menubar;
with Asl.Tk.Menuitem.Cascade;
with Asl.Tk.Menuitem.Button;
with Asl.Tk.Menuitem.Checkbutton;
with Asl.Tk.Frame.Toplevel;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Tk_Callback;

procedure Test_Tk is
   My_Button    : GUI.Button.Widget_Class;
   My_Callback  : Test_Tk_Callback.Button1_Callback_Ptr;
   My_Checkbutton : GUI.Checkbutton.Widget_Class;
   My_Cb_Callback : Test_Tk_Callback.Checkbutton1_Callback_Ptr;
   My_Frame     : GUI.Managed_Class;
   My_Label     : GUI.Label.Widget_Class;
   Root_Layout  : GUI.Layout_Class := new GUI.Grid.Layout;
   Frame_Layout : GUI.Layout_Class := new GUI.Grid.Layout;
   Test_Bitmap  : GUI.Image.String_Array :=
     (new String'("#define v2_width 14"),
      new String'("#define v2_height 16"),
      new String'("static unsigned char v2_bits[] = {"),
      new String'("0x07, 0x00, 0x02, 0x00, 0x02, 0x00, 0x3a, 0x00, 0x0a,"),
      new String'("0x00, 0x38, 0x00,"),
      new String'("0x08, 0x00, 0xb8, 0x03, 0x80, 0x00, 0x80, 0x03, 0x00,"),
      new String'("0x02, 0x80, 0x3b,"),
      new String'("0x00, 0x10, 0x00, 0x10, 0x00, 0x10, 0x00, 0x10};"));
   Test_Image   : GUI.Image.Object;
   Image_Button : GUI.Button.Widget_Class;

   My_Toplevel     : GUI.Frame.Toplevel.Widget_Class;
   My_Top_Button   : GUI.Button.Widget_Class;
   Toplevel_Layout : GUI.Layout_Class := new GUI.Grid.Layout;

   Menubar1        : Asl.Tk.Menubar.Widget_Class;
   Menu1           : Asl.Tk.Menuitem.Cascade.Widget_Class;
   Menu1_Button1   : Asl.Tk.Menuitem.Button.Widget_Class;
   Menu1_Cb1       : Test_Tk_Callback.Menu1_Callback_Ptr;
   Menu1_Button2   : Asl.Tk.Menuitem.Button.Widget_Class;
   Menu1_Cb2       : Test_Tk_Callback.Menu2_Callback_Ptr;
   Menu1_Check3    : Asl.Tk.Menuitem.Checkbutton.Widget_Class;

   Radio_Group     : GUI.Radiobutton.Group;
   Radio_Button1   : GUI.Radiobutton.Widget_Class;
   Radio_Button2   : GUI.Radiobutton.Widget_Class;
   Radio_Button3   : GUI.Radiobutton.Widget_Class;
   Rb_Callback     : Test_Tk_Callback.Radio1_Callback_Ptr;
begin
   GUI.Initialize;
   GUI.Set_Layout_Manager(GUI.Root_Frame.all, Root_Layout);
   My_Frame := new GUI.Frame.Widget;
   GUI.Set_Layout_Manager(My_Frame.all, Frame_Layout);
   GUI.Set_Parent(GUI.Widget_Class(My_Frame), null);
   My_Label := new GUI.Label.Widget;
   GUI.Label.Set_Text(My_Label.all, "My label");
   GUI.Label.Set_Justify(My_Label.all, GUI.Label.Left);
   GUI.Set_Parent(GUI.Widget_Class(My_Label), My_Frame);
   GUI.Label.Set_Font(My_Label.all, GUI.To_Font("10x20"));
   My_Button := new GUI.Button.Widget;
   GUI.Button.Set_Text(My_Button.all, "My Button");
   GUI.Button.Set_Font(My_Button.all, GUI.To_Font("12x24"));
   My_Callback := new Test_Tk_Callback.Button1_Callback;
   GUI.Button.Add_Event(My_Button.all,
                        GUI.Button.Press_Event_Class(My_Callback));
   GUI.Set_Parent(GUI.Widget_Class(My_Button), My_Frame);
   My_Checkbutton := new GUI.Checkbutton.Widget;
   GUI.Checkbutton.Set_Text(My_Checkbutton.all, "Check button 1");
   My_Cb_Callback := new Test_Tk_Callback.Checkbutton1_Callback;
   GUI.Checkbutton.Add_Event(My_Checkbutton.all,
                             GUI.Checkbutton.Press_Event_Class(My_Cb_Callback));
   GUI.Set_Parent(GUI.Widget_Class(My_Checkbutton), My_Frame);
   Test_Tk_Callback.Menu_To_Set := My_Checkbutton;
   Put_Line("Label justification is: "
            & GUI.Label.Justification'Image(GUI.Label.Get_Justify
                                            (My_Label.all)));

   Test_Image := GUI.Image.Create_Bitmap(Test_Bitmap);
   Image_Button := new GUI.Button.Widget;
   GUI.Button.Set_Image(Image_Button.all, Test_Image);
   GUI.Set_Parent(GUI.Widget_Class(Image_Button), My_Frame);

   My_Toplevel := new GUI.Frame.Toplevel.Widget;
   GUI.Frame.Toplevel.Set_Name(My_Toplevel.all, "myToplevel");
   GUI.Set_Layout_Manager(GUI.Managed(My_Toplevel.all), Toplevel_Layout);
   GUI.Frame.Toplevel.Display(My_Toplevel.all);
   My_Top_Button := new GUI.Button.Widget;
   GUI.Button.Set_Text(My_Top_Button.all, "Top Button!");
   GUI.Set_Parent(GUI.Widget_Class(My_Top_Button),
                  GUI.Managed_Class(My_Toplevel));

   Menubar1 := new Asl.Tk.Menubar.Widget;
   Asl.Tk.Frame.Toplevel.Set_Menu(My_Toplevel.all, Menubar1);
   Menu1 := new Asl.Tk.Menuitem.Cascade.Widget;
   Asl.Tk.Menuitem.Cascade.Set_Text(Menu1.all, "Menu1");
   Asl.Tk.Menubar.Add_Button(Menubar1.all, Menu1);
   Menu1_Button1 := new Asl.Tk.Menuitem.Button.Widget;
   Asl.Tk.Menuitem.Button.Set_Text(Menu1_Button1.all, "menubutton1");
   Menu1_Cb1 := new Test_Tk_Callback.Menu1_Callback;
   GUI.Menuitem.Button.Add_Event
     (Menu1_Button1.all,
      GUI.Menuitem.Button.Press_Event_Class(Menu1_Cb1));
   Menu1_Button2 := new Asl.Tk.Menuitem.Button.Widget;
   Asl.Tk.Menuitem.Button.Set_Text(Menu1_Button2.all, "menubutton2");
   Menu1_Cb2 := new Test_Tk_Callback.Menu2_Callback;
   GUI.Menuitem.Button.Add_Event
     (Menu1_Button2.all,
      GUI.Menuitem.Button.Press_Event_Class(Menu1_Cb2));
   Menu1_Check3 := new Asl.Tk.Menuitem.Checkbutton.Widget;
   Asl.Tk.Menuitem.Checkbutton.Set_Text(Menu1_Check3.all, "Check menu");
   Asl.Tk.Menuitem.Cascade.Add_Button
     (Menu1.all,
      Asl.Tk.Menuitem.Widget_Class(Menu1_Button1));
   Asl.Tk.Menuitem.Cascade.Add_Button
     (Menu1.all,
      Asl.Tk.Menuitem.Widget_Class(Menu1_Button2));
   Asl.Tk.Menuitem.Cascade.Add_Button
     (Menu1.all,
      Asl.Tk.Menuitem.Widget_Class(Menu1_Check3));

   Rb_Callback := new Test_Tk_Callback.Radio1_Callback;
   GUI.Radiobutton.Add_Event(Radio_Group,
                             GUI.Radiobutton.Change_Event_Class(Rb_Callback));
   Radio_Button1 := new Test_Tk_Callback.Radio_Button_Type;
   Test_Tk_Callback.Radio_Button_Type(Radio_Button1.all).Button_Num := 1;
   GUI.Radiobutton.Set_Text(Radio_Button1.all, "radio 1");
   GUI.Radiobutton.Add_To_Group(Radio_Group, Radio_Button1);
   GUI.Set_Parent(GUI.Widget_Class(Radio_Button1),
                  GUI.Managed_Class(My_Toplevel));
   Radio_Button2 := new Test_Tk_Callback.Radio_Button_Type;
   Test_Tk_Callback.Radio_Button_Type(Radio_Button2.all).Button_Num := 2;
   GUI.Radiobutton.Set_Text(Radio_Button2.all, "radio 2");
   GUI.Radiobutton.Add_To_Group(Radio_Group, Radio_Button2);
   GUI.Set_Parent(GUI.Widget_Class(Radio_Button2),
                  GUI.Managed_Class(My_Toplevel));
   Radio_Button3 := new Test_Tk_Callback.Radio_Button_Type;
   Test_Tk_Callback.Radio_Button_Type(Radio_Button3.all).Button_Num := 3;
   GUI.Radiobutton.Set_Text(Radio_Button3.all, "radio 3");
   GUI.Radiobutton.Add_To_Group(Radio_Group, Radio_Button3);
   GUI.Set_Parent(GUI.Widget_Class(Radio_Button3),
                  GUI.Managed_Class(My_Toplevel));

   GUI.Mainloop;
   GUI.Shutdown;
   Put_Line("Done");
end Test_Tk;
