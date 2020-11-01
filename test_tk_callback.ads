
with GUI;
with GUI.Button;
with GUI.Checkbutton;
with GUI.Radiobutton;
with Asl.Tk.Menuitem.Button;

package Test_Tk_Callback is

   type Button1_Callback is new GUI.Button.Press_Event
     with null record;
   type Button1_Callback_Ptr is access all Button1_Callback;

   procedure Handle (Event : in out Button1_Callback;
                     W     : in GUI.Button.Widget'Class);

   type Checkbutton1_Callback is new GUI.Checkbutton.Press_Event
     with null record;
   type Checkbutton1_Callback_Ptr is access all Checkbutton1_Callback;

   procedure Handle (Event : in out Checkbutton1_Callback;
                     W     : in GUI.Checkbutton.Widget'Class);

   Menu_To_Set : GUI.Checkbutton.Widget_Class;

   type Menu1_Callback is new Asl.Tk.Menuitem.Button.Press_Event
     with null record;
   type Menu1_Callback_Ptr is access all Menu1_Callback;

   procedure Handle (Event : in out Menu1_Callback;
                     W     : in Asl.Tk.Menuitem.Button.Widget'Class);

   type Menu2_Callback is new Asl.Tk.Menuitem.Button.Press_Event
     with null record;
   type Menu2_Callback_Ptr is access all Menu2_Callback;

   procedure Handle (Event : in out Menu2_Callback;
                     W     : in Asl.Tk.Menuitem.Button.Widget'Class);


   type Radio_Button_Type is new GUI.Radiobutton.Widget with record
      Button_Num : Integer;
   end record;
   type Radio_Button_Type_Ptr is access all Radio_Button_Type;

   type Radio1_Callback is new GUI.Radiobutton.Change_Event
     with null record;
   type Radio1_Callback_Ptr is access all Radio1_Callback;

   procedure Handle (Event : in out Radio1_Callback;
                     W     : in GUI.Radiobutton.Group'Class);

end Test_Tk_Callback;
