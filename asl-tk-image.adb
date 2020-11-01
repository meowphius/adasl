
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Asl.Tk.Image is

   procedure Free_Natural is new Ada.Unchecked_Deallocation(Natural,
                                                            Natural_Ptr);

   ------------------------------------------------------------------------
   function Create_Bitmap (File           : in String;
                           Set_Background : in Boolean    := False;
                           Background     : in Color_Attr := Black_Color;
                           Set_Foreground : in Boolean    := False;
                           Foreground     : in Color_Attr := White_Color;
                           Maskfile       : in String     := "")
                           return Object is
      Str    : ASU.Unbounded_String;
      Rv     : C.Int;
      Retval : Object;
   begin
      Str := ASU.To_Unbounded_String("image create bitmap -file " & File);

      if (Set_Background) then
         Str := Str & " -background " & Background.Name;
      end if;

      if (Set_Foreground) then
         Str := Str & " -foreground " & Foreground.Name;
      end if;

      if (Maskfile /= "") then
         Str := Str & " -maskfile " & Maskfile;
      end if;

      Rv := Asl.Tcl.Tcl_Eval(Interp, Str);

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      Retval.Name := ASU.To_Unbounded_String
        (CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));

      return Retval;
   end Create_Bitmap;


   ------------------------------------------------------------------------
   function Create_Bitmap (Bitmap         : in String_Array;
                           Set_Background : in Boolean      := False;
                           Background     : in Color_Attr   := Black_Color;
                           Set_Foreground : in Boolean      := False;
                           Foreground     : in Color_Attr   := White_Color;
                           Mask           : in String_Array := Dummy_Mask)
                           return Object is
      Str    : ASU.Unbounded_String;
      Rv     : C.Int;
      Retval : Object;
   begin
      Str := ASU.To_Unbounded_String("image create bitmap -data """);

      for I in Bitmap'Range loop
         Str := Str & Bitmap(I).all & Ada.Characters.Latin_1.LF;
      end loop;

      Str := Str & """";

      if (Set_Background) then
         Str := Str & " -background " & Background.Name;
      end if;

      if (Set_Foreground) then
         Str := Str & " -foreground " & Foreground.Name;
      end if;

      if (Mask /= Dummy_Mask) then
         Str := Str & " -maskdata ";
         for I in Mask'Range loop
            Str := Str & Mask(I).all;
         end loop;
      end if;

      Rv := Asl.Tcl.Tcl_Eval(Interp, Str);

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      Retval.Name := ASU.To_Unbounded_String
        (CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));

      return Retval;
   end Create_Bitmap;


   ------------------------------------------------------------------------
   function Create_Photo (File  : in String;
                          Gamma : in Float := 1.0)
                          return Object is
      Str    : ASU.Unbounded_String;
      Rv     : C.Int;
      Retval : Object;
   begin
      Str := ASU.To_Unbounded_String("image create bitmap -file " & File);

      if (Gamma /= 1.0) then
         Str := Str & " -gamma " & Float'Image(Gamma);
      end if;

      Rv := Asl.Tcl.Tcl_Eval(Interp, Str);

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      Retval.Name := ASU.To_Unbounded_String
        (CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));

      return Retval;
   end Create_Photo;


   ------------------------------------------------------------------------
   -- Takes a base-64 encoded array of strings.
   function Create_Photo (Data  : in String_Array;
                          Gamma : in Float := 1.0)
                          return Object is
      Str    : ASU.Unbounded_String;
      Rv     : C.Int;
      Retval : Object;
   begin
      Str := ASU.To_Unbounded_String("image create bitmap -data """);

      for I in Data'Range loop
         Str := Str & Data(I).all;
      end loop;

      Str := Str & """";

      if (Gamma /= 1.0) then
         Str := Str & " -gamma " & Float'Image(Gamma);
      end if;

      Rv := Asl.Tcl.Tcl_Eval(Interp, Str);

      if (Rv /= Asl.Tcl.TCL_OK) then
         Ada.Exceptions.Raise_Exception
           (Widget_Error'Identity,
            CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));
      end if;

      Retval.Name := ASU.To_Unbounded_String
        (CStr.Value(Asl.Tcl.Tcl_GetStringResult(Interp)));

      return Retval;
   end Create_Photo;


   ------------------------------------------------------------------------
   procedure Finalize (O : in out Object) is
      Rv : C.Int;
   begin
      O.Refcount.all := O.Refcount.all - 1;
      if (O.Refcount.all = 0) then
         Free_Natural(O.Refcount);
         Rv := Asl.Tcl.Tcl_Eval(Interp,
                                "image delete " & O.Name);
      end if;
   end Finalize;


   ------------------------------------------------------------------------
   procedure Adjust (O : in out Object) is
   begin
      O.Refcount.all := O.Refcount.all + 1;
   end Adjust;


   ------------------------------------------------------------------------
   function Attribute (O : in Object)
                       return String is
   begin
      return ASU.To_String(O.Name);
   end Attribute;

end Asl.Tk.Image;
