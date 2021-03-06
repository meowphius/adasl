--------------------------------------------------------------------
--
-- cargv.adb -- 
--
-- Copyright (c) 1995-1997 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
-- 01-Sep-97   TJW   Fixed dangling pointer problem independently
--                   discovered by me and Brett Kettering (thanks, Brett!).
--
--------------------------------------------------------------------

with Ada.Command_Line;
with Text_IO;

package body CArgv is

   -- To make operators visible:
   use type C.Int;
   use type C.Strings.Chars_Ptr;

   type Vector_Access is access Vector;

   Empty_Vector : Vector := (0 => C.Strings.Null_Ptr);

   procedure Create (Argc : out C.Int; Argv : out Chars_Ptr_Ptr) is
      Size : constant C.Int := C.Int (Ada.Command_Line.Argument_Count+1);
      Vec  : Vector_Access  := new Vector (0..Size);
   begin -- Create
      Vec (0) := C.Strings.New_String (Ada.Command_Line.Command_Name);
      for i in 1..Size-1 loop
         Vec (i) := C.Strings.New_String (
	    Ada.Command_Line.Argument (Integer(i)));
      end loop;
      Vec (Size) := C.Strings.Null_Ptr;
      Argc := Size;
      Argv := Vec (Vec'first)'unchecked_access;
   end Create;

   procedure Show (Argc : in C.Int; Argv : in Chars_Ptr_Ptr) is
      Ptr : Chars_Ptr_Ptr := Argv;
   begin -- Show
      Text_IO.Put_Line ("Argc :" & C.Int'Image (Argc));
      for i in 0..Argc - 1 loop
	 Text_IO.Put (C.Int'Image (I) & " : ");
         if Ptr.all = C.Strings.Null_Ptr then
	    Text_IO.Put_Line ("<null-ptr>");
	 else
	    Text_IO.Put_Line (C.Strings.Value (Ptr.all));
	 end if;
         Argv_Pointer.Increment (Ptr);
      end loop;
   end Show;

   procedure Free (Argv : in out Chars_Ptr_Ptr) is
      Ptr : Chars_Ptr_Ptr := Argv;
   begin -- Free
      while Ptr.all /= C.Strings.Null_Ptr loop
         C.Strings.Free (Ptr.all);
         Argv_Pointer.Increment (Ptr);
      end loop;
      Free (Argv);
   end Free;

   function Empty return Chars_Ptr_Ptr is
   begin -- Empty
      return Empty_Vector (Empty_Vector'first)'unchecked_access;
   end Empty;

   function "&" (Argv : Chars_Ptr_Ptr; Arg : String) return Chars_Ptr_Ptr is
      Size : C.Int := C.Int (Argv_Pointer.Virtual_Length (Argv)) + 1;
      Vec  : Vector_Access := new Vector (0..Size);
   begin -- "&"
      Vec (0..Size-1) := Argv_Pointer.Value (Argv);
      Vec (Size)      := C.Strings.New_String (Arg);
      return Vec (Vec'first)'unchecked_access;
   end "&";

   function Argc (Argv : in Chars_Ptr_Ptr) return C.Int is
   begin -- Argc
      return C.Int (Argv_Pointer.Virtual_Length (Argv));
   end Argc;

end CArgv;
