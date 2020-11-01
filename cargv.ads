--------------------------------------------------------------------
--
-- cargv.ads -- Create C-style "argv" vectors from strings and
--              Ada.Command_Line.
--
-- Copyright (c) 1995-1997 Terry J. Westley
--
-- See the file "license.htm" for information on usage and
-- redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
--
-- This package provides the data type Chars_Ptr_Ptr which corresponds
-- to the char** of C and subprograms for creating and manipulating
-- arrays of C strings.
--
--------------------------------------------------------------------

with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package CArgv is

   package C renames Interfaces.C;

   subtype CNatural is C.Int range 0..C.Int'last;

   type Vector is array (CNatural range <>) of aliased C.Strings.Chars_Ptr;
   -- This is a C-style "argv" vector.

   package Argv_Pointer is new C.Pointers (
      Index              => CNatural,
      Element            => C.Strings.Chars_Ptr,
      Element_Array      => Vector,
      Default_Terminator => C.Strings.Null_Ptr);

   subtype Chars_Ptr_Ptr is Argv_Pointer.Pointer;
   -- This is C char **

   ---------------------------------------------------------------------
   --
   -- The following subprograms support converting command line arguments
   -- to C-style argc/argv command line arguments.
   --
   ---------------------------------------------------------------------

   procedure Create (Argc : out C.Int; Argv : out Chars_Ptr_Ptr);
   -- Create returns the command line arguments from Ada.Command_Line 
   -- and converts them to a C-style, null-terminated argument vector.

   procedure Show (Argc : in C.Int; Argv : in Chars_Ptr_Ptr);
   -- Prints Argc and Argv to standard out.

   procedure Free (Argv : in out Chars_Ptr_Ptr);
   -- Free all space used by Vec.

   -- Example of getting Ada command line arguments and passing them
   -- to a C function requiring argc/argv arguments:
   --
   --    declare
   --       Argc : C.Int;
   --       Argv : CArgv.Chars_Ptr_Ptr;
   --    begin
   --       CArgv.Create (Argc, Argv);
   --       Tcl.Tcl_Concat (Argc, Argv);
   --       CArgv.Free (Argv);
   --    end;

   ---------------------------------------------------------------------
   --
   -- The following subprograms support creating C-style argc/argv
   -- argument vectors from strings.
   --
   ---------------------------------------------------------------------

   function Empty return Chars_Ptr_Ptr;
   -- An empty Chars_Ptr_Ptr, used for constructors.

   function "&" (Argv : Chars_Ptr_Ptr; Arg : String) return Chars_Ptr_Ptr;
   -- Construct a Chars_Ptr_Ptr using concat operation.

   function Argc (Argv : in Chars_Ptr_Ptr) return C.Int;
   -- Returns the number of arguments in a Chars_Ptr_Ptr.

   -- Example of creating a Chars_Ptr_Ptr to pass to a C function requiring
   -- argc/argv arguments:
   --
   --    declare
   --       Argv : CArgv.Chars_Ptr_Ptr := 
   --          Empty & "this" & "is" & "four" & "arguments";
   --    begin
   --       Tcl.Tcl_Concat (CArgv.Argc (Argv), Argv);
   --       CArgv.Free (Argv);
   --    end;

end CArgv;