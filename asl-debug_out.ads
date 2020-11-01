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

with Asgc.Hash.Dynamic;

-- This package provides a framework for handling optional debug
-- information.

package Asl.Debug_Out is

   -- This is the user-visible portion of a debug level.  The user
   -- can directly look at the enabled boolean; this allows a very
   -- efficient check of the status of the debug level.
   type User_Debug_Level is tagged limited record
      Enabled : Boolean := False;
   end record;

   -- The actual debug level.  The user should instantiate one
   -- of these, or they should descend from this to create their
   -- own debug levels.  This way, they can bind in special code
   -- to run when the debug level is enabled or diabled.
   type Debug_Level is new User_Debug_Level with private;
   type Debug_Level_Ptr is access all Debug_Level;
   type Debug_Level_Class is access all Debug_Level'Class;

   function Get_Name(Level : in Debug_Level) return String;
   procedure Enable_Debug_Level(Level : in out Debug_Level);
   procedure Disable_Debug_Level(Level : in out Debug_Level);

   -- The main class that does the debug processing.  It holds
   -- the table of debug levels.  Note that it is abstract,
   -- a real implementation must be provided that implements
   -- Output_Log() and actually does the log output.
   type Debug_Processor is abstract tagged limited private;
   type Debug_Processor_Ptr is access all Debug_Processor;
   type Debug_Processor_Class is access all Debug_Processor'Class;

   -- Raised when a debug level name is reused.
   Duplicate_Debug_Level : exception;

   -- Raised when a debug level is registered when it has already
   -- been registered.
   Debug_Level_Already_Registered : exception;

   -- Raised when a request for a debug level is made, but it does
   -- not exist.
   Invalid_Debug_Level : exception;

   procedure Output_Log(Proc  : in out Debug_Processor;
                        Level : in out Debug_Level'Class;
                        Text  : in String)
      is abstract;

   -- Add a debug level to the debug processor.
   procedure Register_Debug_Level(Proc  : in out Debug_Processor;
                                  Name  : in String;
                                  Level : access Debug_Level'Class);

   -- Get a debug level by name.  If the level is not in the processor,
   -- the Level will be set to null;
   procedure Get_Debug_Level(Proc  : in out Debug_Processor;
                             Name  : access String;
                             Level : out Debug_Level_Class);

   -- Enable a debug level by name.  If the level is not in the processor,
   -- this throws Invalid_Debug_Level.
   procedure Enable_Debug_Level(Proc : in out Debug_Processor;
                                Name : access String);

   -- Disable a debug level by name.  If the level is not in the processor,
   -- this throws Invalid_Debug_Level.
   procedure Disable_Debug_Level(Proc : in out Debug_Processor;
                                 Name : access String);

private

   type String_Ptr is access all String;

   type Debug_Level is new User_Debug_Level with record
      Name : String_Ptr := null;
      Proc : Debug_Processor_Class;
   end record;

   function Is_Equal(V1, V2 : in Debug_Level_Class)
                     return Boolean;

   function Hash(V : in Debug_Level_Class) return Natural;


   package Debug_Asgc is new Asgc(Contained_Type => Debug_Level_Class,
                                  "="            => Is_Equal);
   package Debug_Hash_Parent is new Debug_Asgc.Hash(Do_Hash => Hash);
   package Debug_Hash is new Debug_Hash_Parent.Dynamic;
   use type Debug_Asgc.End_Marker;

   protected type Protected_Debug_Table is
      procedure Register_Debug(Name  : in String;
                               Level : in Debug_Level_Class);

      procedure Get_Debug(Name  : access String;
                          Level : out Debug_Level_Class);

      procedure Destroy;
   private
      T : Debug_Hash.Object_Ptr
        := new Debug_Hash.Object(Allow_Duplicates => False,
                                 Size             => 1000);
   end Protected_Debug_Table;


   type Debug_Processor is abstract tagged limited record
      Table : Protected_Debug_Table;
      Self  : Debug_Processor_Class;
   end record;

end Asl.Debug_Out;
