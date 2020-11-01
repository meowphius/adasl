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

with Asl.Strings.Tokenizer; use Asl.Strings.Tokenizer;
with Asl.Abstract_IO; use Asl.Abstract_IO;
with Asgc.Hash.Dynamic;
with Asl.Security;
with Ada.Finalization; use Ada.Finalization;

-- This package provides a command processor over and abstract I/O
-- class.  The user can register commands in a command table
-- with a specific name.  Then the user can create a command
-- processor with the command table.  Whenever the command processor
-- sees the specific command name registered in the table, it calls
-- the command registered against it.

package Asl.Cmdproc is

   -- This is a table that holds commands to be executed.  It may be
   -- used by multiple command processors.
   type Command_Table is new Limited_Controlled with private;
   type Command_Table_Ptr is access all Command_Table;
   type Command_Table_Class is access all Command_Table'Class;

   -- The actual command processor that executes the commands.
   type Command_Processor is tagged limited private;
   type Command_Processor_Ptr is access all Command_Processor;
   type Command_Processor_Class is access all Command_Processor'Class;

   -- The user commands must descend from this type
   type Command is abstract tagged limited private;
   type Command_Class is access all Command'Class;

   -- The user must implement this procedure to actually execute the
   -- command.  Tok contains the command string, see Asl.Strings.Tokenizer
   -- for details on how that work.  See Asl.Abstract_IO for information
   -- on the IO parameter.  The actual name of the command type is is
   -- passed in as Name.
   procedure Execute(Cmd  : in out Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class)
      is abstract;

   -- Like the Execute command, but the user type "help <command>".  This
   -- should print some type of help.  Note that user can type in
   -- parameters after the command, so that the can ask for help on
   -- specific subcommand or parameters.  If they type "help <command>"
   -- without any parameters, the code should print out genera help
   -- on the command and how to find out details with more specific
   -- help commands.
   procedure Help(Cmd  : in out Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class)
      is abstract;


   -- Raised if the same name is registered twice in the command table.
   Duplicate_Command : exception;

   -- Add a command to the command table.
   procedure Register_Command(T    : in out Command_Table;
                              Name : in String;
                              Cmd  : in Command_Class);

   -- Find the command in the command table.
   procedure Get_Command(T    : in out Command_Table;
                         Name : access String;
                         Cmd  : out Command_Class);



   -- Get the command table associated with the command processor.
   function Get_Command_Table(P : in Command_Processor)
                              return Command_Table_Class;

   -- Get the abstract I/O associated with the command processor.
   function Get_Abstract_File(P : in Command_Processor)
                              return Abstract_File_Class;


   -- This starts reading input and processing commands.  It will return
   -- when done (the user exits).  If Sec is not null, then the Sec
   -- parameter is used to verify access;
   procedure Command_Loop(T   : in Command_Table_Class;
                          IO  : in Abstract_File_Class;
                          Sec : in Asl.Security.Verify_User_Class);

private

   type Command is abstract tagged limited null record;

   type Help_Command is new Command with null record;
   type Help_Command_Ptr is access all Help_Command;

   procedure Execute(Cmd  : in out Help_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class);

   procedure Help(Cmd  : in out Help_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class);


   type Exit_Command is new Command with null record;
   type Exit_Command_Ptr is access all Exit_Command;

   procedure Execute(Cmd  : in out Exit_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class);

   procedure Help(Cmd  : in out Exit_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class);


   type String_Ptr is access all String;

   type Command_Holder is record
      Cmd  : Command_Class;
      Name : String_Ptr;
   end record;

   function Is_Equal(V1, V2 : in Command_Holder)
                     return Boolean;

   function Hash(V : in Command_Holder) return Natural;

   package Command_Asgc is new Asgc(Contained_Type => Command_Holder,
                                    "="            => Is_Equal);
   package Command_Hash_Parent is new Command_Asgc.Hash(Do_Hash => Hash);
   package Command_Hash is new Command_Hash_Parent.Dynamic;
   use type Command_Asgc.End_Marker;


   protected type Protected_Command_Table is
      procedure Register_Command(Name : in String;
                                 Cmd  : in Command_Class);

      procedure Get_Command(Name : access String;
                            Cmd  : out Command_Class);

      procedure Put_Names(IO     : in out Abstract_File'Class;
                          Indent : in Integer);

      procedure Destroy;
   private
      T : Command_Hash.Object_Ptr
        := new Command_Hash.Object(Allow_Duplicates => False,
                                   Size             => 100);
      Max_Namesize : Integer := 0;
   end Protected_Command_Table;

   type Command_Table is new Limited_Controlled with record
      Table    : Protected_Command_Table;
      Help_Cmd : Help_Command_Ptr := new Help_Command;
      Exit_Cmd : Exit_Command_Ptr := new Exit_Command;
   end record;
   procedure Initialize(Item : in out Command_Table);
   procedure Finalize(Item : in out Command_Table);


   type Command_Processor is tagged limited record
      IO      : Abstract_File_Class;
      Table   : Command_Table_Class;
      Do_Exit : Boolean := False;
   end record;

end Asl.Cmdproc;
