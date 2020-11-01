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

with Asl.Cmdproc; use Asl.Cmdproc;
with Asl.Strings.Tokenizer; use Asl.Strings.Tokenizer;
with Asl.Abstract_IO; use Asl.Abstract_IO;


-- A cmdproc command that handles debug levels.

package Asl.Debug_Out.Command is

   type Level_Command is new Asl.Cmdproc.Command with private;
   type Level_Command_Ptr is access all Level_Command;

   -- Assign the debug processor to use with this.
   procedure Initialize(Cmd   : in out Level_Command;
                        Debug : in Debug_Processor_Class);

   procedure Execute(Cmd  : in out Level_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class);

   procedure Help(Cmd  : in out Level_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class);

private

   type Level_Command is new Asl.Cmdproc.Command with record
      Debug : Debug_Processor_Class;
   end record;

end Asl.Debug_Out.Command;
