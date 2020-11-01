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

with Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with Asl.Security;
use type Asl.Security.Verify_User_Class;

package body Asl.Cmdproc is

   procedure Free is new Unchecked_Deallocation(String, String_Ptr);

   procedure Free is new Unchecked_Deallocation(Command_Hash.Object,
                                                Command_Hash.Object_Ptr);

   procedure Free is new Unchecked_Deallocation(Help_Command,
                                                Help_Command_Ptr);

   procedure Free is new Unchecked_Deallocation(Exit_Command,
                                                Exit_Command_Ptr);

   procedure Handle_Input_Line(P    : in out Command_Processor'Class;
                               Line : in String) is
      Tok   : Tokenized_String;
      Cmd   : Command_Class;
      Found : Boolean := False;
   begin
      Initialize(Tok, Line);
      Move_To_Next_Token(Tok);
      if (Current_Token_Length(Tok) = 0) then
         return;
      end if;

      declare
         Name : aliased String := Current_Token(Tok);
      begin
         Get_Command(P.Table.all, Name'Unchecked_Access, Cmd);
         if (Cmd = null) then
            Put_Line(P.IO.all, "Command '" & Name & "' not found");
         else
            begin
               Move_To_Next_Token(Tok);
               Execute(Cmd.all, Name, P, P.IO.all, Tok);
            exception
               when E : others =>
                  Put_Line(P.IO.all, "Command raised exception:");
                  Put_Line(P.IO.all, Exception_Information(E));
            end;
         end if;
      end;
   end Handle_Input_Line;

   procedure Output_Prompt(P : in Command_Processor'Class) is
   begin
      Put(P.IO.all, "=> ");
   end Output_Prompt;

   procedure Command_Loop(T   : in Command_Table_Class;
                          IO  : in Abstract_File_Class;
                          Sec : in Asl.Security.Verify_User_Class) is
      P      : Command_Processor;
      Buffer : String(1 .. 1024);
      Pos    : Integer := Buffer'First;
      Ch     : Character;
      Permit : Boolean;
   begin
      P.IO := IO;
      P.Table := T;
      Buffer(1) := ' '; -- Keep GNAT from complaining
      P.Do_Exit := False;

      if (Sec /= null) then
         Asl.Security.Verify_Access(Sec.all, IO.all, Permit);
         if not Permit then
            Put_Line(IO.all, "Permission denied");
            return;
         end if;
      end if;

      Output_Prompt(P);
      while not P.Do_Exit loop
         Get(P.IO.all, Ch);
         if ((Ch = Ada.Characters.Latin_1.BS)
             or (Ch = Ada.Characters.Latin_1.DEL))
         then
            -- It's a backspace.
            if (Pos > 1) then
               Put(P.IO.all, Ada.Characters.Latin_1.BS);
               Put(P.IO.all, ' ');
               Put(P.IO.all, Ada.Characters.Latin_1.BS);
               Pos := Pos - 1;
            end if;
         elsif End_Of_Line(P.IO.all, Ch) then
            New_Line(P.IO.all);
            Handle_Input_Line(P, Buffer(1 .. Pos-1));
            Output_Prompt(P);
            Pos := Buffer'First;
         else
            Put(P.IO.all, Ch);
            if (Pos > Buffer'Last) then
               Put_Line(P.IO.all, "Line too long");
               Pos := 1;
            end if;
            Buffer(Pos) := Ch;
            Pos := Pos + 1;
         end if;
      end loop;
   end Command_Loop;

   protected body Protected_Command_Table is
      procedure Register_Command(Name : in String;
                                 Cmd  : in Command_Class) is
         New_Command : Command_Holder;
      begin
         if (Name'Length = 0) then
            raise Constraint_Error;
         end if;

         if (Name'Length > Max_Namesize) then
            Max_Namesize := Name'Length;
         end if;

         New_Command.Name := new String'(Name);
         New_Command.Cmd  := Cmd;
         begin
            Command_Hash.Add(T.all, New_Command);
         exception
            when others =>
               Free(New_Command.Name);
               raise Duplicate_Command;
         end;
      end Register_Command;

      procedure Get_Command(Name : access String;
                            Cmd  : out Command_Class) is
         Iter   : Command_Hash.Iterator;
         Tmp    : Command_Holder;
         Found  : Boolean := False;
      begin
         Iter := Command_Hash.New_Iterator(Command_Hash.Object_Class(T));
         Tmp.Name := String_Ptr(Name);
         Command_Hash.Search(Iter, Tmp, Found);
         if (not Found) then
            Cmd := null;
         else
            Tmp := Command_Hash.Get(Iter);
            Cmd := Tmp.Cmd;
         end if;
      end Get_Command;

      procedure Put_Names(IO     : in out Abstract_File'Class;
                          Indent : in Integer) is
         Iter   : Command_Hash.Iterator;
         Tmp    : Command_Holder;
         Found  : Boolean := False;
         Is_End : Command_Asgc.End_Marker;
         Fill   : String(1 .. Max_Namesize);
         Count  : Integer := 0;
      begin
         if (Max_Namesize = 0) then
            return;
         end if;

         for I in Fill'Range loop
            Fill(I) := ' ';
         end loop;

         Iter := Command_Hash.New_Iterator(Command_Hash.Object_Class(T));
         Command_Hash.First(Iter, Is_End);
         while (Is_End = Command_Asgc.Not_Past_End) loop
            Tmp := Command_Hash.Get(Iter);
            Put_Wrapped(IO, Fill(1 .. Count+1) & Tmp.Name.all,
                        Left_Indent => Indent);
            Count := Max_Namesize - Tmp.Name'Length;
            Command_Hash.Next(Iter, Is_End);
         end loop;
         New_Line(IO);
      end Put_Names;

      procedure Destroy is
      begin
         Free(T);
      end Destroy;
   end Protected_Command_Table;

   procedure Register_Command(T    : in out Command_Table;
                              Name : in String;
                              Cmd  : in Command_Class) is
   begin
      T.Table.Register_Command(Name, Cmd);
   end Register_Command;

   procedure Get_Command(T    : in out Command_Table;
                         Name : access String;
                         Cmd  : out Command_Class) is
   begin
      T.Table.Get_Command(Name, Cmd);
   end Get_Command;

   procedure Destroy(T : in out Command_Table) is
   begin
      T.Table.Destroy;
   end Destroy;

   function Is_Equal(V1, V2 : in Command_Holder)
                     return Boolean is
   begin
      return ((V1.Name'Length = V2.Name'Length)
              and then (V1.Name.all = V2.Name.all));
   end Is_Equal;

   function Hash(V : in Command_Holder) return Natural is
      type Hasher is mod 2 ** (Natural'Size - 1);
      Val : Hasher := 0;
   begin
      for I in V.Name'Range loop
         Val := (Val * 67) * (Character'Pos(V.Name(I)) * 43);
      end loop;

      return Natural(Val);
   end Hash;

   function Get_Command_Table(P : in Command_Processor)
                              return Command_Table_Class is
   begin
      return P.Table;
   end Get_Command_Table;

   function Get_Abstract_File(P : in Command_Processor)
                              return Abstract_File_Class is
   begin
      return P.IO;
   end Get_Abstract_File;

   procedure Execute(Cmd  : in out Help_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class) is
      Size : Integer := Current_Token_Length(Tok);
   begin
      if (Size = 0) then
         Put_Line(IO, "Valid commands are:");
         Proc.Table.Table.Put_Names(IO, 2);
         return;
      end if;

      declare
         Name    : aliased String := Current_Token(Tok);
         To_Help : Command_Class;
      begin
         Get_Command(Proc.Table.all, Name'Unchecked_Access, To_Help);
         if (To_Help = null) then
            Put_Line(Proc.IO.all, "Command '" & Name & "' not found");
         else
            Help(To_Help.all, Name, Proc, Proc.IO.all, Tok);
         end if;
      end;
   end Execute;

   procedure Help(Cmd  : in out Help_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class) is
   begin
      Put_Wrapped(IO,
                  "Help [command [parms]] - Display help for the given"
                  & " command and parameters.  If no command is given,"
                  & " then a list of all the commands is printed");
      New_Line(IO);
   end Help;


   procedure Execute(Cmd  : in out Exit_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class) is
   begin
      Proc.Do_Exit := True;
   end Execute;

   procedure Help(Cmd  : in out Exit_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class) is
   begin
      Put_Wrapped(IO, "Exit - Leave the command interpreter");
      New_Line(IO);
   end Help;

   procedure Initialize(Item : in out Command_Table) is
   begin
      Register_Command(Item, "help", Command_Class(Item.Help_Cmd));
      Register_Command(Item, "exit", Command_Class(Item.Exit_Cmd));
   end Initialize;

   procedure Finalize(Item : in out Command_Table) is
   begin
      Item.Table.Destroy;
      Free(Item.Help_Cmd);
      Free(Item.Exit_Cmd);
   end Finalize;

end Asl.Cmdproc;
