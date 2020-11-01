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

with Ada.Streams; use Ada.Streams;
with Sockets; use Sockets;
with Asl.Abstract_IO; use Asl.Abstract_IO;
with Asl.Abstract_IO.Telnet; use Asl.Abstract_IO.Telnet;
with Asl.Cmdproc; use Asl.Cmdproc;
with Asl.Security;
use type Asl.Security.Verify_User_Class;
with Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Asgc.List.Dynamic;
with Asl.Debug_Out; use Asl.Debug_Out;
with Asl.Debug_Out.Command; use Asl.Debug_Out.Command;
with Ada.Exceptions; use Ada.Exceptions;

package body Asl.Cmdproc.Telnet is

   type String_Ptr is access all String;

   procedure Free is new Unchecked_Deallocation(String, String_Ptr);

   type Local_Debug_Processor;
   type Local_Debug_Processor_Ptr is access all Local_Debug_Processor;

   type Processor;
   type Processor_Ptr is access all Processor;
   task type Processor is
      entry Start(Login_Text : in String_Ptr;
                  S          : in Socket_FD;
                  Sec        : in Asl.Security.Verify_User_Class;
                  Cmd        : in Command_Table_Ptr;
                  Self_Ptr   : in Processor_Ptr);
   end Processor;

   type Debug_Out;
   type Debug_Out_Ptr is access all Debug_Out;
   task type Debug_Out is
      entry Start(Login_Text : in String_Ptr;
                  S          : in Socket_FD;
                  Sec        : in Asl.Security.Verify_User_Class;
                  Self_Ptr   : in Debug_Out_Ptr;
                  Debug      : in Local_Debug_Processor_Ptr);
   end Debug_Out;

   procedure Free is new Unchecked_Deallocation(Processor, Processor_Ptr);
   procedure Free is new Unchecked_Deallocation(Debug_Out, Debug_Out_Ptr);

   type Debug_Out_Rec is record
      Runner : Debug_Out_Ptr;
      IO     : Abstract_File_Class;
   end record;

   package Debug_Out_Base is new Asgc(Contained_Type => Debug_Out_Rec);
   package Debug_Out_Base_List is new Debug_Out_Base.List;
   package Debug_Out_List is new Debug_Out_Base_List.Dynamic;
   use Debug_Out_List;
   use type Debug_Out_Base.End_Marker;

   protected type Protected_Debug_List is
      procedure Add_Item(Runner : in Debug_Out_Ptr;
                         IO     : in Abstract_File_Class);
      procedure Remove_Item(Runner : in Debug_Out_Ptr);
      procedure Gen_Out(Text : String);
      procedure Initialize;
   private
      L : aliased Debug_Out_List.Object;
      L_Ptr : Debug_Out_List.Object_Ptr;
   end Protected_Debug_List;

   protected body Protected_Debug_List is
      procedure Add_Item(Runner : in Debug_Out_Ptr;
                         IO     : in Abstract_File_Class) is
         Rec : Debug_Out_Rec;
      begin
         Rec.Runner := Runner;
         Rec.IO := IO;
         Debug_Out_List.Add_Tail(L, Rec);
      end Add_Item;

      procedure Remove_Item(Runner : in Debug_Out_Ptr) is
         Rec    : Debug_Out_Rec;
         Iter   : Debug_Out_List.Iterator;
         Is_End : Debug_Out_Base.End_Marker;
      begin
         Iter := New_Iterator(Object_Class(L_Ptr));
         First(Iter, Is_End);
         while (Is_End = Debug_Out_Base.Not_Past_End) loop
            Rec := Get(Iter);
            if (Rec.Runner = Runner) then
               Delete(Iter, Is_End);
               exit;
            end if;
            Next(Iter, Is_End);
         end loop;
      end Remove_Item;

      procedure Gen_Out(Text : in String) is
         Rec    : Debug_Out_Rec;
         Iter   : Debug_Out_List.Iterator;
         Is_End : Debug_Out_Base.End_Marker;
      begin
         Iter := New_Iterator(Object_Class(L_Ptr));
         First(Iter, Is_End);
         while (Is_End = Debug_Out_Base.Not_Past_End) loop
            Rec := Get(Iter);
            Put_Line(Rec.IO.all, Text);
            Next(Iter, Is_End);
         end loop;
      end Gen_Out;

      procedure Initialize is
      begin
         L_Ptr := L'Unchecked_Access;
      end Initialize;
   end Protected_Debug_List;

   type Local_Debug_Processor is new Asl.Debug_Out.Debug_Processor with record
      Outlist : Protected_Debug_List;
   end record;

   procedure Output_Log(Proc  : in out Local_Debug_Processor;
                        Level : in out Debug_Level'Class;
                        Text  : in String) is
   begin
      if (Level.Enabled) then
         Proc.Outlist.Gen_Out(Get_Name(Level) & ": " & Text);
      end if;
   end Output_Log;

   task Free_Me_Task is
      entry Free_Proc(P : Processor_Ptr);
      entry Free_Debug(P : Debug_Out_Ptr);
   end Free_Me_Task;

   task body Free_Me_Task is
      To_Free_Proc  : Processor_Ptr := null;
      To_Free_Debug : Debug_Out_Ptr := null;
   begin
      loop
         select
            accept Free_Proc(P : Processor_Ptr) do
               To_Free_Proc := P;
            end Free_Proc;

         or

            accept Free_Debug(P : Debug_Out_Ptr) do
               To_Free_Debug := P;
            end Free_Debug;
         or
            terminate;
         end select;

         if (To_Free_Proc /= null) then
            Free(To_Free_Proc);
            To_Free_Proc := null;
         end if;
         if (To_Free_Debug /= null) then
            Free(To_Free_Debug);
            To_Free_Debug := null;
         end if;
      end loop;
   end Free_Me_Task;

   task body Debug_Out is
      Socket        : Socket_FD;
      File          : aliased Telnet_Abstract_File;
      Ch            : Character;
      File_Ptr      : Telnet_Abstract_File_Ptr := File'Unchecked_Access;
      Security      : Asl.Security.Verify_User_Class;
      At_Login      : String_Ptr;
      Self          : Debug_Out_Ptr;
      Debug_List    : Local_Debug_Processor_Ptr;
      Permit        : Boolean := True;
   begin
      select
         accept Start(Login_Text : in String_Ptr;
                      S          : in Socket_FD;
                      Sec        : in Asl.Security.Verify_User_Class;
                      Self_Ptr   : in Debug_Out_Ptr;
                      Debug      : in Local_Debug_Processor_Ptr)
         do
            Socket := S;
            Security := Sec;
            At_Login := Login_Text;
            Self := Self_Ptr;
            Debug_List := Debug;
         end Start;

      or
         terminate;
      end select;

      begin
         Initialize(File_Ptr, Socket);
         Put_Wrapped(Abstract_File'Class(File_Ptr.all), At_Login.all);
         New_Line(Abstract_File'Class(File_Ptr.all));

         if (Security /= null) then
            Asl.Security.Verify_Access(Security.all, File_Ptr.all, Permit);
         end if;

         Put_Line(Abstract_File'Class(File_Ptr.all),
                  "Enter ctrl-D to exit");

         if not Permit then
            Put_Line(File_Ptr.all, "Permission denied");
         else
            Debug_List.Outlist.Add_Item(Self, Abstract_File_Class(File_Ptr));
            loop
               Get(File, Ch);
               exit when (Ch = Ada.Characters.Latin_1.EOT);
            end loop;
         end if;
      exception
         when others => null;
      end;
      Debug_List.Outlist.Remove_Item(Self);
      Shutdown(Socket);
      Free_Me_Task.Free_Debug(Self);
   end Debug_Out;


   task type Telnet_Debug_Listener_Task is
      entry Start(Login_Text : in String_Ptr;
                  Port       : in Integer;
                  Sec        : in Asl.Security.Verify_User_Class;
                  Debug_Proc : out Asl.Debug_Out.Debug_Processor_Class);
   end Telnet_Debug_Listener_Task;

   task body Telnet_Debug_Listener_Task is
      Security       : Asl.Security.Verify_User_Class;
      Listen_Socket  : Socket_FD;
      Data_Socket    : Socket_FD;
      Debug_List     : Local_Debug_Processor_Ptr := new Local_Debug_Processor;
      Tmp_Debug_Out  : Debug_Out_Ptr;
      Login_Text_Ptr : String_Ptr;
   begin
      select
         accept Start(Login_Text : in String_Ptr;
                      Port       : in Integer;
                      Sec        : in Asl.Security.Verify_User_Class;
                      Debug_Proc : out Asl.Debug_Out.Debug_Processor_Class)
         do
            Login_Text_Ptr := Login_Text;
            Socket(Listen_Socket, AF_INET, SOCK_STREAM);
            Setsockopt(Listen_Socket, SOL_SOCKET, SO_REUSEADDR, 0);
            Bind(Listen_Socket, Port);
            Listen(Listen_Socket);
            Debug_Proc := Asl.Debug_Out.Debug_Processor_Class(Debug_List);

            Security := Sec;
         end Start;
      or
         terminate;
      end select;

      Debug_List.Outlist.Initialize;
      loop
         Accept_Socket(Listen_Socket, Data_Socket);
         Tmp_Debug_Out := new Debug_Out;
         begin
            Tmp_Debug_Out.Start(Login_Text_Ptr, Data_Socket, Security,
                                Tmp_Debug_Out, Debug_List);
         exception
            when others =>
               Free(Tmp_Debug_Out);
         end;
      end loop;
   end Telnet_Debug_Listener_Task;


   task body Processor is
      Socket        : Socket_FD;
      File          : aliased Telnet_Abstract_File;
      Data          : Ada.Streams.Stream_Element_Array(1 .. 100);
      Last          : Ada.Streams.Stream_Element_Offset;
      File_Ptr      : Telnet_Abstract_File_Ptr := File'Unchecked_Access;
      Commands_Ptr  : Command_Table_Ptr;
      Security      : Asl.Security.Verify_User_Class;
      At_Login      : String_Ptr;
      Self          : Processor_Ptr;
   begin
      select
         accept Start(Login_Text : in String_Ptr;
                      S          : in Socket_FD;
                      Sec        : in Asl.Security.Verify_User_Class;
                      Cmd        : in Command_Table_Ptr;
                      Self_Ptr   : in Processor_Ptr)
         do
            Socket := S;
            Commands_Ptr := Cmd;
            Security := Sec;
            At_Login := Login_Text;
            Self := Self_Ptr;
         end Start;

      or
         terminate;
      end select;

      begin
         Initialize(File_Ptr, Socket);
         Put_Wrapped(Abstract_File'Class(File_Ptr.all), At_Login.all);
         New_Line(Abstract_File'Class(File_Ptr.all));
         Command_Loop(Command_Table_Class(Commands_Ptr),
                      Abstract_File_Class(File_Ptr),
                      Security);
      exception
         when others => null;
      end;
      Shutdown(Socket);
      Free_Me_Task.Free_Proc(Self);
   end Processor;

   task body Telnet_Cmd_Listener_Task is
      Accepting_Socket : Socket_FD;
      Data_Socket      : Socket_FD;
      Debug_Socket     : Socket_FD;
      Tmp_Proc         : Processor_Ptr;
      Commands_Ptr     : Command_Table_Ptr := new Command_Table;
      Listen_Port      : Integer;
      Security         : Asl.Security.Verify_User_Class;
      Login_Text_Ptr   : String_Ptr;
      Debug_Listener   : Telnet_Debug_Listener_Task;
      Level_Cmd        : Level_Command_Ptr := new Level_Command;
      My_Debug_Proc    : Asl.Debug_Out.Debug_Processor_Class;
   begin
      select
         accept Start(Login_Text : in String;
                      Debug_Text : in String;
                      Login_Port : in Integer;
                      Debug_Port : in Integer;
                      Login_Sec  : in Asl.Security.Verify_User_Class;
                      Debug_Sec  : in Asl.Security.Verify_User_Class;
                      Cmd_Table  : out Asl.Cmdproc.Command_Table_Ptr;
                      Debug_Proc : out Asl.Debug_Out.Debug_Processor_Class)
         do
            Listen_Port := Login_Port;
            Login_Text_Ptr := new String'(Login_Text);

            Socket(Accepting_Socket, AF_INET, SOCK_STREAM);
            Setsockopt(Accepting_Socket, SOL_SOCKET, SO_REUSEADDR, 0);
            Bind(Accepting_Socket, Login_Port);
            Listen(Accepting_Socket);

            Cmd_Table := Commands_Ptr;
            Security := Login_Sec;

            Debug_Listener.Start(new String'(Debug_Text), Debug_Port,
                                 Debug_Sec, My_Debug_Proc);

            Initialize(Level_Cmd.all, My_Debug_Proc);
            Register_Command(Cmd_Table.all, "level", Command_Class(Level_Cmd));
            Debug_Proc := My_Debug_Proc;
         end Start;
      or
         terminate;
      end select;

      loop
         Accept_Socket(Accepting_Socket, Data_Socket);
         Tmp_Proc := new Processor;
         begin
            Tmp_Proc.Start(Login_Text_Ptr, Data_Socket, Security,
                           Commands_Ptr, Tmp_Proc);
         exception
            when others =>
               Free(Tmp_Proc);
         end;
      end loop;
   end Telnet_Cmd_Listener_Task;

end Asl.Cmdproc.Telnet;
