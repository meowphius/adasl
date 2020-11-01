
with Asl.Security; use Asl.Security;
with Asl.Security.Userpass; use Asl.Security.Userpass;

with Asl.Cmdproc; use Asl.Cmdproc;
with Asl.Debug_Out; use Asl.Debug_Out;
with Asl.Abstract_IO; use Asl.Abstract_IO;
with Asl.Strings.Tokenizer; use Asl.Strings.Tokenizer;

package Telnet_Cmdproc_Handlers is

   type Dummy_Verify_User is new Verify_User_Userpass with null record;
   type Dummy_Verify_User_Ptr is access all Dummy_Verify_User;

   procedure Verify_Access(Verify   : in out Dummy_Verify_User;
                           User     : in String;
                           Password : in String;
                           Permit   : out Boolean);

   type Gen_Debug_Command is new Command with record
      Level : Debug_Level_Class;
      Proc  : Debug_Processor_Class;
   end record;
   type Gen_Debug_Command_Ptr is access all Gen_Debug_Command;

   procedure Execute(Cmd  : in out Gen_Debug_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class);

   procedure Help(Cmd  : in out Gen_Debug_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class);

end Telnet_Cmdproc_Handlers;
