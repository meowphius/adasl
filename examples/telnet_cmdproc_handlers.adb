with Asl.Strings.Tokenizer; use Asl.Strings.Tokenizer;

package body Telnet_Cmdproc_Handlers is

   Permitted_User : String := "dummy";
   Permitted_Pass : String := "login";

   procedure Verify_Access(Verify   : in out Dummy_Verify_User;
                           User     : in String;
                           Password : in String;
                           Permit   : out Boolean) is
   begin
      Permit := (((User'Length = Permitted_User'Length)
                  and (Password'Length = Permitted_Pass'Length))
                 and then ((User = Permitted_User)
                           and (Password = Permitted_Pass)));
   end Verify_Access;

   procedure Execute(Cmd  : in out Gen_Debug_Command;
                     Name : in String;
                     Proc : in out Command_Processor'Class;
                     IO   : in out Abstract_File'Class;
                     Tok  : in out Tokenized_String'Class) is
      Size : Integer;
   begin
      Recalc_Current_Token_End(Tok, "");
      Size := Current_Token_Length(Tok);
      if (Size = 0) then
         Put_Line(IO, "No text to send to the level");
      else
         declare
            Text : String := Current_Token(Tok);
         begin
            Output_Log(Cmd.Proc.all, Cmd.Level.all, Text);
         end;
      end if;
   end Execute;

   procedure Help(Cmd  : in out Gen_Debug_Command;
                  Name : in String;
                  Proc : in out Command_Processor'Class;
                  IO   : in out Abstract_File'Class;
                  Tok  : in out Tokenized_String'Class) is
   begin
      null;
   end Help;

end Telnet_Cmdproc_Handlers;
