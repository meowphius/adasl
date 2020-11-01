
with Ada.Strings.Unbounded; use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO; use Ada.Text_IO;

package Word_Parser is

   procedure Get_Next_Word
     (File       : in File_Type;
      Word       : out Ada.Strings.Unbounded.Unbounded_String;
      Word_Found : out Boolean;
      File_Done  : out Boolean);

end Word_Parser;
