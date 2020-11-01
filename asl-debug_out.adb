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

package body Asl.Debug_Out is

   procedure Free is new Unchecked_Deallocation(String, String_Ptr);

   procedure Free is new Unchecked_Deallocation(Debug_Hash.Object,
                                                Debug_Hash.Object_Ptr);

   procedure Enable_Debug_Level(Level : in out Debug_Level) is
   begin
      Level.Enabled := True;
   end Enable_Debug_Level;

   procedure Disable_Debug_Level(Level : in out Debug_Level) is
   begin
      Level.Enabled := False;
   end Disable_Debug_Level;

   function Get_Name(Level : in Debug_Level)
                     return String is
   begin
      return Level.Name.all;
   end Get_Name;


   procedure Register_Debug_Level(Proc  : in out Debug_Processor;
                                  Name  : in String;
                                  Level : access Debug_Level'Class) is
   begin
      Level.Proc := Proc.Self;
      Proc.Table.Register_Debug(Name, Debug_Level_Class(Level));
   end Register_Debug_Level;

   procedure Enable_Debug_Level(Proc : in out Debug_Processor;
                                Name : access String) is
      Level : Debug_Level_Class;
   begin
      Proc.Table.Get_Debug(Name, Level);
      if (Level = null) then
         raise Invalid_Debug_Level;
      else
         Enable_Debug_Level(Level.all);
      end if;
   end Enable_Debug_Level;

   procedure Disable_Debug_Level(Proc : in out Debug_Processor;
                                 Name : access String) is
      Level : Debug_Level_Class;
   begin
      Proc.Table.Get_Debug(Name, Level);
      if (Level = null) then
         raise Invalid_Debug_Level;
      else
         Disable_Debug_Level(Level.all);
      end if;
   end Disable_Debug_Level;

   procedure Get_Debug_Level(Proc  : in out Debug_Processor;
                             Name  : access String;
                             Level : out Debug_Level_Class) is
   begin
      Proc.Table.Get_Debug(Name, Level);
   end Get_Debug_Level;

   function Is_Equal(V1, V2 : in Debug_Level_Class)
                     return Boolean is
   begin
      return ((V1.Name'Length = V2.Name'Length)
              and then (V1.Name.all = V2.Name.all));
   end Is_Equal;

   function Hash(V : in Debug_Level_Class) return Natural is
      type Hasher is mod 2 ** (Natural'Size - 1);
      Val : Hasher := 0;
   begin
      for I in V.Name'Range loop
         Val := (Val * 67) * (Character'Pos(V.Name(I)) * 43);
      end loop;

      return Natural(Val);
   end Hash;

   protected body Protected_Debug_Table is
      procedure Register_Debug(Name  : in String;
                               Level : in Debug_Level_Class) is
      begin
         if (Name'Length = 0) then
            raise Constraint_Error;
         end if;

         if (Level.Name /= null) then
            raise Debug_Level_Already_Registered;
         end if;

         Level.Name := new String'(Name);
         begin
            Debug_Hash.Add(T.all, Level);
         exception
            when others =>
               Free(Level.Name);
               raise Duplicate_Debug_Level;
         end;
      end Register_Debug;

      procedure Get_Debug(Name  : access String;
                          Level : out Debug_Level_Class) is
         Iter      : Debug_Hash.Iterator;
         Tmp       : aliased Debug_Level;
         Tmp_Ptr   : Debug_Level_Ptr := Tmp'Unchecked_Access;
         Found     : Boolean := False;
      begin
         Iter := Debug_Hash.New_Iterator(Debug_Hash.Object_Class(T));
         Tmp.Name := String_Ptr(Name);
         Debug_Hash.Search(Iter, Debug_Level_Class(Tmp_Ptr), Found);
         if (not Found) then
            Level := null;
         else
            Level := Debug_Hash.Get(Iter);
         end if;
      end Get_Debug;

      procedure Destroy is
      begin
         Free(T);
      end Destroy;
   end Protected_Debug_Table;

end Asl.Debug_Out;
