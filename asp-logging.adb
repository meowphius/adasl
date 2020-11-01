-- The Ada Structured Platform - A set of utilities the form the base
-- of a platform.
-- Copyright (C) 2001  Corey Minyard (minyard@acm.org)
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
--

with Ada.Unchecked_Deallocation;
with Ada.Tags; use type Ada.Tags.Tag;

-- The package defines an abstract interface and global variables for
-- reporting various types of log and alarm information for the
-- platform.
package body Asp.Logging is

   procedure Free_It is new Ada.Unchecked_Deallocation(Log, Log_Ptr);
   procedure Free_It is new Ada.Unchecked_Deallocation(Log_Item, Log_Item_Ptr);
   procedure Free_It is new Ada.Unchecked_Deallocation(Log_Directory,
                                                       Log_Directory_Ptr);
   procedure Free_It is new Ada.Unchecked_Deallocation(Log_Instance,
                                                       Log_Instance_Ptr);
   procedure Free_It is new Ada.Unchecked_Deallocation(String, String_Ptr);

   procedure Report_Internal_Error(Error : in String) is
   begin
      -- FIXME - implement
      null;
   end Report_Internal_Error;

   function Allocate_Log(Class  : in String;
                         Number : in Log_Number)
                         return Log_Ptr is
      Retval : Log_Ptr := new Log;
   begin
      Allocate(Retval.Class, Class);
      Retval.Number := Number;
      Retval.Self := Retval;
      return Retval;
   end Allocate_Log;

   procedure Free(Item : in out Log_Ptr) is
   begin
      Free_It(Item);
   end Free;

   procedure Finalize(L : in out Log) is
      Item : Log_Item_Ptr;
      Dir  : Log_Directory_Ptr;
   begin
      L.Self := null;

      -- Finalization will take care of the managed strings.

      while (L.Items /= null) loop
         if (L.Items.all'Tag = Log_Item'Tag) then
            Item := Log_Item_Ptr(L.Items);
            L.Items := L.Items.Next;
            Free_It(Item);
         else
            Dir := Log_Directory_Ptr(L.Items);
            L.Items := L.Items.Next;
            Free_It(Dir);
         end if;
      end loop;
   end Finalize;

   procedure Set_Text(L    : in out Log;
                      Text : in String) is
   begin
      Allocate(L.Text, Text);
   end Set_Text;

   procedure Set_Action(L      : in out Log;
                        Action : in String) is
   begin
      Allocate(L.Action, Action);
   end Set_Action;

   procedure Set_Severity(L   : in out Log;
                          Val : in Severity) is
   begin
      L.How_Bad := Val;
   end Set_Severity;

   procedure Free(Item : in out Log_Item_Ptr) is
   begin
      Free_It(Item);
   end Free;

   function Duplicate(Item : in Log_Item_Base) return Log_Item_Class is
   begin
      return null;
   end Duplicate;

   procedure Copy(Dest : in Log_Item_Class;
                  Src  : in Log_Item_Base) is
   begin
      Duplicate(Dest.Name, Src.Name);
      Dest.Self := Dest;
   end Copy;

   function Duplicate(Item : in Log_Item) return Log_Item_Class is
      Retval : Log_Item_Ptr := new Log_Item;
   begin
      Copy(Log_Item_Class(Retval), Log_Item_Base(Item));
      Duplicate(Retval.Value, Item.Value);
      return Log_Item_Class(Retval);
   end Duplicate;

   function Duplicate(Dir : in Log_Directory) return Log_Item_Class is
      Retval : Log_Directory_Ptr := new Log_Directory;
      Curr, New_Item : Log_Item_Class;
   begin
      Copy(Log_Item_Class(Retval), Log_Item_Base(Dir));
      Curr := Dir.Items;
      while (Curr /= null) loop
         New_Item := Duplicate(Curr.all);
         New_Item.Owner_Dir := Retval;
         if (Retval.Items = null) then
            Retval.Items := New_Item;
            Retval.Items_Tail := Retval.Items;
         else
            Retval.Items_Tail.Next := New_Item;
            Retval.Items_Tail := New_Item;
         end if;
         Curr := Curr.Next;
      end loop;

      return Log_Item_Class(Retval);
   end Duplicate;

   procedure Finalize(Item : in out Log_Item_Base) is
      Prev, Curr : Log_Item_Class;
   begin
      if (Item.Owner_Dir /= null) then
         Prev := null;
         Curr := Item.Owner_Dir.Items;
         while (Curr /= null) loop
            if (Curr = Item.Self) then
               if (Prev = null) then
                  Item.Owner_Dir.Items := Curr.Next;
                  if (Item.Owner_Dir.Items_Tail = Curr) then
                     Item.Owner_Dir.Items_Tail := null;
                  end if;
               else
                  Prev.Next := Curr.Next;
                  if (Item.Owner_Dir.Items_Tail = Curr) then
                     Item.Owner_Dir.Items_Tail := Prev;
                  end if;
               end if;
               exit;
            end if;
            Prev := Curr;
            Curr := Curr.Next;
         end loop;
      elsif (Item.Owner /= null) then
         Prev := null;
         Curr := Item.Owner.Items;
         while (Curr /= null) loop
            if (Curr = Item.Self) then
               if (Prev = null) then
                  Item.Owner.Items := Curr.Next;
                  if (Item.Owner.Items_Tail = Curr) then
                     Item.Owner.Items_Tail := null;
                  end if;
               else
                  Prev.Next := Curr.Next;
                  if (Item.Owner.Items_Tail = Curr) then
                     Item.Owner.Items_Tail := Prev;
                  end if;
               end if;
               exit;
            end if;
            Prev := Curr;
            Curr := Curr.Next;
         end loop;
      end if;
      Item.Owner := null;
      Item.Owner_Dir := null;
      Item.Next := null;

      -- Finalization will take care of the name and value.

      Item.Self := null;
   end Finalize;

   procedure Free(Dir : in out Log_Directory_Ptr) is
   begin
      Free_It(Dir);
   end Free;

   procedure Finalize(Dir : in out Log_Directory) is
      Prev, Curr : Log_Item_Class;
   begin
      -- Do the directory-specific cleanup.
      Prev := null;
      Curr := Dir.Items;
      while (Curr /= null) loop
         Curr.Owner_Dir := null;
         Curr.Owner := null;
         Prev := Curr;
         Curr := Curr.Next;
         Prev.Next := null;
      end loop;
      Dir.Items := null;
      Dir.Items_Tail := null;

      -- Now call the parent finalizer.
      Finalize(Log_Item_Base(Dir));
   end Finalize;

   procedure Add_Item(L    : in out Log;
                      Name : in String;
                      Item : out Log_Item_Ptr) is
      Retval : Log_Item_Ptr := new Log_Item;
   begin
      Retval.Owner := L.Self;
      if (L.Items = null) then
         L.Items := Log_Item_Class(Retval);
         L.Items_Tail := L.Items;
      else
         Retval.Next := L.Items_Tail;
         L.Items_Tail := Log_Item_Class(Retval);
      end if;
      Allocate(Retval.Name, Name);
      Item := Retval;
   end Add_Item;

   procedure Add_Item(Dir  : in out Log_Directory;
                      Name : in String;
                      Item : out Log_Item_Ptr) is
      Retval : Log_Item_Ptr := new Log_Item;
   begin
      Retval.Owner_Dir := Log_Directory_Ptr(Dir.Self);
      if (Dir.Items = null) then
         Dir.Items := Log_Item_Class(Retval);
         Dir.Items_Tail := Dir.Items;
      else
         Retval.Next := Dir.Items_Tail;
         Dir.Items_Tail := Log_Item_Class(Retval);
      end if;
      Allocate(Retval.Name, Name);
      Item := Retval;
   end Add_Item;

   function Get_Name(Item : in Log_Item) return String is
   begin
      return Get(Item.Name);
   end Get_Name;

   procedure Add_Directory(L    : in out Log;
                           Name : in String;
                           Dir  : out Log_Directory_Ptr) is
      Retval : Log_Directory_Ptr := new Log_Directory;
   begin
      Retval.Owner := L.Self;
      if (L.Items = null) then
         L.Items := Log_Item_Class(Retval);
         L.Items_Tail := L.Items;
      else
         Retval.Next := L.Items_Tail;
         L.Items_Tail := Log_Item_Class(Retval);
      end if;
      Allocate(Retval.Name, Name);
      Dir := Retval;
   end Add_Directory;

   procedure Add_Directory(Parent : in out Log_Directory;
                           Name   : in String;
                           Dir    : out Log_Directory_Ptr) is
      Retval : Log_Directory_Ptr := new Log_Directory;
   begin
      Retval.Owner_Dir := Log_Directory_Ptr(Parent.Self);
      if (Parent.Items = null) then
         Parent.Items := Log_Item_Class(Retval);
         Parent.Items_Tail := Parent.Items;
      else
         Retval.Next := Parent.Items_Tail;
         Parent.Items_Tail := Log_Item_Class(Retval);
      end if;
      Allocate(Retval.Name, Name);
      Dir := Retval;
   end Add_Directory;

   procedure Set_Value(Item : in out Log_Item;
                       Val  : in String) is
   begin
      Allocate(Item.Value, Val);
   end Set_Value;

   procedure Clear_Value(Item : in out Log_Item) is
   begin
      if (not Is_Null(Item.Value)) then
         Free(Item.Value);
      end if;
   end Clear_Value;

   function Get_Instance(L : in Log) return Log_Instance_Ptr is
      Retval : Log_Instance_Ptr := new Log_Instance;
      Curr, New_Item : Log_Item_Class;
   begin
      Retval.Log_Time := Clock;
      Duplicate(Retval.Class, L.Class);
      Retval.Number := L.Number;
      Duplicate(Retval.Text, L.Text);
      Duplicate(Retval.Action, L.Action);
      Retval.How_Bad := L.How_Bad;
      Retval.Self := Log_Ptr(Retval);

      Curr := L.Items;
      while (Curr /= null) loop
         New_Item := Duplicate(Curr.all);
         New_Item.Owner := Log_Ptr(Retval);
         if (Retval.Items = null) then
            Retval.Items := New_Item;
            Retval.Items_Tail := Retval.Items;
         else
            Retval.Items_Tail.Next := New_Item;
            Retval.Items_Tail := New_Item;
         end if;
         Curr := Curr.Next;
      end loop;

      Retval.Curr_Item := Retval.Items;

      return Retval;
   end Get_Instance;

   procedure Free(Inst : in out Log_Instance_Ptr) is
   begin
      Free_It(Inst);
   end Free;

   procedure Finalize(Inst : in out Log_Instance) is
      Curr, Next : Log_Item_Class;
      Item : Log_Item_Ptr;
      Dir  : Log_Directory_Ptr;
   begin
      -- Finalization will take care of the managed strings.

      -- We free the items in the instance, because the user doesn't
      -- manage them.
      Curr := Inst.Items;
      while (Curr /= null) loop
         Next := Curr.Next;
         if (Curr'Tag = Log_Item'Tag) then
            Item := Log_Item_Ptr(Curr);
            Free_It(Item);
         else
            Dir := Log_Directory_Ptr(Curr);
            Free_It(Dir);
         end if;
         Curr := Next;
      end loop;
   end Finalize;

   procedure Reset(Inst : in out Log_Instance) is
   begin
      Inst.Curr_Item := Inst.Items;
      Inst.Curr_Dir := null;
   end Reset;

   function Get_Time(Inst : in Log_Instance) return Ada.Calendar.Time is
   begin
      return Inst.Log_Time;
   end Get_Time;

   function Get_Class(Inst : in Log_Instance) return String is
   begin
      return Get(Inst.Class);
   end Get_Class;

   function Get_Number(Inst : in Log_Instance) return Log_Number is
   begin
      return Inst.Number;
   end Get_Number;

   function Get_Text(Inst : in Log_Instance) return String is
   begin
      return Get(Inst.Text);
   end Get_Text;

   function Get_Action(Inst : in Log_Instance) return String is
   begin
      return Get(Inst.Action);
   end Get_Action;

   function Get_Severity(Inst : in Log_Instance) return Severity is
   begin
      return Inst.How_Bad;
   end Get_Severity;

   function Get_Curr_Type(Inst : in Log_Instance) return Log_Item_Type is
   begin
      if (Inst.Curr_Item = null) then
         if (Inst.Curr_Dir = null) then
            return End_Log;
         else
            return End_Directory;
         end if;
      elsif (Inst.Curr_Item'Tag = Log_Directory'Tag) then
         return Directory;
      elsif (Is_Null(Log_Item_Ptr(Inst.Curr_Item).Value)) then
         return Empty_Item;
      else
         return Item;
      end if;
   end Get_Curr_Type;

   function Get_Curr_Name(Inst : in Log_Instance) return String is
   begin
      if (Inst.Curr_Item = null) then
         if (Inst.Curr_Dir = null) then
            raise Invalid_Item;
         else
            return Get(Inst.Curr_Dir.Name);
         end if;
      else
         return Get(Inst.Curr_Item.Name);
      end if;
   end Get_Curr_Name;

   function Get_Curr_Value(Inst : in Log_Instance) return String is
   begin
      if (Inst.Curr_Item = null) then
         raise Invalid_Item;
      else
         return Get(Inst.Curr_Item.Name);
      end if;
   end Get_Curr_Value;

   procedure Next_Item(Inst : in out Log_Instance) is
   begin
      if (Inst.Curr_Item = null) then
         if (Inst.Curr_Dir = null) then
            raise Invalid_Item;
         else
            Inst.Curr_Item := Inst.Curr_Dir.Next;
            Inst.Curr_Dir := Inst.Curr_Dir.Owner_Dir;
         end if;
      else
         Inst.Curr_Item := Inst.Curr_Item.Next;
      end if;
   end Next_Item;


   Orig_Logger : Basic_Logger_Ptr := new Basic_Logger;

   procedure Generate(L      : in out Basic_Logger;
                      To_Gen : in Log_Instance_Ptr) is
   begin

      Add_Tail(L.Logs.all, To_Gen);
   end Generate;

   function Get_Next_Initial_Log return Log_Instance_Ptr is
      It : Log_Instance_List.Iterator
        := New_Iterator(Log_Instance_List.Object_Class(Orig_Logger.Logs));
      Is_End : Log_Instance_Container.End_Marker;
      Retval : Log_Instance_Ptr;
   begin
      First(It, Is_End);
      if (Is_End = Past_End) then
         return null;
      else
         Retval := Get(It);
         Delete(It, Is_End);
         return Retval;
      end if;
   end Get_Next_Initial_Log;


begin
   Main_Logger := Logger_Class(Orig_logger);
end Asp.Logging;
