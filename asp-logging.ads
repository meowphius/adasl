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

with Ada.Finalization; use Ada.Finalization;
with Ada.Calendar; use Ada.Calendar;
with Asl.Refcount_String; use Asl.Refcount_String;
with Asgc.List.Dynamic;

-- The package defines an abstract interface and global variables for
-- reporting various types of log and alarm information for the
-- platform.
package Asp.Logging is

   -- Report an internal error.  This should be called when the software
   -- detects that an error has occured in itself that is due to a
   -- software bug.  It should report enough information to reproduce
   -- the bug or at least figure out what the error was.
   procedure Report_Internal_Error(Error : in String);

   -- Severity level of a log.
   type Severity is (Fatal,     -- The application is going down.
                     Critical,  -- The application is unable to operate.
                     Major,     -- The application can operate, but is
                                --   hampered in a major way.
                     Minor,     -- The application can operate, but something
                                --   needs to be fixed.
                     None);     -- Logs for reporting information use this.

   -- Valid numbers for logs.
   type Log_Number is new Integer range 0 .. 999;

   -- A log for reporting things the the system's user.
   type Log is new Limited_Controlled with private;
   type Log_Ptr is access all Log;

   -- Allocate a log.  The class and number are used to identify the
   -- log, the class should be a short string, the number should be
   -- an integer.  This lets logs be categorized and easily looked
   -- up by the system user.
   function Allocate_Log(Class  : in String;
                         Number : in Log_Number)
                         return Log_Ptr;
   procedure Free(Item : in out Log_Ptr);
   procedure Finalize(L : in out Log);

   -- Set the text to be printed by the log.  This is a free format
   -- and may be any arbitrary length, the logger is expected to handle
   -- wrapping it properly.
   procedure Set_Text(L    : in out Log;
                      Text : in String);

   -- Set the action text for a log.  This is what the system
   -- operator should do to fix the problem.  This may be left unset or set
   -- to an empty string if no action is required by the operator.
   procedure Set_Action(L      : in out Log;
                        Action : in String);

   -- Set the severity of the log.
   procedure Set_Severity(L   : in out Log;
                          Val : in Severity);

   type Log_Item_Base is abstract new Limited_Controlled with private;
   type Log_Item_Class is access all Log_Item_Base'Class;

   procedure Finalize(Item : in out Log_Item_Base);

   -- If the user finished with a directory for some reason, they may
   -- A log may contain multiple name-value pairs for data items in the
   -- log.  These may be nested into directories in a hierarchical
   -- format, much like XML.  The idea is to give the logs a nice
   -- structure that can be easily converted to different forms and
   -- processed by other programs.
   type Log_Item is new Log_Item_Base with private;
   type Log_Item_Ptr is access all Log_Item;

   -- If the user finished with a log item for some reason, they may
   -- free it.  Freeing an item removes it from the log it is part
   -- of.
   procedure Free(Item : in out Log_Item_Ptr);

   -- A directory that can contain other items and directories.
   type Log_Directory is new Log_Item_Base with private;
   type Log_Directory_Ptr is access all Log_Directory;

   -- If the user finished with a directory for some reason, they may
   -- free it.  Freeing a directory removes it from the log it is part
   -- of, and all its sub-items.  The sub-items are NOT freed automatically,
   -- though.  The user must still do that.
   procedure Free(Dir : in out Log_Directory_Ptr);
   procedure Finalize(Dir : in out Log_Directory);

   -- Add an item to the log with the given name.  The item is returned, and
   -- that item should be used to manipulate the thing.
   procedure Add_Item(L    : in out Log;
                      Name : in String;
                      Item : out Log_Item_Ptr);

   -- Add an item to the given log directory with the given name.  The
   -- item is returned.
   procedure Add_Item(Dir  : in out Log_Directory;
                      Name : in String;
                      Item : out Log_Item_Ptr);

   -- Return the name of a log item.
   function Get_Name(Item : in Log_Item) return String;

   -- Add a directory to a log with the given name.
   procedure Add_Directory(L    : in out Log;
                           Name : in String;
                           Dir  : out Log_Directory_Ptr);

   -- Add a directory to another directory with the given name.
   procedure Add_Directory(Parent : in out Log_Directory;
                           Name   : in String;
                           Dir    : out Log_Directory_Ptr);

   -- Set the value for an item.
   procedure Set_Value(Item : in out Log_Item;
                       Val  : in String);

   -- Clear out the value for an item so the "value" is unset.
   procedure Clear_Value(Item : in out Log_Item);


   -- The actual log is not passed to a logger, an instance must be passed
   -- The application must get an instance of a log and give it to the
   -- logger.  This also lets the application set specific fields in the
   -- log.
   type Log_Instance is new Log with private;
   type Log_Instance_Ptr is access all Log_Instance;

   -- Thrown when data is fetched from an item that it is not valid to
   -- fetch the data from.
   Invalid_Item : exception;

   function Get_Instance(L : in Log) return Log_Instance_Ptr;

   procedure Free(Inst : in out Log_Instance_Ptr);
   procedure Finalize(Inst : in out Log_Instance);

   type Log_Item_Type is (Item,          -- A data item with a value
                          Empty_Item,    -- A data item without a value
                          Directory,     -- A directory of items
                          End_Directory, -- The end of the current directory
                          End_Log);      -- The end of the whole log

   -- Go the the beginning of the log instance, setting the first item
   -- as the current item.
   procedure Reset(Inst : in out Log_Instance);

   -- Get the time the log was generated
   function Get_Time(Inst : in Log_Instance) return Ada.Calendar.Time;

   -- Get the log class
   function Get_Class(Inst : in Log_Instance) return String;

   -- Get the log number
   function Get_Number(Inst : in Log_Instance) return Log_Number;

   -- Get the text of the log.
   function Get_Text(Inst : in Log_Instance) return String;

   -- Get the action of the log, or an empty string if no action was
   -- specified.
   function Get_Action(Inst : in Log_Instance) return String;

   -- Get the severity of the log.
   function Get_Severity(Inst : in Log_Instance) return Severity;

   -- Return the type of the log item.
   function Get_Curr_Type(Inst : in Log_Instance) return Log_Item_Type;

   -- Get the name of the current log instance.  This is not valid for
   -- End_Log types, which will cause an Invalid_Item exception to be
   -- raised.  For items and directories, it returns the name.  For
   -- End_Directory types, it returns the name of the directory that
   -- was started.
   function Get_Curr_Name(Inst : in Log_Instance) return String;

   -- Return the value for the item.  If the type if not an item,
   -- this will raise an Invalid_Item exception.
   function Get_Curr_Value(Inst : in Log_Instance) return String;

   -- Move to the next item in the log instance.  If the current
   -- item is End_Log type, then this will raise Invalid_Item.
   procedure Next_Item(Inst : in out Log_Instance);


   -- This is an abstract class of an object that will actually take
   -- logs and do something with them.
   type Logger is abstract tagged limited private;
   type Logger_Class is access all Logger'Class;

   -- Actuall do something with the log.
   procedure Generate(L      : in out Logger;
                      To_Gen : in Log_Instance_Ptr) is abstract;

   -- This is the variable the applications should use for logging.  They
   -- should not make a local copy of this, as it may change, especially
   -- during initialization, but all logs are guaranteed to be handled.
   Main_Logger : Logger_Class;

   -- The initial logger just queues items until they are ready to be
   -- processed by a real log system, this call lets the real log
   -- system get everything from the original basic logger.  This will
   -- return null when no initial logs are left.
   function Get_Next_Initial_Log return Log_Instance_Ptr;

private

   type Log is new Limited_Controlled with record
      Class      : Counted_String;
      Number     : Log_Number;
      Text       : Counted_String;
      Action     : Counted_String;
      How_Bad    : Severity := None;
      Items      : Log_Item_Class := null;
      Items_Tail : Log_Item_Class := null;
      Self       : Log_Ptr;
   end record;

   type Log_Item_Base is abstract new Limited_Controlled with record
      Owner     : Log_Ptr := null;
      Owner_Dir : Log_Directory_Ptr := null;
      Next      : Log_Item_Class := null;
      Name      : Counted_String;
      Self      : Log_Item_Class;
   end record;

   function Duplicate(Item : in Log_Item_Base) return Log_Item_Class;

   type Log_Item is new Log_Item_Base with record
      Value : Counted_String;
   end record;

   function Duplicate(Item : in Log_Item) return Log_Item_Class;

   type Log_Directory is new Log_Item_Base with record
      Items      : Log_Item_Class := null;
      Items_Tail : Log_Item_Class := null;
   end record;

   function Duplicate(Dir : in Log_Directory) return Log_Item_Class;

   type Log_Instance is new Log with record
      Log_Time  : Time;
      Curr_Item : Log_Item_Class;
      Curr_Dir  : Log_Directory_Ptr;
   end record;

   type Logger is abstract tagged limited null record;

   package Log_Instance_Container is new Asgc(Log_Instance_Ptr);
   package Log_Instance_L1 is new Log_Instance_Container.List;
   package Log_Instance_List is new Log_Instance_L1.Dynamic;
   use Log_Instance_List; use Log_Instance_Container;

   type Basic_Logger is new Logger with record
      Logs : Log_Instance_List.Object_Ptr := new Log_Instance_List.Object;
   end record;
   type Basic_Logger_Ptr is access all Basic_Logger;

   procedure Generate(L      : in out Basic_Logger;
                      To_Gen : in Log_Instance_Ptr);

end Asp.Logging;
