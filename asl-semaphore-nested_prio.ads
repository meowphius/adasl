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
--

with System;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification;

-- A nested priority-inheritance semaphore.  Note that this will change the
-- task's priority dynamically.  If the user modifies a semaphore owner's
-- priority, the change might be lost when the semaphore is given because
-- the priority is set back to the value when the task took the semaphore.

-- DANGER - These semaphores don't work right unless setting the dynamic
-- priority of a task sets it immediately.  GNAT doesn't do this by under
-- Linux.  I'm thinking of a way to fix it, but nothing has come to me yet.

package Asl.Semaphore.Nested_Prio is

   type Object is new Asl.Semaphore.Object with private;
   type Object_Ptr is access all Object;

   -- Claim the semaphore.  Will block until the semaphore is available.
   procedure Take (O : in out Object);

   -- Release the semaphore.
   procedure Give (O : in out Object);

   -- Try to take the semaphore, but return an error if not successful.
   -- Returns True if the semaphore was claimed and False if not.  This
   -- will wait up to Timeout time for the semaphore to become available.
   procedure Try_To_Take (O       : in out Object;
                          Success : out Boolean;
                          Timeout : in Duration := 0.0);

private

   Internal_Nested_Prio_Error : exception;

   package Task_Ident renames Ada.Task_Identification;
   package Task_Prio renames Ada.Dynamic_Priorities;

   type Task_Data;
   type Task_Data_Ptr is access all Task_Data;
   type Task_Data is record
      Id   : Task_Ident.Task_Id;
      Prio : System.Any_Priority;
      Next : Task_Data_Ptr;
   end record;

   protected type Mutex is
      procedure Add_Task (Info : in Task_Data_Ptr);
      procedure Remove_Task (Info : in Task_Data_Ptr);
      entry Wait;
      procedure Release;
   private
      Owner         : Task_Ident.Task_Id  := Task_Ident.Null_Task_Id;
      Count         : Natural             := 0;
      Orig_Prio     : System.Any_Priority;
      Max_Wait_Prio : System.Any_Priority := System.Any_Priority'First;
      Waiting_Tasks : Task_Data_Ptr := null;
   end Mutex;

   type Object is new Asl.Semaphore.Object with record
      The_Mutex : Mutex;
   end record;

end Asl.Semaphore.Nested_Prio;
