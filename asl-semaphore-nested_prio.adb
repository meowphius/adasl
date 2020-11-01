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

with Ada.Task_Identification; use type Ada.Task_Identification.Task_Id;
with System; use type System.Any_Priority;

package body Asl.Semaphore.Nested_Prio is

   ------------------------------------------------------------------------
   -- Take creates a task data structure on the stack.  This way, we don't
   -- have to allocate one with dynamic allocation.  This should be safe to
   -- do since it will only be used in the context of this routine.
   procedure Take (O : in out Object) is
      My_Task : aliased Task_Data;
   begin
      O.The_Mutex.Add_Task(My_Task'Unchecked_Access);
      O.The_Mutex.Wait;
      O.The_Mutex.Remove_Task(My_Task'Unchecked_Access);
      -- Fixme - Do I need to add an exception handler or some way to
      -- handle aborts?
   end Take;

   ------------------------------------------------------------------------
   procedure Give (O : in out Object) is
   begin
      O.The_Mutex.Release;
   end Give;

   ------------------------------------------------------------------------
   procedure Try_To_Take (O       : in out Object;
                          Success : out Boolean;
                          Timeout : in Duration := 0.0) is
      My_Task : aliased Task_Data;
   begin
      O.The_Mutex.Add_Task(My_Task'Unchecked_Access);
      select
         O.The_Mutex.Wait;
         Success := True;
      or
         delay Timeout;
         Success := False;
      end select;
      O.The_Mutex.Remove_Task(My_Task'Unchecked_Access);
      -- Fixme - Do I need to add an exception handler or some way to
      -- handle aborts?
   end Try_To_Take;

   ------------------------------------------------------------------------
   ------------------------------------------------------------------------
   protected body Mutex is

      ---------------------------------------------------------------------
      -- Add a task to the list of waiting tasks and update the priority
      -- accordingly.
      procedure Add_Task (Info : in Task_Data_Ptr) is
      begin
         -- Fill in the task's data and put it into the list.
         Info.Id := Task_Ident.Current_Task;
         Info.Prio := Task_Prio.Get_Priority;
         Info.Next := Waiting_Tasks;
         Waiting_Tasks := Info;

         -- If the new tasks priority is greater than the current priority,
         -- up the priority of the running task.  The check here is needed
         -- for a running task due to a race condition between adding a
         -- task and claiming the semaphore.
         if ((Owner /= Task_Ident.Null_Task_Id)
             and (Info.Prio > Max_Wait_Prio))
         then
            Max_Wait_Prio := Info.Prio;
            Task_Prio.Set_Priority(Max_Wait_Prio, Owner);
         end if;
      end Add_Task;

      ---------------------------------------------------------------------
      -- Remove the task from the list of waiting tasks and decrease the
      -- priority if necessary.
      procedure Remove_Task (Info : in Task_Data_Ptr) is
         Prev     : Task_Data_Ptr := null;
         Curr     : Task_Data_Ptr := Waiting_Tasks;
         New_Prio : System.Any_Priority;
      begin
         while ((Curr /= null) and then (Curr /= Info)) loop
            Prev := Curr;
            Curr := Curr.Next;
         end loop;
         if (Curr = null) then
            raise Internal_Nested_Prio_Error;
         end if;
         if (Prev = null) then
            Waiting_Tasks := Curr.Next;
         else
            Prev.Next := Curr.Next;
         end if;

         -- The deleted task is the highest priority, perhaps we need to
         -- reduce Max_Wait_Prio.
         if (Info.Prio = Max_Wait_Prio) then

            -- Search for the highest priority.
            Curr := Waiting_Tasks;
            New_Prio := System.Any_Priority'First;
            while (Curr /= null) loop
               if (Curr.Prio > New_Prio) then
                  New_Prio := Curr.Prio;
               end if;
               Curr := Curr.Next;
            end loop;

            if (New_Prio /= Max_Wait_Prio) then
               -- The highest waiting priority has changed.

               if ((Owner /= Task_Ident.Null_Task_Id)
                   and (Max_Wait_Prio > Orig_Prio))
               then
                  -- We have changed the owner's priority previously, so
                  -- reduce it to either the natural priority or to the new
                  -- max priority.  We need to check the owner because of
                  -- possible race conditions.
                  if (New_Prio > Orig_Prio) then
                     Task_Prio.Set_Priority(New_Prio, Owner);
                  else
                     Task_Prio.Set_Priority(Orig_Prio, Owner);
                  end if;
               end if;

               Max_Wait_Prio := New_Prio;
            end if;
         end if;
      end Remove_Task;

      ---------------------------------------------------------------------
      -- Let the task in if no other task has the semaphore or if the
      -- calling task already owns the semaphore.
      entry Wait when ((Count = 0) or (Task_Ident.Current_Task = Owner)) is
      begin
         if (Count = 0) then
            Owner := Wait'Caller;
            Orig_Prio := Task_Prio.Get_Priority(Owner);
            if (Max_Wait_Prio > Orig_Prio) then
               Task_Prio.Set_Priority(Max_Wait_Prio, Owner);
            end if;
         end if;
         Count := Count + 1;
      end Wait;

      ---------------------------------------------------------------------
      procedure Release is
      begin
         if (Task_Ident.Current_Task /= Owner) then
            raise Tasking_Error;
         end if;

         Count := Count - 1;
         if (Count = 0) then
            -- We are releasing the semaphore, so set our priority back
            -- if necessary.
            if (Max_Wait_Prio > Orig_Prio) then
               Task_Prio.Set_Priority(Orig_Prio, Owner);
            end if;
            Owner := Task_Ident.Null_Task_Id;
         end if;
      end Release;

   end Mutex;

end Asl.Semaphore.Nested_Prio;
