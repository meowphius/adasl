-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
--
-- This code is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This code is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--

-- Test all the different types of semaphores.

-- We need this here to be sure we are using this tasking policy.  We rely
-- on giving up the CPU with delay to make things work properly.
pragma Task_Dispatching_Policy(FIFO_Within_Priorities);

with Asl.Semaphore.Binary;
with Asl.Semaphore.Counting;
with Asl.Semaphore.Nested;
with Asl.Semaphore.Nested_Prio;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification;
with System;
with Text_IO; use Text_IO;
with Ada.Command_Line;

procedure Test_Semaphore is

   package Base_Sem renames Asl.Semaphore;
   package Binary renames Asl.Semaphore.Binary;
   package Counting renames Asl.Semaphore.Counting;
   package Nested renames Asl.Semaphore.Nested;
   package Nested_Prio renames Asl.Semaphore.Nested_Prio;

   Bin_Sem           : aliased Binary.Object;
   Bin_Sem_Ptr       : Binary.Object_Ptr := Bin_Sem'Unchecked_Access;
   Count_Sem         : aliased Counting.Object(Initial_Count => 1);
   Count_Sem_Ptr     : Counting.Object_Ptr := Count_Sem'Unchecked_Access;
   Nest_Sem          : aliased Nested.Object;
   Nest_Sem_Ptr      : Nested.Object_Ptr := Nest_Sem'Unchecked_Access;
   Nest_Prio_Sem     : aliased Nested_Prio.Object;
   Nest_Prio_Sem_Ptr : Nested_Prio.Object_Ptr
     := Nest_Prio_Sem'Unchecked_Access;

   Semaphore_Error : exception;

   Completed : Boolean;

   -- A task that will take the semaphore when Ready is called and then
   -- give it up when Give_It is called.
   task type Take_Give (Sem : Base_Sem.Object_Class) is
      entry Ready;
      entry Give_It;
   end Take_Give;

   task body Take_Give is
   begin
      accept Ready;
      Base_Sem.Take(Sem.all);
      Completed := True;
      accept Give_It;
      Base_Sem.Give(Sem.all);
   exception
      when others =>
         Put_Line("Exception raised in Take_Give");
   end Take_Give;


   -- A task that will take the semaphore when set ready.
   task type Taker (Sem : Base_Sem.Object_Class) is
      entry Ready;
   end Taker;

   task body Taker is
   begin
      accept Ready;
      Base_Sem.Take(Sem.all);
      Completed := True;
   exception
      when others =>
         Put_Line("Exception raised in Taker");
   end Taker;


   -- Test a binary semaphore.
   procedure Test_Binary (Sem : in Base_Sem.Object_Class) is
      Task1   : Taker(Sem);
      Success : Boolean;
   begin
      -- Set up, I will own the semaphore at first.
      Completed := False;
      Base_Sem.Take(Sem.all);

      -- I shouldn't be able to take it again.
      Base_Sem.Try_To_Take(Sem.all, Success);
      if (Success) then
         raise Semaphore_Error;
      end if;

      -- Give it up and do a conditional take.
      Base_Sem.Give(Sem.all);
      Base_Sem.Try_To_Take(Sem.all, Success);
      if (not Success) then
         raise Semaphore_Error;
      end if;

      -- Kick the other task off, it will claim the semaphore.
      Task1.Ready;
      -- Cause a task switch so the other task will run.
      delay 0.0;

      if (Completed) then
         raise Semaphore_Error;
      end if;
      Base_Sem.Give(Sem.all);
      -- Cause a task switch so the other task will run.
      delay 0.0;

      -- Task1 should have taken the semaphore now.
      if (not Completed) then
         raise Semaphore_Error;
      end if;
      Base_Sem.Give(Sem.all);
   end Test_Binary;


   procedure Test_Count (Sem : in Base_Sem.Object_Class) is
      Success : Boolean;
   begin
      -- Count should be 1 on entry, increase it to 3.
      Base_Sem.Give(Sem.all);
      Base_Sem.Give(Sem.all);

      -- make sure it can be taken 3 times now.
      Base_Sem.Try_To_Take(Sem.all, Success);
      if (not Success) then
         raise Semaphore_Error;
      end if;

      Base_Sem.Try_To_Take(Sem.all, Success);
      if (not Success) then
         raise Semaphore_Error;
      end if;

      Base_Sem.Try_To_Take(Sem.all, Success);
      if (not Success) then
         raise Semaphore_Error;
      end if;

      -- I can't take it any more!
      Base_Sem.Try_To_Take(Sem.all, Success);
      if (Success) then
         raise Semaphore_Error;
      end if;

      -- Increase the count back to 1.
      Base_Sem.Give(Sem.all);
   end Test_Count;


   procedure Test_Nested (Sem : in Base_Sem.Object_Class) is
      Task1   : Take_Give(Sem);
      Success : Boolean;
   begin
      Completed := False;
      Base_Sem.Take(Sem.all);

      Base_Sem.Try_To_Take(Sem.all, Success);
      if (not Success) then
         raise Semaphore_Error;
      end if;

      Base_Sem.Give(Sem.all);
      Base_Sem.Give(Sem.all);

      -- Semaphore should be free, try to give it and catch the exception.
      begin
         Base_Sem.Give(Sem.all);
         raise Semaphore_Error;
      exception
         when Tasking_Error =>
            null;
      end;

      -- Take the semaphore so task1 will block.
      Base_Sem.Take(Sem.all);

      -- Now start of task1.
      Task1.Ready;
      -- Cause a task switch so the other task will run.
      delay 0.0;

      -- He shouldn't have the semaphore yet.
      if (Completed) then
         raise Semaphore_Error;
      end if;

      -- Now let task1 run.
      Base_Sem.Give(Sem.all);
      -- Cause a task switch so the other task will run.
      delay 0.0;

      -- Task1 should have taken the semaphore now.
      if (not Completed) then
         raise Semaphore_Error;
      end if;

      -- Now Task1 has the semaphore, try to give it and catch the
      -- exception.
      begin
         Base_Sem.Give(Sem.all);
         raise Semaphore_Error;
      exception
         when Tasking_Error =>
            null;
      end;

      -- Now tell task1 to give up the semaphore.
      Task1.Give_It;
      delay 0.0;
   end Test_Nested;


   procedure Test_Prio (Sem : in Base_Sem.Object_Class) is
      Task1     : Take_Give(Sem);
      Orig_Prio : System.Any_Priority
                    := Ada.Dynamic_Priorities.Get_Priority;
      New_Prio  : System.Any_Priority;
   begin
      -- Take the semaphore here.  My priority should remain the same.
      Base_Sem.Take(Sem.all);
      if (Ada.Dynamic_Priorities.Get_Priority /= Orig_Prio) then
         raise Semaphore_Error;
      end if;

      -- Let task1 run at a higher priority than me and let him try to take
      -- the semphore.
      Ada.Dynamic_Priorities.Set_Priority(Orig_Prio + 1, Task1'Identity);
      Task1.Ready;
      -- Cause the other task to run.  In GNAT (and on Unix in general),
      -- priorities don't work unless you are root, but the priority
      -- reported by Ada still works.
      delay 0.0;

      -- I should have inherited task1's priority.
      New_Prio := Ada.Dynamic_Priorities.Get_Priority;
      if (New_Prio /= (Orig_Prio + 1)) then
         Put_Line("Priority was" & Integer'Image(New_Prio)
                  & " and should have been" & Integer'Image(Orig_Prio + 1));
         raise Semaphore_Error;
      end if;

      Base_Sem.Give(Sem.all);
      -- Cause the other task to run.  In GNAT (and on Unix in general),
      -- priorities don't work unless you are root, but the priority
      -- reported by Ada still works.
      delay 0.0;

      -- I've given up the semaphore, so my priority should be back.
      if (Ada.Dynamic_Priorities.Get_Priority /= Orig_Prio) then
         raise Semaphore_Error;
      end if;

      -- Now tell task1 to give up the semaphore.
      Task1.Give_It;
   end Test_Prio;

begin
   -- Testing binary operations on binary semaphores
   Test_Binary(Base_Sem.Object_Class(Bin_Sem_Ptr));
   -- Testing binary operations on counting semaphores
   Test_Binary(Base_Sem.Object_Class(Count_Sem_Ptr));
   -- Testing counting semaphores
   Test_Count(Base_Sem.Object_Class(Count_Sem_Ptr));
   -- Testing nested semaphores
   Test_Nested(Base_Sem.Object_Class(Nest_Sem_Ptr));
   -- Testing nested operations on priority semaphores
   Test_Nested(Base_Sem.Object_Class(Nest_Prio_Sem_Ptr));
   -- Testing priority inheritance semaphores
   Test_Prio(Base_Sem.Object_Class(Nest_Prio_Sem_Ptr));

   Put_Line("Tests passed");
   Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception
   when others =>
      Put_Line("*** Tests failed");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      raise;
end Test_Semaphore;
