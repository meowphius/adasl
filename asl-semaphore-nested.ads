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

with Ada.Task_Identification;

-- A nested semaphore (by its very nature a binary semaphore).  This is
-- much like a binary semaphore, but it allows the same task to take the
-- semaphore multiple times.  That same task must give the semaphore the
-- same number of times to release it.  Only the task that owns the
-- semaphore may give it, if another task tries a Program_Error will be
-- raised.

package Asl.Semaphore.Nested is

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

   package Task_Ident renames Ada.Task_Identification;

   protected type Mutex is
      entry Wait;
      procedure Release;
   private
      Owner : Task_Ident.Task_Id := Task_Ident.Null_Task_Id;
      Count : Natural := 0;
   end Mutex;

   type Object is new Asl.Semaphore.Object with record
      The_Mutex : Mutex;
   end record;

end Asl.Semaphore.Nested;
