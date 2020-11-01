-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
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

with Asl.Date_Time.Calendar; use Asl.Date_Time.Calendar;

-- Set the default timezone in Asl.Date_Time.Timezone to the current
-- local timezone.  If the local timezone cannot be determined, then
-- this sets the timezone to UTC.
package Asl.Date_Time.Local is

   -- Attempt to determine the local timezone and set it as the default
   -- in Asl.Date_Time.Timezone.
   procedure Set_Local_Timezone;

   -- Return the current local time (in the default timezone).
   function Get_Local_Time return Calendar_Time;

   -- Return the current UTC time.
   function Get_UTC_Time return Calendar_Time;

end Asl.Date_Time.Local;
