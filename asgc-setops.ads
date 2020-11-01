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

generic
package Asgc.Setops is

   -- Return the number of time the specific value is in the container.
   function Entry_Count (O   : in Object_Class;
                         Val : in Contained_Type)
                         return Natural;

   -- Perform a set union of the two containers, putting the result in the
   -- Dest container.  These do not have to be the same container types.
   -- Note that if the container supports multiple entries of the
   -- same value, the results of the call are unpredictable.  The values
   -- passed in may reference the same containers, the call handles that.
   procedure Union (Dest   : in Object_Class;
                    O1, O2 : in Object_Class);

   -- Perform a set intersection of the two containers, putting the result
   -- in the Dest container.  These do not have to be the same container
   -- types.  Note that if the container supports multiple entries of the
   -- same value, the results of the call are unpredictable.  The values
   -- passed in may reference the same containers, the call handles that.
   procedure Intersection (Dest   : in Object_Class;
                           O1, O2 : in Object_Class);

   -- Perform a bag union of two containers, putting the result in the Dest
   -- container.  A bag union allows duplicates and will basically be the
   -- combination of all entries in O1 and O2.  If the Dest container does
   -- not support duplicate values, this call will probably cause an
   -- exception to be raised.  The values passed in may reference the same
   -- containers, the call handles that.
   procedure Bag_Union (Dest   : in Object_Class;
                        O1, O2 : in Object_Class);

   -- Perform a bag intersection of two containers, putting the result in
   -- the Dest container.  A bag intersection allows duplicates and the
   -- result for each entry in the destination will be the minimum number
   -- of the value in O1 or O2.  If the Dest container does not
   -- support duplicate values, this call will probably cause an exception
   -- to be raised.  The values passed in may reference the same
   -- containers, the call handles that.
   procedure Bag_Intersection (Dest   : in Object_Class;
                               O1, O2 : in Object_Class);

   -- Return True if O1 is a subset of O2, meaning that all the members of
   -- O1 are in O2.
   function Is_Subset (O1, O2 : in Object_Class) return Boolean;

   -- Return True if O1 is a bag subset of O2, meaning that for every
   -- member of O1, O2 has equal or more members.
   function Is_Bag_Subset (O1, O2 : in Object_Class) return Boolean;

end Asgc.Setops;
