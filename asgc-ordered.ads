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
   with function "=" (V1 : in Contained_Type; V2 : in Contained_Type)
                      return Boolean is <>;
package Asgc.Ordered is

   ------------------------------------------------------------------------
   -- An ordered container objects.  These objects have members that
   -- have a specific numeric position.
   subtype Parent_Object is Object;
   type Object is abstract new Parent_Object with private;
   type Object_Class is access all Object'Class;

   -- GNAT makes us add this, but I don't think it is required.
   function "=" (O1, O2 : in Object) return Boolean is abstract;

   -- The inherited function to add a member generically into the
   -- container.  This will add it to the tail.
   procedure Add (O : in out Object; Val : in Contained_Type);

   -- Add a member to the container at the specified location.  All other
   -- members at that location with greater positions will have their
   -- position increased by one.  All iterators will be invalidated by this
   -- operation.
   procedure Add_At (O   : in out Object;
                     Loc : in Positive;
                     Val : in Contained_Type)
      is abstract;

   -- Set a member position in the container to a specific value.
   procedure Set_At (O   : in out Object;
                     Loc : in Positive;
                     Val : in Contained_Type)
      is abstract;

   -- Return the value at a specific location.
   function Get_At (O   : in Object;
                    Loc : in Positive)
                    return Contained_Type
      is abstract;

   -- Swap the values at specific locations.
   procedure Swap_At (O          : in out Object;
                      Loc1, Loc2 : in Positive)
      is abstract;

   -- Delete the member at the specified location.  All iterators will be
   -- invalidated by this operation.
   procedure Delete_At (O   : in out Object;
                        Loc : in Positive)
      is abstract;

   -- Stack operations.  Note that when using an ordered container as a
   -- stack, all the locations specified are valid with reference to the
   -- top of the stack.  So, position 1 or first is the stack top and last
   -- is the stack bottom.
   procedure Push (O   : in out Object;
                   Val : in Contained_Type)
      is abstract;

   procedure Pop (O   : in out Object;
                  Val : out Contained_Type)
      is abstract;

   -- Queue operations.  When using an ordered container as a queue, all
   -- the locations specified are referenced from the queue head.  So
   -- position 1 (or First) is the queue head and Last is the queue tail.
   -- Things are put onto the tail and taken off of the head.
   procedure Enqueue (O   : in out Object;
                      Val : in Contained_Type)
      is abstract;

   procedure Dequeue (O   : in out Object;
                      Val : out Contained_Type)
      is abstract;

   ------------------------------------------------------------------------
   -- An iterator for an ordered container.

   subtype Parent_Iterator is Iterator;
   type Iterator is abstract new Parent_Iterator with private;
   type Iterator_Class is access all Iterator'Class;

   -- For some reason, GNAT makes me add this to get it to compile.  I'm
   -- not sure if Ada requires it specifically.  We shall see.
   function "=" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   -- We implement this here because it is pretty generic to all the
   -- containers.
   procedure Add (Iter : in out Iterator;
                  Val  : in Contained_Type);

   -- If Last returns Past_End in Is_End, then the container is empty and
   -- iterator will not be valid.
   procedure Last (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- Move to the previous value in the vector.  If the current position is
   -- the first value, Is_End will be set to Past_End and the iterator will
   -- not be moved.
   procedure Prev (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   procedure Set_Loc (Iter : out Iterator; Loc : in Positive)
      is abstract;

   -- Return the position of the given iterator.
   function Get_Loc (Iter : in Iterator)
                     return Natural
      is abstract;

   -- Return True if iter1 is after iter2 in the sequence, False otherwise.
   function Is_After (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   -- Return True if iter1 is before iter2 in the sequence, False otherwise.
   function Is_Before (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   -- Return position(Iter1) - position(Iter2).
   function "-" (Iter1, Iter2 : in Iterator) return Integer;

   -- Arithmetic on iterators, modify their position by the given offset.
   function "+" (Iter : in Iterator; Offset : in Integer)
                 return Iterator
      is abstract;
   function "-" (Iter : in Iterator; Offset : in Integer)
                 return Iterator
      is abstract;

   -- Swap the values pointed to by iter1 and iter2.
   procedure Swap (Iter1, Iter2 : in out Iterator)
      is abstract;

   -- Add a value before or after the specified iterator.  All other
   -- iterators will be invalidated.  The iterator will point to the newly
   -- added value.
   procedure Add_After (Iter : in out Iterator; Val : in Contained_Type)
      is abstract;
   procedure Add_Before (Iter : in out Iterator; Val : in Contained_Type)
      is abstract;

   -- Set the value of the position the given iterator points to.
   procedure Set (Iter : in Iterator; Val : in Contained_Type)
      is abstract;

   -- Get the current value and move to the previous position.  If the
   -- iterator points to the first position, the iterator will be set
   -- invalid and Is_End set to Past_End.  Otherwise, Is_End will be set to
   -- Not_Past_End.  A valid value is returned in either case
   procedure Get_Decr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker)
      is abstract;

private

   type Object is abstract new Parent_Object with null record;

   type Iterator is abstract new Parent_Iterator with null record;

end Asgc.Ordered;
