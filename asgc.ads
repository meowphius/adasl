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

with Baseclass;

generic

   -- This is the type being contained in the container.
   type Contained_Type is private;

   -- An equivalence function, if necessary, can be supplied.
   with function "=" (V1, V2 : in Contained_Type) return Boolean is <>;

package Asgc is

   ------------------------------------------------------------------------
   -- All container objects derive from this class.
   type Object is abstract new Baseclass.Object with private;
   type Object_Class is access all Object'Class;

   -- Add a value to the container.  How this is done is
   -- container-specific.  If the container does not support duplicates
   -- and the value is already in the container, Item_Already_Exists
   -- will be raised.
   procedure Add (O   : in out Object;
                  Val : in Contained_Type)
      is abstract;

   -- Delete a value to the container.  If the container can hold
   -- more than one of the same value, only one of the items is
   -- deleted.  If the item is not in the container, then
   -- Item_Not_Found will be raised.
   procedure Delete (O   : in out Object;
                     Val : in Contained_Type)
      is abstract;

   -- Return True if the value is in the container and False if not.  Boy,
   -- I'd like it if I could define an "in" operator for this.
   function Value_Exists (O   : in Object;
                          Val : in Contained_Type)
                          return Boolean
      is abstract;

   -- Return the number of items in the container.
   function Member_Count (O : in Object)
                          return Natural
      is abstract;

   -- Compare two containers to see if they are the same.  The meaning
   -- of "same" depends on the specific container.
   function "=" (O1, O2 : in Object) return Boolean is abstract;

   -- Verify the integrity of the container's data structures.  This will
   -- raise exceptions if the container has integrity problems, otherwise
   -- it will just return.
   procedure Verify_Integrity (O : in Object) is abstract;

   -- Generate an exact copy of a container.
   function Copy (O : in Object) return Object_Class
      is abstract;


   ------------------------------------------------------------------------
   -- A callback routine that can be provided to an object to tell when
   -- objects are added or deleted from the collection;
   type Callbacks is abstract new Baseclass.Object with private;
   type Callbacks_Class is access all Callbacks'Class;

   -- A value was added to the container.
   procedure Added (Cb  : access Callbacks;
                    O   : in Object'Class;
                    Val : in out Contained_Type)
      is abstract;

   -- A value was copied to a new container.
   procedure Copied (Cb  : access Callbacks;
                     O   : in Object'Class;
                     Val : in out Contained_Type)
      is abstract;

   -- A value was deleted from a container.
   procedure Deleted (Cb  : access Callbacks;
                      O   : in Object'Class;
                      Val : in out Contained_Type)
      is abstract;

   -- Set the callbacks for an object.  Setting it to null will turn the
   -- callbacks off.
   procedure Set_Callbacks (O  : in out Object;
                            Cb : in Callbacks_Class);

   -- Call the Operate procedure supplied for every item in the container.
   generic
      with procedure Operate (Val : in Contained_Type);
   procedure Generic_For_All (O : in Object_Class);


   ------------------------------------------------------------------------
   -- An object that can iterate through another object.  A container can
   -- have more than one iterator operating on it at any point in time.
   -- Note that an update to a container will invalidate all iterators
   -- using that container except the one through which the change came.
   type Iterator is abstract new Baseclass.Object with private;
   type Iterator_Class is access all Iterator'Class;

   -- Return an iterator for a container.  It will be dynamically
   -- allocated and must be freed with the "Free" function below.
   function New_Iterator (O : access Object) return Iterator_Class
      is abstract;

   -- Free an iterator allocated with New_Iterator.
   procedure Free (Iter : access Iterator)
      is abstract;

   -- Set the container for an iterator.  Before the container is set, the
   -- iterator cannot be used.  After it is set, the iterator must still be
   -- positioned, it will be at an invalid position after this call.
   procedure Set_Container (Iter : in out Iterator;
                            O    : in Object_Class)
      is abstract;

   -- Add a value to the container the iterator references and move the
   -- iterator to the newly added item's position.
   procedure Add (Iter : in out Iterator;
                  Val  : in Contained_Type)
      is abstract;

   -- Functions to do simple iteration through a container.  All containers
   -- will support this, but efficiency obviously varies with different
   -- container types.
   type End_Marker is (Past_End, Not_Past_End);

   -- If First returns Past_End in Is_End, then the container is empty and
   -- iterator will not be valid.
   procedure First (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;

   -- If Next returns Past_End in Is_End, then the iterator will not be
   -- moved, the entry at call time was the last item.
   procedure Next (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;


   -- Delete the item from the container that the iterator points to.  All
   -- other iterators but this one will be invalidated.  If the contained
   -- object is the last object in the container, then Is_End will be
   -- set to Past_End and the iterator will be invalid.  Otherwise,
   -- Not_Past_Pnd will be returned and the iterator will point to the
   -- next object after the one deleted.
   procedure Delete (Iter : in out Iterator; Is_End : out End_Marker)
      is abstract;


   -- Do two iterators point to the same element of the same object?
   function Is_Same (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   -- Return the value the iterator points to.
   function Get (Iter : in Iterator) return Contained_Type
      is abstract;

   -- Get the current value and move to the next position.  If the iterator
   -- points to the last position, the iterator will be set invalid and
   -- Is_End set to Past_End.  Otherwise, Is_End will be set to Not_Past_End.
   -- A valid value is returned in either case
   procedure Get_Incr (Iter   : in out Iterator;
                       Val    : out Contained_Type;
                       Is_End : out End_Marker)
      is abstract;

   -- Are the items the two iterators point to equal (tested by the "="
   -- function provided generically?
   function "=" (Iter1, Iter2 : in Iterator) return Boolean
      is abstract;

   -- Is the item the iterator points to equal to the supplied item?
   function "=" (Iter : in Iterator; Val : in Contained_Type) return Boolean
      is abstract;
   function "=" (Val : in Contained_Type; Iter : in Iterator) return Boolean
      is abstract;

   -- Search for a value in the conatiner for the given value.  This is
   -- implemented as a linear search in this package, but other functions
   -- are free to reimplement this.
   procedure Search (Iter  : in out Iterator;
                     Val   : in Contained_Type;
                     Found : out Boolean);

   -- Search for the value in the container from the position following the
   -- iterator to the end of the container.  The value searched for is the
   -- value the iterater currently references, so this can be used with the
   -- Search method to find all items in the container of the specific value.
   procedure Search_Again (Iter  : in out Iterator;
                           Found : out Boolean);


   ------------------------------------------------------------------------
   -- Exceptions for various operations

   -- The iterator is invalid.  Either the iterator was not initialized
   -- or the position of the iterator was not valid.
   Invalid_Iterator : exception;

   -- The iterators performed an operations that required them to point
   -- to the same container but the containers were different.
   Iterator_Mismatch : exception;

   -- An operation was attempted on an object that has been freed
   Object_Free : exception;

   -- An operation was attempted on an iterator that has been freed
   Iterator_Free : exception;

   -- The container has changed since the last time the iterator was set.
   Object_Updated : exception;

   -- A comparison of contained types that do not support comparison.
   Invalid_Compare : exception;

   -- An item was not found in the container
   Item_Not_Found : exception;

   -- An item was already in the container
   Item_Already_Exists : exception;

   -- A container has filled up and the item being added will not fit.
   Container_Full : exception;

private

   type Update_Count is mod 2 ** 32;

   Invalid_Update : constant Update_Count := 0 - 1;

   type Object is abstract new Baseclass.Object with record
      Cb      : Callbacks_Class := null;
      Update  : Update_Count    := 0;
      Is_Free : Boolean         := False;
   end record;

   type Iterator is abstract new Baseclass.Object with record
      Update  : Update_Count := Invalid_Update;
      Is_Free : Boolean      := False;
   end record;

   type Callbacks is abstract new Baseclass.Object with null record;

end Asgc;
