
-- Count the number of of each word in the standard input and print the
-- words with the maximum 10 counts along with the counts themselves.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use type Ada.Strings.Unbounded.Unbounded_String;
with Word_Parser;
with Asu_Components;
with Ada.Unchecked_Deallocation;

procedure Count_Words is
   package Asu renames Ada.Strings.Unbounded;
   use type Asu_Components.Str_Comp.End_Marker;
   use type Asu_Components.Count_Comp.End_Marker;

   procedure Free is new Ada.Unchecked_Deallocation
     (Asu_Components.Word_Counter,
      Asu_Components.Word_Counter_Ptr);

   -- An associative array containing the words, along with an iterator for
   -- it.
   Word_Array       : Asu_Components.Asoc_Array.Object_Class
     := new Asu_Components.Asoc_Array.Object (Allow_Duplicates => False,
                                              Node_Size        => 10);
   Word_Array_Iter  : Asu_Components.Asoc_Array.Iterator
     := Asu_Components.Asoc_Array.New_Iterator(Word_Array);
   Array_Is_End     : Asu_Components.Str_Comp.End_Marker;

   -- Stuff to hold values in the associative array and the count array.
   Val              : aliased Asu_Components.Word_Counter;
   Val_Ptr          : Asu_Components.Word_Counter_Ptr;
   Word_Found       : Boolean;
   File_Done        : Boolean;

   -- An array that will sort by count, along with it's iterator.  This is
   -- created later once the number of words is known.
   Count_Sort       : Asu_Components.Count_Sorter.Object_Class;
   Count_Sort_Iter  : Asu_Components.Count_Sorter.Iterator;
   Count_Is_End     : Asu_Components.Count_Comp.End_Marker;

   Num_Words        : Natural;
begin
   Word_Parser.Get_Next_Word(Standard_Input, Val.Str, Word_Found, File_Done);
   while (Word_Found) loop
      -- Attempt to find the word in the container.
      Asu_Components.Asoc_Array.Search(Word_Array_Iter,
                                       Val'Unchecked_Access,
                                       Word_Found);
      if (Word_Found) then
         -- We found the word, just increment the count.
         Val_Ptr := Asu_Components.Asoc_Array.Get(Word_Array_Iter);
         Val_Ptr.Count := Val_Ptr.Count + 1;
      else
         -- We didn't find the word, add it.
         Val_Ptr := new Asu_Components.Word_Counter;
         Val_Ptr.Str := Val.Str;
         Val_Ptr.Count := 1;
         Asu_Components.Asoc_Array.Add(Word_Array.all, Val_Ptr);
      end if;

      exit when File_Done;

      Word_Parser.Get_Next_Word(Standard_Input,
                                Val.Str,
                                Word_Found,
                                File_Done);
   end loop;

   -- Now create a fixed-size container to hold all the words.
   Num_Words := Asu_Components.Asoc_Array.Member_Count(Word_Array.all);
   Count_Sort := new Asu_Components.Count_Sorter.Object
                      (Balanced => True,
                       Size     => Num_Words);
   Count_Sort_Iter := Asu_Components.Count_Sorter.New_Iterator
                       (Count_Sort);

   -- Iterate through the whole container, pull the values out of the
   -- associative array, and put them into the counter.
   Asu_Components.Asoc_Array.First(Word_Array_Iter, Array_Is_End);
   while (Array_Is_End = Asu_Components.Str_Comp.Not_Past_End) loop
      Val_Ptr := Asu_Components.Asoc_Array.Get(Word_Array_Iter);
      Asu_Components.Count_Sorter.Add(Count_Sort.all, Val_Ptr.all);
      Asu_Components.Asoc_Array.Delete(Word_Array_Iter, Array_Is_End);
      Free(Val_Ptr);
   end loop;

   -- Now iterate through the counter from the end and take the top ten
   -- values, since they will be at the end of the container.
   Asu_Components.Count_Sorter.Last(Count_Sort_Iter, Count_Is_End);
   for I in 1 .. 10 loop
      exit when (Count_Is_End = Asu_Components.Count_Comp.Past_End);

      Asu_Components.Count_Sorter.Get_Decr(Count_Sort_Iter,
                                           Val,
                                           Count_Is_End);
      Put_Line(Asu.To_String(Val.Str)
               & " "
               & Positive'Image(Val.Count));
   end loop;
end Count_Words;
