
with Asgc.Btree.Dynamic;
with Asgc.Tree.Fixed;
with Ada.Strings.Unbounded; use type Ada.Strings.Unbounded.Unbounded_String;

package Asu_Components is

   package Asu renames Ada.Strings.Unbounded;

   type Word_Counter is record
      Str   : Asu.Unbounded_String;
      Count : Positive;
   end record;
   type Word_Counter_Ptr is access all Word_Counter;

   -- Functions to compare two pointers to the previous record, since these
   -- are required for the Btree module.  Also, we don't want to compare
   -- the count value since we will be changing it.
   function "=" (V1, V2 : in Word_Counter_Ptr) return Boolean;
   function ">" (V1, V2 : in Word_Counter_Ptr) return Boolean;
   function "<" (V1, V2 : in Word_Counter_Ptr) return Boolean;
   function ">=" (V1, V2 : in Word_Counter_Ptr) return Boolean;
   function "<=" (V1, V2 : in Word_Counter_Ptr) return Boolean;

   -- An associate array implemented with a Btree.  We use a pointer to the
   -- record instead of the record itself so it is easy to update the count
   -- in the record.
   package Str_Comp is new Asgc(Contained_Type => Word_Counter_Ptr);
   package Str_Btree is new Str_Comp.Btree;
   package Asoc_Array is new Str_Btree.Dynamic;


   -- Functions to compare two of the previous records, but it compares the
   -- count value first then the string value.  This will order a container
   -- by count first then by name.
   function Count_Eq (V1, V2 : in Word_Counter) return Boolean;
   function Count_Gt (V1, V2 : in Word_Counter) return Boolean;
   function Count_Lt (V1, V2 : in Word_Counter) return Boolean;
   function Count_Ge (V1, V2 : in Word_Counter) return Boolean;
   function Count_Le (V1, V2 : in Word_Counter) return Boolean;

   -- Now create a fixed-sized tree that sorts by count.
   package Count_Comp is new Asgc(Contained_Type => Word_Counter,
                                  "="            => Count_Eq);
   package Count_Tree is new Count_Comp.Tree
     (">"  => Count_Gt,
      "<"  => Count_Lt,
      ">=" => Count_Ge,
      "<=" => Count_Le);
   package Count_Sorter is new Count_Tree.Fixed;

end Asu_Components;
