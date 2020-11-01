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

package body State_Graph is

   -- The graph of states is a list of the states in the US.  The graph has
   -- a connection between states for every state that is adjacent.  The
   -- following subprograms manage that map.

   -- The subprograms below mostly use the standard graph functions for
   -- what they do.  See the spec file for the descriptions of what these
   -- do.

   procedure Find (Iter  : in out Iterator;
                   Name  : in String;
                   Found : out Boolean) is
      Local_Name  : aliased String := Name;
      Local_State : aliased State;
   begin
      Local_State.Name := Local_Name'Unchecked_Access;
      The_State_Graph.Search(Iter.Iter, Local_State'Unchecked_Access, Found);
   end Find;

   procedure First (Iter   : in out Iterator;
                    Is_End : out End_Marker) is
   begin
      The_State_Graph.First(Iter.Iter, Is_End);
   end First;

   procedure Next (Iter   : in out Iterator;
                   Is_End : out End_Marker) is
   begin
      The_State_Graph.Next(Iter.Iter, Is_End);
   end Next;

   procedure First_Connection (Iter : in out Iterator;
                               Is_End : out End_Marker) is
   begin
      The_State_Graph.First_Link(Iter.Iter, Is_End);
   end First_Connection;

   procedure Next_Connection (Iter : in out Iterator;
                              Is_End : out End_Marker) is
   begin
      The_State_Graph.Next_Link(Iter.Iter, Is_End);
   end Next_Connection;

   procedure Find_Connection (Iter  : in out Iterator;
                              Dest  : in Iterator;
                              Found : out Boolean) is
   begin
      The_State_Graph.Find_Link(Iter.Iter, Dest.Iter, Found);
   end Find_Connection;

   procedure Find_Connection (Iter  : in out Iterator;
                              Name  : in String;
                              Found : out Boolean) is
      Local_Name  : aliased String := Name;
      Local_State : aliased State;
      Dest        : The_State_Graph.Iterator := Iter.Iter;
      Local_Found : Boolean;
   begin
      Local_State.Name := Local_Name'Unchecked_Access;
      The_State_Graph.Search(Dest, Local_State'Unchecked_Access, Local_Found);
      if (not Local_Found) then
         Found := False;
      else
         The_State_Graph.Find_Link(Iter.Iter, Dest, Found);
      end if;
   end Find_Connection;

   function Is_Connected (Iter1, Iter2 : in Iterator) return Boolean is
   begin
      return The_State_Graph.Link_Exists(Iter1.Iter, Iter2.Iter);
   end Is_Connected;

   function Is_Connected (Name1, Name2 : in String) return Boolean is
      Local_Name1  : aliased String := Name1;
      Local_State1 : aliased State;
      Local_Name2  : aliased String := Name2;
      Local_State2 : aliased State;
      Source       : The_State_Graph.Iterator;
      Dest         : The_State_Graph.Iterator;
      Local_Found  : Boolean;
   begin
      Local_State1.Name := Local_Name1'Unchecked_Access;
      The_State_Graph.Search(Source,
                             Local_State1'Unchecked_Access,
                             Local_Found);
      if (not Local_Found) then
         return False;
      end if;

      Local_State2.Name := Local_Name2'Unchecked_Access;
      The_State_Graph.Search(Dest,
                             Local_State2'Unchecked_Access,
                             Local_Found);
      if (not Local_Found) then
         return False;
      end if;

      The_State_Graph.Find_Link(Source, Dest, Local_Found);
      return Local_Found;
   end Is_Connected;

   function Follow_Connection (Iter : in Iterator)
                               return Iterator is
      Retval : Iterator := Iter;
   begin
      Retval.Iter := The_State_Graph.Follow_Link(Iter.Iter);
      return Retval;
   end Follow_Connection;

   procedure Follow_Connection (Iter : in out Iterator) is
   begin
      The_State_Graph.Follow_Link(Iter.Iter);
   end Follow_Connection;

   function Get_Name (Iter : in Iterator) return String is
   begin
      return The_State_Graph.Get(Iter.Iter).Name.all;
   end Get_Name;


   function Hash_State (Val : in State_Class) return Natural is
      Retval : Natural := 0;
   begin
      for I in Val.Name.all'Range loop
         Retval := Retval + Character'Pos(Val.Name.all(I));
      end loop;

      return Retval;
   end Hash_State;

   function "=" (Val1, Val2 : in State_Class) return Boolean is
   begin
      return Val1.Name.all = Val2.Name.all;
   end "=";


   -- Some types to hold connections between states.
   type State_Connection_Entry is record
      State1 : String_Ptr;
      State2 : String_Ptr;
   end record;
   type State_Connection_Array is array (Positive range <>)
     of State_Connection_Entry;

   -- Now all the states in the US.
   Maine          : String_Ptr := new String'("Maine");
   Vermont        : String_Ptr := new String'("Vermont");
   New_Hampshire  : String_Ptr := new String'("New Hampshire");
   Massachusetts  : String_Ptr := new String'("Massachusetts");
   Connecticut    : String_Ptr := new String'("Connecticut");
   New_York       : String_Ptr := new String'("New York");
   New_Jersey     : String_Ptr := new String'("New Jersey");
   Delaware       : String_Ptr := new String'("Delaware");
   Pennsylvania   : String_Ptr := new String'("Pennsylvania");
   Maryland       : String_Ptr := new String'("Maryland");
   West_Virginia  : String_Ptr := new String'("West Virginia");
   Virginia       : String_Ptr := new String'("Virginia");
   North_Carolina : String_Ptr := new String'("North Carolina");
   South_Carolina : String_Ptr := new String'("South Carolina");
   Georgia        : String_Ptr := new String'("Georgia");
   Florida        : String_Ptr := new String'("Florida");
   Ohio           : String_Ptr := new String'("Ohio");
   Michigan       : String_Ptr := new String'("Michigan");
   Indiana        : String_Ptr := new String'("Indiana");
   Kentucky       : String_Ptr := new String'("Kentucky");
   Tennessee      : String_Ptr := new String'("Tennessee");
   Alabama        : String_Ptr := new String'("Alabama");
   Mississippi    : String_Ptr := new String'("Mississippi");
   Wisconsin      : String_Ptr := new String'("Wisconsin");
   Illinois       : String_Ptr := new String'("Illinois");
   Minnesota      : String_Ptr := new String'("Minnesota");
   Iowa           : String_Ptr := new String'("Iowa");
   Missouri       : String_Ptr := new String'("Missouri");
   Arkansas       : String_Ptr := new String'("Arkansas");
   North_Dakota   : String_Ptr := new String'("North Dakota");
   South_Dakota   : String_Ptr := new String'("South Dakota");
   Nebraska       : String_Ptr := new String'("Nebraska");
   Kansas         : String_Ptr := new String'("Kansas");
   Oklahoma       : String_Ptr := new String'("Oklahoma");
   Texas          : String_Ptr := new String'("Texas");
   Montana        : String_Ptr := new String'("Montana");
   Idaho          : String_Ptr := new String'("Idaho");
   Wyoming        : String_Ptr := new String'("Wyoming");
   Colorado       : String_Ptr := new String'("Colorado");
   Utah           : String_Ptr := new String'("Utah");
   New_Mexico     : String_Ptr := new String'("New Mexico");
   Arizona        : String_Ptr := new String'("Arizona");
   Washington     : String_Ptr := new String'("Washington");
   Oregon         : String_Ptr := new String'("Oregon");
   California     : String_Ptr := new String'("California");
   Nevada         : String_Ptr := new String'("Nevada");
   Rhode_Island   : String_Ptr := new String'("Rhode Island");
   Louisiana      : String_Ptr := new String'("Louisiana");
   Alaska         : String_Ptr := new String'("Alaska");
   Hawaii         : String_Ptr := new String'("Hawaii");

   -- This array holds a list of connections between states.  The order is
   -- arbitrary.  I have no delusion that this is complete, it is probably
   -- missing some adjacencies.
   Connections : State_Connection_Array := (
        1 => (Maine,          Vermont),
        2 => (Vermont,        New_Hampshire),
        3 => (Vermont,        Massachusetts),
        4 => (New_Hampshire,  New_York),
        5 => (New_Hampshire,  Massachusetts),
        6 => (Massachusetts,  Rhode_Island),
        7 => (Massachusetts,  Connecticut),
        8 => (Massachusetts,  New_York),
        9 => (Connecticut,    Rhode_Island),
       10 => (New_York,       New_Jersey),
       11 => (New_York,       Pennsylvania),
       12 => (New_Jersey,     Delaware),
       13 => (Delaware,       Maryland),
       14 => (Pennsylvania,   Maryland),
       15 => (Pennsylvania,   Delaware),
       16 => (Pennsylvania,   Ohio),
       17 => (Pennsylvania,   West_Virginia),
       18 => (Maryland,       West_Virginia),
       19 => (Maryland,       Virginia),
       20 => (West_Virginia,  Virginia),
       21 => (West_Virginia,  Ohio),
       22 => (West_Virginia,  Kentucky),
       23 => (Virginia,       Kentucky),
       24 => (Virginia,       North_Carolina),
       25 => (Virginia,       Tennessee),
       26 => (North_Carolina, South_Carolina),
       27 => (North_Carolina, Tennessee),
       28 => (North_Carolina, Georgia),
       29 => (South_Carolina, Georgia),
       30 => (Georgia,        Alabama),
       31 => (Georgia,        Florida),
       32 => (Georgia,        Tennessee),
       33 => (Florida,        Alabama),
       34 => (Ohio,           Kentucky),
       35 => (Ohio,           Indiana),
       36 => (Ohio,           Michigan),
       37 => (Michigan,       Indiana),
       38 => (Michigan,       Wisconsin),
       39 => (Indiana,        Illinois),
       40 => (Indiana,        Kentucky),
       41 => (Kentucky,       Tennessee),
       42 => (Kentucky,       Illinois),
       43 => (Kentucky,       Missouri),
       44 => (Tennessee,      Missouri),
       45 => (Tennessee,      Arkansas),
       46 => (Tennessee,      Alabama),
       47 => (Tennessee,      Mississippi),
       48 => (Alabama,        Mississippi),
       49 => (Mississippi,    Arkansas),
       50 => (Mississippi,    Louisiana),
       51 => (Wisconsin,      Minnesota),
       52 => (Wisconsin,      Iowa),
       53 => (Wisconsin,      Illinois),
       54 => (Illinois,       Iowa),
       55 => (Illinois,       Missouri),
       56 => (Minnesota,      North_Dakota),
       57 => (Minnesota,      South_Dakota),
       58 => (Minnesota,      Iowa),
       59 => (Iowa,           South_Dakota),
       60 => (Iowa,           Nebraska),
       61 => (Iowa,           Missouri),
       62 => (Missouri,       Nebraska),
       63 => (Missouri,       Kansas),
       64 => (Missouri,       Oklahoma),
       65 => (Missouri,       Arkansas),
       66 => (Arkansas,       Oklahoma),
       67 => (Arkansas,       Texas),
       68 => (Arkansas,       Louisiana),
       69 => (North_Dakota,   Montana),
       70 => (North_Dakota,   South_Dakota),
       71 => (South_Dakota,   Montana),
       72 => (South_Dakota,   Wyoming),
       73 => (South_Dakota,   Nebraska),
       74 => (Nebraska,       Wyoming),
       75 => (Nebraska,       Colorado),
       76 => (Nebraska,       Kansas),
       77 => (Kansas,         Colorado),
       78 => (Kansas,         Oklahoma),
       79 => (Oklahoma,       Colorado),
       80 => (Oklahoma,       New_Mexico),
       81 => (Oklahoma,       Texas),
       82 => (Texas,          New_Mexico),
       83 => (Montana,        Idaho),
       84 => (Montana,        Wyoming),
       85 => (Idaho,          Wyoming),
       86 => (Idaho,          Washington),
       87 => (Idaho,          Oregon),
       88 => (Idaho,          Nevada),
       89 => (Idaho,          Utah),
       90 => (Wyoming,        Utah),
       91 => (Wyoming,        Colorado),
       92 => (Colorado,       New_Mexico),
       93 => (Colorado,       Utah),
       94 => (Utah,           Nevada),
       95 => (Utah,           Arizona),
       96 => (Utah,           New_Mexico),
       97 => (New_Mexico,     Arizona),
       98 => (Arizona,        Nevada),
       99 => (Arizona,        California),
      100 => (Washington,     Oregon),
      101 => (Oregon,         Nevada),
      102 => (Oregon,         California),
      103 => (California,     Nevada),
      104 => (Louisiana,      Texas),
      105 => (Colorado,       Arizona));

begin
   declare
      Iter1     : The_State_Graph.Iterator
                   := The_State_Graph.New_Iterator(Graph'Access);
      Iter2     : The_State_Graph.Iterator
                   := The_State_Graph.New_Iterator(Graph'Access);
      Found     : Boolean;
      Tmp_State : aliased State;
   begin
      -- Loop through all the connections and add them.
      for I in Connections'Range loop
         -- Look for the first state.  If it doesn't exist then add it.
         Tmp_State.Name := Connections(I).State1;
         The_State_Graph.Search(Iter1, Tmp_State'Unchecked_Access, Found);
         if (not Found) then
            The_State_Graph.Add(Graph, new State'(Tmp_State));
         end if;

         -- Look for the second state.  If it doesn't exist then add it.
         Tmp_State.Name := Connections(I).State2;
         The_State_Graph.Search(Iter2, Tmp_State'Unchecked_Access, Found);
         if (not Found) then
            The_State_Graph.Add(Graph, new State'(Tmp_State));
         end if;

         -- Now get tw iterators referencing the two states.
         Tmp_State.Name := Connections(I).State1;
         The_State_Graph.Search(Iter1, Tmp_State'Unchecked_Access, Found);
         Tmp_State.Name := Connections(I).State2;
         The_State_Graph.Search(Iter2, Tmp_State'Unchecked_Access, Found);

         -- Add the link between the states.
         The_State_Graph.Add_Link(Iter1, Iter2, False);
      end loop;

      -- Alaska and Hawaii hove no links, so add them separately.
      Tmp_State.Name := Alaska;
      The_State_Graph.Add(Graph, new State'(Tmp_State));
      Tmp_State.Name := Hawaii;
      The_State_Graph.Add(Graph, new State'(Tmp_State));
   end;
end State_Graph;
