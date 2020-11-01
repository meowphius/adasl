--     Copyright (C) 1998  Daniel Gaudry (Daniel.Gaudry@wanadoo.fr)
--     9 av calmels
--     92270 bois colombes  france
--     +33147862334
--     +33608512371

--
--     This library is free software; you can redistribute it and/or
--     modify it under the terms of the GNU Library General Public
--     License as published by the Free Software Foundation; either
--     version 2 of the License, or (at your option) any later version.
--
--     This library is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--     Library General Public License for more details.
--
--     You should have received a copy of the GNU Library General Public
--     License along with this library; if not, write to the Free
--     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
--     MA 02111-1307, USA
--
-- written & designed by d.gaudry
-- Modified by Corey Minyard to work with the ASL containers.

with Ada.Text_IO;
with Demo_Graph1_Package; use Demo_Graph1_Package;
procedure Demo_Graph1 is
   package Graph_Node_IO is new Ada.Text_IO.Enumeration_IO(Graph_Node);
   package Float_IO      is new Ada.Text_IO.Float_IO(Float);
   package Natural_IO    is new Ada.Text_IO.Integer_IO(Natural);
   package I_IO          is new Ada.Text_IO.Integer_IO(Natural);
   package Graph         renames Demo_Graph1_Package.Graph;
   package Base          renames Demo_Graph1_Package.Base;
   use type Base.End_Marker;

   The_Graph          : aliased Graph.Object(Hash_Size => 20);
   A_1                : Attribute_Of_The_Arc  := (1.0  ,111097);
   A_2                : Attribute_Of_The_Arc  := (2.0  ,020598);
   A_3                : Attribute_Of_The_Arc  := (1.5  ,110498);
   A_4                : Attribute_Of_The_Arc  := (9.74 ,080198);
   A_5                : Attribute_Of_The_Arc  := (22.0 ,290598);
   A_6                : Attribute_Of_The_Arc  := (5.0  ,120398);
   A_7                : Attribute_Of_The_Arc  := (3.3  ,290498);
   A_8                : Attribute_Of_The_Arc  := (2.9  ,270398);
   A_9                : Attribute_Of_The_Arc  := (6.4  ,050698);


   procedure Display (The_Graph : access Graph.Object) is
      Iter   : Graph.Iterator;
      OIter  : Graph.Iterator;
      Is_End : Base.End_Marker;
   begin
      Iter := Graph.New_Iterator(Graph.Object_Class(The_Graph));
      Graph.First(Iter, Is_End);
      while (Is_End = Base.Not_Past_End) loop
         Ada.Text_IO.Put("The vertex ");
         Graph_Node_IO.Put(Graph.Get(Iter));
         Ada.Text_IO.Put(" is the origin of ");
         Natural_IO.Put(Graph.Link_Count(Iter), Width => 2);
         if (Graph.Link_Count(Iter) = 1) then
            Ada.Text_IO.Put_Line(" arc.");
         else
            Ada.Text_IO.Put_Line(" arcs.");
         end if;

         Graph.First_Link(Iter, Is_End);
         while (Is_End = Base.Not_Past_End) loop
            Ada.Text_IO.Put("  ");
            Graph_Node_IO.Put(Graph.Get(Graph.Follow_Link(Iter)),
                              Width => 8);
            Ada.Text_IO.Put(" => ");
            Float_IO.Put (Graph.Get_Link(Iter).Weight);
            Ada.Text_IO.Put(", ");
            I_IO.Put (Graph.Get_Link(Iter).Date);
            Ada.Text_IO.New_Line;
            Graph.Next_Link(Iter, Is_End);
         end loop;

         Graph.Next(Iter, Is_End);
      end loop;
   end Display;

begin

   -----------------------------------------------------------------
   --
   -- the graph does not exist yet...
   --
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Heu);

   -----------------------------------------------------------------
   --
   --           |----|
   --           |v_1 |
   --           |----|
   --
   -- V1 := Heu
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Heu,
                  To       => Heu,
                  Contents => A_1);

   -----------------------------------------------------------------
   -- see you can even do unrealistic additions !!!!
   --
   --           |----|<------|
   --           | V1 |      (A1)
   --           |----|-------|
   --
   --
   -- V1 := Heu
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Nicl2);

   -----------------------------------------------------------------
   --
   --
   --           |----|
   --           | V7 |
   --           |----|
   --
   --
   --
   --           |----|<------|
   --           | V1 |      (A1)
   --           |----|-------|
   --
   --
   -- V1 := Heu
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Nicl2,
                  To       => Heu,
                  Contents => A_2);

   -----------------------------------------------------------------
   --
   --
   --           |----|
   --           | V7 |
   --           |----|
   --              |
   --             (A2)
   --              |
   --              V
   --           |----|<------|
   --           | V1 |      (A1)
   --           |----|-------|
   --
   --
   --
   -- V1 := Heu
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Hno3);

   -----------------------------------------------------------------
   --
   --
   --           |----|
   --           | V2 |
   --           |----|
   --
   --
   --           |----|
   --           | V7 |
   --           |----|
   --              |
   --             (A2)
   --              |
   --              V
   --           |----|<------|
   --           | V1 |      (A1)
   --           |----|-------|
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Heu,
                  To       => Hno3,
                  Contents => A_3);

   -----------------------------------------------------------------
   --
   --
   --           |----|
   --       |-->| V2 |
   --       |   |----|
   --      (A3)
   --       |
   --       |   |----|
   --       |   | V7 |
   --       |   |----|
   --       |      |
   --       |     (A2)
   --       |      |
   --       |      V
   --       |   |----|<------|
   --       |<--| V1 |      (A1)
   --           |----|-------|
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Kcn);

   -----------------------------------------------------------------
   --
   --
   --
   --           |----|
   --       |-->| V2 |
   --       |   |----|
   --      (A3)
   --       |
   --       |   |----|
   --       |   | V7 |
   --       |   |----|
   --       |      |
   --       |     (A2)
   --       |      |
   --       |      V
   --       |   |----|<------|        |----|
   --       |<--| V1 |      (A1)      | V5 |
   --           |----|-------|        |----|
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Hno3,
                  To       => Kcn,
                  Contents => A_4);

   -----------------------------------------------------------------
   --
   --
   --
   --           |----|
   --       |-->| V2 |-------------------|
   --       |   |----|                   |
   --      (A3)                          |
   --       |                            |
   --       |   |----|                   |
   --       |   | V7 |                   |
   --       |   |----|                  (A4)
   --       |      |                     |
   --       |     (A2)                   |
   --       |      |                     |
   --       |      V                     v
   --       |   |----|<------|        |----|
   --       <---| V1 |      (A1)      | V5 |
   --           |----|-------|        |----|
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Cucn);

   -----------------------------------------------------------------
   --
   --
   --
   --           |----|
   --       |-->| V2 |-------------------|
   --       |   |----|                   |
   --      (A3)                          |
   --       |                            |
   --       |   |----|                   |             |----|
   --       |   | V7 |                   |             | V4 |
   --       |   |----|                  (A4)           |----|
   --       |      |                     |
   --       |     (A2)                   |
   --       |      |                     |
   --       |      V                     v
   --       |   |----|<------|        |----|
   --       <---| V1 |      (A1)      | V5 |
   --           |----|-------|        |----|
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Kcn,
                  To       => Cucn,
                  Contents => A_5);

   -----------------------------------------------------------------
   --
   --
   --
   --           |----|
   --       |-->| V2 |-------------------|
   --       |   |----|                   |
   --      (A3)                          |
   --       |                            |
   --       |   |----|                   |             |----|
   --       |   | V7 |                   |             | V4 |
   --       |   |----|                  (A4)           |----|
   --       |      |                     |               |
   --       |     (A2)                   |               |
   --       |      |                     |              (A5)
   --       |      V                     v               |
   --       |   |----|<------|        |----|             |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Kcn,
                  To       => Cucn,
                  Contents => A_9);

   -----------------------------------------------------------------
   --
   --
   --
   --           |----|
   --       |-->| V2 |-------------------|
   --       |   |----|                   |
   --      (A3)                          |
   --       |                            |
   --       |   |----|                   |             |----|
   --       |   | V7 |                   |       |->-->| V4 |
   --       |   |----|                  (A4)     |     |----|
   --       |      |                     |      (A9)     |
   --       |     (A2)                   |       |       |
   --       |      |                     |       |      (A5)
   --       |      V                     v       |       |
   --       |   |----|<------|        |----|-->--|       |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Vergnole);

   -----------------------------------------------------------------
   --
   --                                             |----|
   --                                             | V3 |
   --           |----|                            |----|
   --       |-->| V2 |-------------------|
   --       |   |----|                   |
   --      (A3)                          |
   --       |                            |
   --       |   |----|                   |             |----|
   --       |   | V7 |                   |       |->-->| V4 |
   --       |   |----|                  (A4)     |     |----|
   --       |      |                     |      (A9)     |
   --       |     (A2)                   |       |       |
   --       |      |                     |       |      (A5)
   --       |      V                     v       |       |
   --       |   |----|<------|        |----|-->--|       |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V3 := Vergnole
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Cucn,
                  To       => Vergnole,
                  Contents => A_6);

   -----------------------------------------------------------------
   --
   --                                             |----|
   --                                             | V3 |<---------|
   --           |----|                            |----|          |
   --       |-->| V2 |-------------------|                       (A6)
   --       |   |----|                   |                        |
   --      (A3)                          |                        |
   --       |                            |                        |
   --       |   |----|                   |             |----|     |
   --       |   | V7 |                   |       |->-->| V4 |     |
   --       |   |----|                  (A4)     |     |----|-----|
   --       |      |                     |      (A9)     |
   --       |     (A2)                   |       |       |
   --       |      |                     |       |      (A5)
   --       |      V                     v       |       |
   --       |   |----|<------|        |----|-->--|       |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V3 := Vergnole
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Vergnole,
                  To       => Cucn,
                  Contents => A_6);

   -----------------------------------------------------------------
   --
   --                                             |----|
   --                                             | V3 |<---------|
   --           |----|                            |----|>-|       |
   --       |-->| V2 |-------------------|                |       (A6)
   --       |   |----|                   |               (A8)     |
   --      (A3)                          |                |       |
   --       |                            |                V       |
   --       |   |----|                   |             |----|     |
   --       |   | V7 |                   |       |->-->| V4 |     |
   --       |   |----|                  (A4)     |     |----|-----|
   --       |      |                     |      (A9)     |
   --       |     (A2)                   |       |       |
   --       |      |                     |       |      (A5)
   --       |      V                     v       |       |
   --       |   |----|<------|        |----|-->--|       |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V3 := Vergnole
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add_Link(O        => The_Graph,
                  From     => Vergnole,
                  To       => Hno3,
                  Contents => A_7);

   -----------------------------------------------------------------
   --
   --              |-------(A7)--------<---------<|----|
   --              V                              | V3 |<---------|
   --           |----|                            |----|>-|       |
   --       |-->| V2 |-------------------|                |       (A6)
   --       |   |----|                   |               (A8)     |
   --      (A3)                          |                |       |
   --       |                            |                V       |
   --       |   |----|                   |             |----|     |
   --       |   | V7 |                   |       |->-->| V4 |     |
   --       |   |----|                  (A4)     |     |----|-----|
   --       |      |                     |      (A9)     |
   --       |     (A2)                   |       |       |
   --       |      |                     |       |      (A5)
   --       |      V                     v       |       |
   --       |   |----|<------|        |----|-->--|       |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V3 := Vergnole
   -- V4 := Cucn
   -- V5 := Kcn
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Graph.Add(O   => The_Graph,
             Val => Melina);

   -----------------------------------------------------------------
   --
   --              |-------(A7)--------<---------<|----|
   --              V                              | V3 |<---------|
   --           |----|                            |----|>-|       |
   --       |-->| V2 |-------------------|                |       (A6)
   --       |   |----|                   |               (A8)     |
   --      (A3)                          |                |       |
   --       |                            |                V       |
   --       |   |----|                   |             |----|     |
   --       |   | V7 |                   |       |->-->| V4 |     |
   --       |   |----|                  (A4)     |     |----|-----|
   --       |      |                     |      (A9)     |
   --       |     (A2)                   |       |       |
   --       |      |                     |       |      (A5)
   --       |      V                     v       |       |
   --       |   |----|<------|        |----|-->--|       |
   --       <---| V1 |      (A1)      | V5 |             |
   --           |----|-------|        |----|------->------
   --
   --
   --
   --          |----|
   --          | V6 |
   --          |----|
   --
   -- V1 := Heu
   -- V2 := Hno3
   -- V3 := Vergnole
   -- V4 := Cucn
   -- V5 := Kcn
   -- V6 := Melina
   -- V7 := Nicl2
   -----------------------------------------------------------------

   Display (The_Graph'Unchecked_Access);

end Demo_Graph1;
