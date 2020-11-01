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

-- A Graph_Link that keeps its links in a fixed-sized array.
generic
   Size : Positive;
package Asgc.Graph.Links.Fixed is

   subtype Graph_Link_Parent is Links.Graph_Link;
   type Graph_Link is new Graph_Link_Parent with private;
   type Graph_Link_Class is access all Graph_Link'Class;

   procedure Free_Graph_Link (Node : in out Graph_Link);

   procedure Copied_Graph_Link (Node : in out Graph_Link);


   subtype Graph_Link_It_Parent is Links.Graph_Link_It;
   type Graph_Link_It is new Graph_Link_It_Parent with private;

   procedure Set_Node (Iter : in out Graph_Link_It;
                       Node : in Links.Graph_Link_Class);

   procedure Add (Node     : in out Graph_Link;
                  Val      : in Link_Type;
                  Contents : in Link_Contained_Type);

   procedure Delete_First (Node : in out Graph_Link);

   procedure Delete_Val (Node     : in out Graph_Link;
                         Val      : in Link_Type;
                         Instance : in Positive := 1);

   function Get_Pos (Node : in Graph_Link;
                     Pos  : in Positive)
                     return Link_Type;

   function Val_Exists (Node     : in Graph_Link;
                        Val      : in Link_Type;
                        Instance : in Positive := 1)
                        return Boolean;

   procedure Delete (Iter   : in out Graph_Link_It;
                     Is_End : out End_Marker);

   procedure First (Iter   : in out Graph_Link_It;
                    Is_End : out End_Marker);

   procedure Next (Iter   : in out Graph_Link_It;
                   Is_End : out End_Marker);

   procedure Find (Iter     : in out Graph_Link_It;
                   Val      : in Link_Type;
                   Found    : out Boolean;
                   Instance : in Positive := 1);

   procedure Find_Again (Iter  : in out Graph_Link_It;
                         Found : out Boolean);

   function Get_Instance_Number (Iter : in Graph_Link_It)
                                 return Positive;

   function Get_Instance_Number (Node : in Graph_Link;
                                 Pos  : in Positive)
                                 return Positive;

   function Get (Iter : in Graph_Link_It)
                 return Link_Type;

   function Get_Contents (Iter : in Graph_Link_It)
                          return Link_Contained_Type;

   function Get_Contents (Node     : in Graph_Link;
                          Val      : in Link_Type;
                          Instance : in Positive := 1)
                          return Link_Contained_Type;

   function Get_First_Contents (Node : in Graph_Link)
                                return Link_Contained_Type;

   procedure Set_Contents (Iter     : in out Graph_Link_It;
                           Contents : in Link_Contained_Type);

   function Link_Count (Node : in Graph_Link) return Natural;

private

   type Link is record
      Val      : Link_Type;
      Contents : Link_Contained_Type;
   end record;

   type Edge_Array is array (1 .. Size) of Link;

   type Graph_Link is new Graph_Link_Parent with record
      Edges  : Edge_Array;
      Size   : Natural      := 0;
   end record;

   type Graph_Link_It is new Graph_Link_It_Parent with record
      Robj   : Graph_Link_Class;
      Pos    : Positive;
   end record;

end Asgc.Graph.Links.Fixed;
