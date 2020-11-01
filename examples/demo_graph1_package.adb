--     The Ada Structured Library - A set of container classes and general
--     tools for use with Ada95.
--     Copyright (C) 1998  Corey Minyard (minyard@acm.org)
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

package body Demo_Graph1_Package is

   function Hash_Node (Node : in Graph_Node) return Natural is
   begin
      return Graph_Node'Pos(Node);
   end Hash_Node;

end Demo_Graph1_Package;
