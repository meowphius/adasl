-- The Ada telnet processor - A set of packages for dealing with telnet
--   processing under Ada95
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

-- This package defines a telnet status option processor.  Note that it
-- won't do anything unless you enable it with Set_Local_Will() and
-- Set_Local_Do().
package Asl.Protocol.Telnet.Option.Status is

   type Telnet_Status_Option is new Telnet_Base_Option with private;
   type Telnet_Status_Option_Ptr is access all Telnet_Status_Option;
   type Telnet_Status_Option_Class is access all Telnet_Status_Option'Class;

   procedure New_Remote_Params(Option : in out Telnet_Status_Option;
                               P      : in out Telnet_Processor'Class;
                               Params : in Ada.Streams.Stream_Element_Array);

   procedure Destroy(Option     : in out Telnet_Status_Option;
                     Option_Ptr : in out Telnet_Option_Class);

   -- Request status from the other end.  You won't get it immediately,
   -- you have to wait until the params change.  This will raise a
   -- Constraint_Error if the remote end cannot handle status or has not
   -- yet reported that it can, so make sure to check Remote_Will on
   -- this option to see if you can do this.
   procedure Request_Status(Option : in out Telnet_Status_Option);

private

   type Telnet_Status_Option is new Telnet_Base_Option with null record;

end Asl.Protocol.Telnet.Option.Status;
