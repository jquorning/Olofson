--------------------------------------------------------------
--        Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--  Copyright (C) 2020 Jesper Quorning
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with SDL.Video.Rectangles;

package Dirty is
   use SDL.Video.Rectangles;
   use type SDL.C.size_t;

   --  A table of dirtyrects for one display page
   subtype Index_Type     is SDL.C.size_t;
   type Rect_Array_Access is access all Rectangle_Arrays;

   type PIG_Dirtytable is record
      Rects : Rect_Array_Access; --  Table of rects
      Last  : Index_Type;        --  # of rects currently used
      Best  : Index_Type;        --  Merge testing starts here!
   end record;
   type PIG_Dirtytable_Access is access all PIG_Dirtytable;

   function Pig_Dirty_Open (Size : in Integer) return PIG_Dirtytable_Access;
   procedure Pig_Dirty_Close (Table : in out PIG_Dirtytable_Access);

   procedure Pig_Dirty_Add (Table : in out PIG_Dirtytable;
                            Rect  : in     Rectangle);
   --  Add rectangle Rect to Table.

   procedure Pig_Dirty_Merge (Table : in out PIG_Dirtytable;
                              From  : in     PIG_Dirtytable);
   --  Merge table From into table Table.

   procedure Pig_Merge (From : in     Rectangle;
                        To   : in out Rectangle);
   --  Extend 'to' to a new rect that includes both 'From' and 'To'

   procedure Pig_Intersect (From : in     Rectangle;
                            To   : in out Rectangle);
   --  Clip 'to' into a rect that is the intersection of 'from' and 'to'

end Dirty;
