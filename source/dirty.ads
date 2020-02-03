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

   subtype Rectangle is SDL.Video.Rectangles.Rectangle;

   --  A table of dirtyrects for one display page
   type Rect_Array is
     array (Natural range <>) of Rectangle;
   type Rect_Array_Access is access all Rect_Array;

   type PIG_Dirtytable is record
      Size  : Integer;           --  Table size
      Rects : Rect_Array_Access; --  Table of rects
      Count : Integer;           --  # of rects currently used
      Best  : Integer;           --  Merge testing starts here!
   end record;
   type PIG_Dirtytable_Access is access all PIG_Dirtytable;

   function Pig_Dirty_Open (Size : in Integer) return PIG_Dirtytable_Access;
   procedure Pig_Dirty_Close (Table : in out PIG_Dirtytable_Access);

   procedure Pig_Dirty_Add (Table : in out PIG_Dirtytable;
                            Rect  : in     Rectangle);
   --  Add rectangle 'dr' to table 'pdt'

   procedure Pig_Dirty_Merge (Table : in out PIG_Dirtytable;
                              From  : in     PIG_Dirtytable);
   --  Merge table 'from' into 'pdt'

   procedure Pig_Mergerect (From : in     Rectangle;
                            To   : in out Rectangle);
   --  Extend 'to' to a new rect that includes both 'from' and 'to'

   procedure Pig_Intersectrect (From : in     Rectangle;
                                To   : in out Rectangle);
   --  Clip 'to' into a rect that is the intersection of 'from' and 'to'

end Dirty;
