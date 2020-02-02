--------------------------------------------------------------
--        Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with SDL.Video.Rectangles;

package Dirty is

   --  A table of dirtyrects for one display page
   type Rect_Array is
     array (Natural range <>) of SDL.Video.Rectangles.Rectangle;
   type Rect_Array_Access is access all Rect_Array;

   type PIG_Dirtytable is record
      Size  : Integer;           --  Table size
      Rects : Rect_Array_Access; --  Table of rects
      Count : Integer;           --  # of rects currently used
      Best  : Integer;           --  Merge testing starts here!
   end record;


--  PIG_dirtytable *pig_dirty_open(int size);
--  void pig_dirty_close(PIG_dirtytable *pdt);

   procedure Pig_Dirty_Add (Table : in out PIG_Dirtytable;
                            Dr    : in     SDL.Video.Rectangles.Rectangle);
   --  Add rectangle 'dr' to table 'pdt'

--  /* Merge table 'from' into 'pdt' */
--  void pig_dirty_merge(PIG_dirtytable *pdt, PIG_dirtytable *from);

--  /* Extend 'to' to a new rect that includes both 'from' and 'to' */
--  void pig_mergerect(SDL_Rect *from, SDL_Rect *to);

--  /* Clip 'to' into a rect that is the intersection of 'from' and 'to' */
--  void pig_intersectrect(SDL_Rect *from, SDL_Rect *to);

end Dirty;
