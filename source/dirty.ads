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

package Dirty
  with Elaborate_Body
is
   use SDL.Video.Rectangles;
   use type SDL.C.size_t;

   --  A table of dirtyrects for one display page
   subtype Index_Type     is SDL.C.size_t;
   type Rectangle_Arrays_Access is not null access Rectangle_Arrays;

   type Table_Type is record
      Rects : Rectangle_Arrays_Access;  --  Table of rects
      Last  : Index_Type;               --  # of rects currently used
      Best  : Index_Type;               --  Merge testing starts here!
   end record;
   type Table_Access is access all Table_Type;

   function Create (Size : Integer) return not null Table_Access;
   procedure Close (Table : in out Table_Access);

   procedure Add (Table : in out Table_Type;
                  Rect  :        Rectangle);
   --  Add rectangle Rect to Table.

   procedure Merge_Tables (Table : in out Table_Type;
                           From  :        Table_Type);
   --  Merge From into Table.

   procedure Merge (From :        Rectangle;
                    To   : in out Rectangle);
   --  Extend To to a new rect that includes both From and To.

   procedure Intersect (From :        Rectangle;
                        To   : in out Rectangle);
   --  Clip To into a rect that is the intersection of From and To.

end Dirty;
