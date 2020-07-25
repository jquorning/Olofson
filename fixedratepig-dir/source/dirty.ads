--------------------------------------------------------------
--  Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with SDL.Video.Rectangles;

package Dirty
  with Elaborate_Body
is
   package Rectangles renames SDL.Video.Rectangles;

   subtype Rectangle is Rectangles.Rectangle;
   subtype Index_Type is SDL.C.size_t;
   --  A table of dirtyrects for one display page

   Null_Rectangle : constant Rectangle := Rectangles.Null_Rectangle;

   type Rectangle_Arrays_Access
     is not null access Rectangles.Rectangle_Arrays;

   type Dirty_Table is record
      Rects : Rectangle_Arrays_Access;  --  Table of rects
      Last  : Index_Type;               --  # of rects currently used
      Best  : Index_Type;               --  Merge testing starts here!
   end record;

   procedure Create (Table : out Dirty_Table;
                     Size  :     Integer);

   procedure Close (Table : in out Dirty_Table);

   procedure Add (Table : in out Dirty_Table;
                  Rect  :        Rectangle);
   --  Add rectangle Rect to Table.

   procedure Merge_Tables (Table : in out Dirty_Table;
                           From  :        Dirty_Table);
   --  Merge From into Table.

   procedure Merge (From :        Rectangle;
                    To   : in out Rectangle);
   --  Extend To to a new rect that includes both From and To.

   procedure Intersect (From :        Rectangle;
                        To   : in out Rectangle);
   --  Clip To into a rect that is the intersection of From and To.

end Dirty;
