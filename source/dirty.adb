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

package body Dirty is

   PIG_WORST_MERGE   : constant := 300;
   --  Approximate worth of one dirtyrect in pixels.

   PIG_INSTANT_MERGE : constant := 10;
   --  If the merged result gets at most this many percent
   --  bigger than the larger of the two input rects,
   --  accept it as Perfect.


   function Create (Size : in Integer) return not null Table_Access
   is
   begin
      return
        new Table_Type'(Rects => new Rectangle_Arrays'(1 .. Index_Type (Size)
                                                         => Null_Rectangle),
                        Last => 0,
                        Best => 0);
   end Create;


   procedure Close (Table : in out Table_Access) is
   begin
      null;
--          free(pdt->rects);
--          free(pdt);
   end Close;


   procedure Merge (From : in     SDL.Video.Rectangles.Rectangle;
                    To   : in out SDL.Video.Rectangles.Rectangle)
   is
      use SDL.C;
      X1 : int := From.X;
      Y1 : int := From.Y;
      X2 : int := From.X + From.Width;
      Y2 : int := From.Y + From.Height;
   begin
      if To.X < X1 then
         X1 := To.X;
      end if;
      if To.Y < Y1 then
         Y1 := To.Y;
      end if;
      if To.X + To.Width > X2 then
         X2 := To.X + To.Width;
      end if;
      if To.Y + To.Height > Y2 then
         Y2 := To.Y + To.Height;
      end if;
      To.X      := X1;
      To.Y      := Y1;
      To.Width  := X2 - X1;
      To.Height := Y2 - Y1;
   end Merge;


   procedure Intersect (From : in     SDL.Video.Rectangles.Rectangle;
                        To   : in out SDL.Video.Rectangles.Rectangle)
   is
      use SDL.C;
      Amin, Amax, Bmin, Bmax : Integer;
   begin
      Amin := Integer (To.X);
      Amax := Amin + Integer (To.Width);
      Bmin := Integer (From.X);
      Bmax := Bmin + Integer (From.Width);
      if Bmin > Amin then
         Amin := Bmin;
      end if;
      To.X := int (Amin);
      if Bmax < Amax then
         Amax := Bmax;
      end if;
      To.Width := int (if Amax - Amin > 0 then Amax - Amin else 0);

      Amin := Integer (To.Y);
      Amax := Amin + Integer (To.Height);
      Bmin := Integer (From.Y);
      Bmax := Bmin + Integer (From.Height);
      if Bmin > Amin then
         Amin := Bmin;
      end if;
      To.Y := int (Amin);
      if Bmax < Amax then
         Amax := Bmax;
      end if;
      To.Height := int (if Amax - Amin > 0 then Amax - Amin else 0);
   end Intersect;


   procedure Add (Table : in out Table_Type;
                  Rect  : in     SDL.Video.Rectangles.Rectangle)
   is
      I         : Index_Type := 0;
      Best_I    : Index_Type;
      Best_Loss : Integer;
   begin
      --  Look for merger candidates.
      --
      --  We start right before the best match we
      --  had the last time around. This can give
      --  us large numbers of direct or quick hits
      --  when dealing with old/new rects for moving
      --  objects and the like.

      Best_I    := 0;
      Best_Loss := 100_000_000;
      if Table.Last /= 0 then
         I := ((Table.Best + Table.Last - 1) mod Table.Last) + 1;
      end if;

      for J in 1 .. Table.Last loop
         declare
            use SDL.C;
            A1, A2, Am, Ratio, Loss : Integer;
            Testr : SDL.Video.Rectangles.Rectangle;
         begin
            A1 := Integer (Rect.Width * Rect.Height);

            Testr := Table.Rects (I);
            A2    := Integer (Testr.Width * Testr.Height);

            Merge (Rect, Testr);
            Am := Integer (Testr.Width * Testr.Height);

            --  Perfect or Instant Pick?
            Ratio := 100 * Am / (if A1 > A2 then A1 else A2);
            if Ratio < PIG_INSTANT_MERGE then
               --  Ok, this is good enough! Stop searching.
               Merge (Rect, Table.Rects (I));
               Table.Best := I;
               return;
            end if;

            Loss := Am - A1 - A2;
            if Loss < Best_Loss then
               Best_I     := I;
               Best_Loss  := Loss;
               Table.Best := I;
            end if;

            I := (I + 1) mod Table.Last + 1;
         end;
      end loop;

      --  ...and if the best result is good enough, merge!
      if Best_I > 0 and Best_Loss < PIG_WORST_MERGE then
         Merge (Rect, Table.Rects (Best_I));
         return;
      end if;

      --  Try to add to table...
      if Table.Last < Table.Rects'Last then
         Table.Last := Table.Last + 1;
         Table.Rects (Table.Last) := Rect;
         return;
      end if;

      --  Emergency: Table full! Grab best candidate...
      Merge (Rect, Table.Rects (Best_I));
   end Add;


   procedure Merge_Tables (Table : in out Table_Type;
                           From  : in     Table_Type)
   is
   begin
      for I in 1 .. From.Last loop
         Add (Table, From.Rects (I));
      end loop;
   end Merge_Tables;


end Dirty;
