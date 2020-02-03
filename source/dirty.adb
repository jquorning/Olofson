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


   function Pig_Dirty_Open (Size : in Integer) return PIG_Dirtytable_Access
   is
      Pdt : PIG_Dirtytable_Access;
   begin
      Pdt := new PIG_Dirtytable'(Rects => null, others => 0);
      Pdt.Size  := Size;
      Pdt.Rects := new Rect_Array'(0 .. Size - 1 => SDL.Video.Rectangles.Null_Rectangle);
      Pdt.Count := 0;
      Pdt.Best  := 0;
      return Pdt;
   end Pig_Dirty_Open;


   procedure Pig_Dirty_Close (Table : in out PIG_Dirtytable_Access) is
   begin
      null;
--          free(pdt->rects);
--          free(pdt);
   end Pig_Dirty_Close;


   procedure Pig_Mergerect (From : in     SDL.Video.Rectangles.Rectangle;
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
   end Pig_Mergerect;


   procedure Pig_Intersectrect (From : in     SDL.Video.Rectangles.Rectangle;
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
   end Pig_Intersectrect;


   procedure Pig_Dirty_Add (Table : in out PIG_Dirtytable;
                            Rect  : in     SDL.Video.Rectangles.Rectangle)
   is
      I, Best_I, Best_Loss : Integer;
   begin
      --  Look for merger candidates.
      --
      --  We start right before the best match we
      --  had the last time around. This can give
      --  us large numbers of direct or quick hits
      --  when dealing with old/new rects for moving
      --  objects and the like.

      Best_I    := -1;
      Best_Loss := 100_000_000;
      if Table.Count /= 0 then
         I := (Table.Best + Table.Count - 1) mod Table.Count;
      end if;

      for J in 0 .. Table.Count - 1 loop
         declare
            use SDL.C;
            A1, A2, Am, Ratio, Loss : Integer;
            Testr : SDL.Video.Rectangles.Rectangle;
         begin
            A1 := Integer (Rect.Width * Rect.Height);

            Testr := Table.Rects (I);
            A2    := Integer (Testr.Width * Testr.Height);

            Pig_Mergerect (Rect, Testr);
            Am := Integer (Testr.Width * Testr.Height);

            --  Perfect or Instant Pick?
            Ratio := 100 * Am / (if A1 > A2 then A1 else A2);
            if Ratio < PIG_INSTANT_MERGE then
               --  Ok, this is good enough! Stop searching.
               Pig_Mergerect (Rect, Table.Rects (I));
               Table.Best := I;
               return;
            end if;

            Loss := Am - A1 - A2;
            if Loss < Best_Loss then
               Best_I     := I;
               Best_Loss  := Loss;
               Table.Best := I;
            end if;

            I := (I + 1) mod Table.Count;
         end;
      end loop;

      --  ...and if the best result is good enough, merge!
      if Best_I >= 0 and Best_Loss < PIG_WORST_MERGE then
         Pig_Mergerect (Rect, Table.Rects (Best_I));
         return;
      end if;

      --  Try to add to table...
      if Table.Count < Table.Size then
         Table.Rects (Table.Count) := Rect;
         Table.Count := Table.Count + 1;
         return;
      end if;

      --  Emergency: Table full! Grab best candidate...
      Pig_Mergerect (Rect, Table.Rects (Best_I));
   end Pig_Dirty_Add;


   procedure Pig_Dirty_Merge (Table : in out PIG_Dirtytable;
                              From  : in     PIG_Dirtytable)
   is
   begin
      for I in 0 .. From.Count - 1 loop
         Pig_Dirty_Add (Table, From.Rects (I));
      end loop;
   end Pig_Dirty_Merge;


end Dirty;
