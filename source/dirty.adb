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


   function Create (Size : Integer) return not null Table_Access
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


   procedure Merge (From :        Rectangle;
                    To   : in out Rectangle)
   is
      use SDL.C;
      X1 : constant int := int'Min (From.X, To.X);
      Y1 : constant int := int'Min (From.Y, To.Y);
      X2 : constant int := int'Max (From.X + From.Width,  To.X + To.Width);
      Y2 : constant int := int'Max (From.Y + From.Height, To.Y + To.Height);
   begin
      To.X      := X1;
      To.Y      := Y1;
      To.Width  := X2 - X1;
      To.Height := Y2 - Y1;
   end Merge;


   procedure Intersect (From :        Rectangle;
                        To   : in out Rectangle)
   is
      use SDL.C;
   begin
      declare
         From_Low  : constant int := From.X;
         From_High : constant int := From.X + From.Width;
         To_Low    : constant int := To.X;
         To_High   : constant int := To.X + To.Width;
         New_Low   : constant int := int'Max (From_Low,  To_Low);
         New_High  : constant int := int'Min (From_High, To_High);
      begin
         To.X     := New_Low;
         To.Width := int'Max (0, New_High - New_Low);
      end;

      declare
         From_Low  : constant int := From.Y;
         From_High : constant int := From.Y + From.Height;
         To_Low    : constant int := To.Y;
         To_High   : constant int := To.Y + To.Height;
         New_Low   : constant int := int'Max (From_Low,  To_Low);
         New_High  : constant int := int'Min (From_High, To_High);
      begin
         To.Y      := New_Low;
         To.Height := int'Max (0, New_High - New_Low);
      end;
   end Intersect;


   procedure Add (Table : in out Table_Type;
                  Rect  :        Rectangle)
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
            A1    : constant Positive  := Positive (Rect.Width * Rect.Height);
            Testr :          Rectangle := Table.Rects (I);
            A2    : constant Positive  := Positive (Testr.Width * Testr.Height);
            Am    : Positive;
            Ratio : Integer;
            Loss  : Integer;
         begin
            Merge (Rect, Testr);
            Am := Positive (Testr.Width * Testr.Height);

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
                           From  :        Table_Type)
   is
   begin
      for I in 1 .. From.Last loop
         Add (Table, From.Rects (I));
      end loop;
   end Merge_Tables;


end Dirty;
