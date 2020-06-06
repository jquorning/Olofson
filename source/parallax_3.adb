--
--  "Parallax Scrolling III - Overdraw Elimination"
--
--   Nghia             <nho@optushome.com.au>
--   Randi J. Relander <rjrelander@users.sourceforge.net>
--   David Olofson     <david@gardena.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact authors for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;
with Ada.Text_IO;

with SDL.Video.Windows.Makers;
with SDL.Images.IO;
with SDl.Events.Events;

package body Parallax_3 is

   ------------------------------------------------------------
   --      ...some globals...
   ------------------------------------------------------------

   --
   --  Foreground map.
   --

   Foreground_Map : aliased Map_Data_Type :=
     -- 123456789ABCDEF
     (
      "3333333333333333",
      "3   2   3      3",
      "3   222 3  222 3",
      "3333 22     22 3",
      "3       222    3",
      "3   222 2 2  333",
      "3   2 2 222    3",
      "3   222      223",
      "3        333   3",
      "3  22 23 323  23",
      "3  22 32 333  23",
      "3            333",
      "3 3  22 33     3",
      "3    222  2  3 3",
      "3  3     3   3 3",
      "3333333333333333"
     );

   Single_Map : aliased Map_Data_Type :=
     --  123456789ABCDEF
     (
      "3333333333333333",
      "3000200030000003",
      "3000222030022203",
      "3333022000002203",
      "3000000022200003",
      "3000222020200333",
      "3000202022200003",
      "3000222000000223",
      "3000000003330003",
      "3002202303230023",
      "3002203203330023",
      "3000000000000333",
      "3030022033000003",
      "3000022200200303",
      "3003000003000303",
      "3333333333333333"
     );

   --
   --  Middle level map; where the planets are.
   --
   Middle_Map : aliased Map_Data_Type :=
     (
      --  123456789ABCDEF
      "   1    1       ",
      "           1   1",
      "  1             ",
      "     1  1    1  ",
      "   1            ",
      "         1      ",
      " 1            1 ",
      "    1   1       ",
      "          1     ",
      "   1            ",
      "        1    1  ",
      " 1          1   ",
      "     1          ",
      "        1       ",
      "  1        1    ",
      "                "
     );

   --
   --  Background map.
   --
   Background_Map : aliased Map_Data_Type :=
     (
      --  123456789ABCDEF
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000",
      "0000000000000000"
     );

   ------------------------------------------------------------
   --    ...and some code. :-)
   ------------------------------------------------------------

   type Tile_Kind is (TM_EMPTY, TM_KEYED, TM_OPAQUE); -- 0, 1, 2

   --  Checks if tile is opaqe, empty or color keyed
   function Tile_Mode (Tile : Character) return Tile_Kind is
   begin
      case Tile is
         when '0' =>        return TM_OPAQUE;
         when '1' =>        return TM_KEYED;
         when '2' | '3' =>  return TM_OPAQUE;
         when '4' =>        return TM_KEYED;
         when others =>     return TM_EMPTY;
      end case;
   end Tile_Mode;

   procedure Draw_Tile (Screen : in out Surface;
                        Tiles  :        Surface;
                        X, Y   :        Integer;
                        Tile   :        Character;
                        Pixels :    out Integer)
   is
      Source_Rect : Rectangle;
      Dest_Rect   : Rectangle;
      use SDL.C;
   begin
      --  Study the following expression. Typo trap! :-)
      if Tile = ' ' then
         Pixels := 0;
         return;
      end if;

      Source_Rect.X      := 0;      -- Only one column, so we never change this.
      Source_Rect.Y      := (Character'Pos (Tile) - Character'Pos ('0')) * TILE_H;  -- Select tile from image!
      Source_Rect.Width  := TILE_W;
      Source_Rect.Height := TILE_H;

      dest_rect.x := int (X);
      dest_rect.y := int (Y);

      Screen.Blit (Source      => Tiles,
                   Source_Area => Source_Rect,
                   Self_Area   => Dest_Rect);

      --  Return area rendered for statistics
      Pixels := Integer (Dest_Rect.Width * Dest_Rect.Height);
   end Draw_Tile;

   procedure Main
   is
      use Ada.Text_IO;
      package Natural_IO is new Integer_IO (Natural);

      Window    : SDL.Video.Windows.Window;
      Screen    : Surface;
      Tiles_Bmp : Surface;
      Tiles     : Surface;
      Otiles    : Surface;
      Border    : Rectangle;

      subtype Verbosity is Integer range 0 .. 3;
      Bpp           : Integer   := 0;
      Flags         : Integer   := 0;
      Verbose       : Verbosity := 0;
      Use_Planets   : Boolean   := True;
      Num_Of_Layers : Integer   := 7;
      Bounce_Around : Boolean   := False;
      Wrap          : boolean   := False;
      Alpha         : Integer   := 0;

      Layers : array (0 .. Num_Of_Layers - 1) of aliased Layer_Type;

      Total_Calls      : Natural;
      Total_Blits      : Natural;
      Total_Recursions : Natural;
      Total_Pixels     : Natural;

      Peak_Calls       : Natural := 0;
      peak_blits       : Natural := 0;
      Peak_Recursions  : Natural := 0;
      Peak_Pixels      : Natural := 0;

      Tick1      : Ada.Real_Time.Time;
      Tick2      : Ada.Real_Time.Time;
      Delta_Time : Duration;
      Time       : Long_Float := 0.0;
   begin
      Natural_IO.Default_Width := 8;

      if not SDL.Initialise then -- (SDL.Enabel_Video) then
         raise Program_Error with "Can not initiallise SDL2";
      end if;

--      for(i = 1; i < argc; ++i)
--      {
--              if(strncmp(argv[i], "-v", 2) == 0)
--                      verbose = argv[i][2] - '0';
--              else if(strncmp(argv[i], "-a", 2) == 0)
--                      alpha = atoi(&argv[i][2]);
--              else if(strncmp(argv[i], "-b", 2) == 0)
--                      bounce_around = 1;
--              else if(strncmp(argv[i], "-w", 2) == 0)
--                      wrap = 1;
--              else if(strncmp(argv[i], "-l", 2) == 0)
--                      num_of_layers = atoi(&argv[i][2]);
--              else if(strncmp(argv[i], "-p", 2) == 0)
--                      use_planets = 1;
--              else if(strncmp(argv[i], "-np", 2) == 0)
--                      use_planets = 0;
--              else if(strncmp(argv[i], "-d", 2) == 0)
--                      flags |= SDL_DOUBLEBUF;
--              else if(strncmp(argv[i], "-f", 2) == 0)
--                      flags |= SDL_FULLSCREEN;
--              else
--                      bpp = atoi(&argv[i][1]);
--      }

--      Layers := new Layer_Array'(0 .. Num_Of_Layers - 1 => null);-- Layer_Type; -- Calloc(sizeof(layer_t), num_of_layers);
--      if(!layers)
--      {
--              fprintf(stderr, "Failed to allocate layers!\n");
--              exit(-1);
--      }

      SDL.Video.Windows.Makers.Create (Window,
                                       Title    => "Parallax 3",
                                       Position => (100, 100),
                                       Size     => (SCREEN_W, SCREEN_H)
                                      );-- , bpp, flags);
      Screen := Window.Get_Surface;
--      screen = SDL_SetVideoMode(SCREEN_W, SCREEN_H, bpp, flags);
--      if(!screen)
--      {
--              fprintf(stderr, "Failed to open screen!\n");
--              exit(-1);
--      }

      Border := Screen.Clip_Rectangle;
--      SDL_WM_SetCaption("Parallax Scrolling 3 - Overdraw", "Parallax 3");
--      SDL_ShowCursor(0);

      SDL.Images.IO.Create (Tiles_Bmp,"assets/tiles.bmp");
--      if(!tiles_bmp)
--      {
--              fprintf(stderr, "Could not load graphics!\n");
--              exit(-1);
--      }
--      tiles = SDL_DisplayFormat(tiles_bmp);
--      otiles = SDL_DisplayFormat(tiles_bmp);
--      SDL_FreeSurface(tiles_bmp);
      Tiles  := Tiles_Bmp;
      Otiles := Tiles_Bmp;

--      /* Set colorkey for non-opaque tiles to bright magenta */
--      SDL_SetColorKey(tiles,
--                      SDL_SRCCOLORKEY|SDL_RLEACCEL,
--                      SDL_MapRGB(tiles->format,255,0,255));

--      if(alpha)
--              SDL_SetAlpha(tiles, SDL_SRCALPHA|SDL_RLEACCEL, alpha);

      if Num_Of_Layers > 1 then

         -- Assign maps and tile palettes to parallax layers
         Layer_Init (Layers (0), Foreground_Map'Access, Tiles, Otiles);
         for I in 1 .. Num_Of_Layers - 2 loop
            if (i mod 2 = 1) and Use_Planets then
               Layer_Init (Layers(I), Middle_Map'Access,
                           tiles, otiles);
            else
               Layer_Init (Layers (I), Foreground_Map'Access,
                           tiles, otiles);
            end if;
         end loop;
         Layer_Init (Layers (Num_Of_Layers - 1), Background_Map'Access,
                     tiles, otiles);

         --  Set up the depth order for the
         --  recursive rendering algorithm.

         for i in 0 .. num_of_layers - 2 loop
            Layer_Next (Layers (I), Layers (I + 1)'Unchecked_Access);
         end loop;
      else
         Layer_Init (Layers (0), Single_Map'Access, tiles, otiles);
      end if;

      if Bounce_Around and (Num_Of_Layers > 1) then

         for I in 0 .. num_of_layers - 2 loop
            declare
               use Ada.Numerics.Elementary_Functions;
               A : constant float := 1.0 + Float (I) * 2.0 * 3.1415 / Float (Num_Of_Layers);
               V : constant float := 200.0 / Float (I + 1);
            begin
               Layer_Vel (Layers (I), V * Cos (a), V * Sin (a));
               if not Wrap then
                  Layer_Limit_Bounce (Layers (i));
               end if;
            end;
         end loop;

      else
         --  Set foreground scrolling speed and enable "bounce mode"
         Layer_Vel (Layers (0), FOREGROUND_VEL_X, FOREGROUND_VEL_Y);
         if not Wrap then
            Layer_Limit_Bounce (Layers(0));
         end if;

         -- Link all intermediate levels to the foreground layer
         for i in 1 .. Num_Of_Layers - 2 loop
            Layer_Link (Layers (I), Layers (0)'Unchecked_Access, 1.5 / Float (I + 1));
         end loop;
      end if;

      --  Get initial tick for time calculation
      Tick1 := Ada.Real_Time.Clock;

      loop
         declare
            use SDL.Events;
            Event : SDL.Events.Events.Events;
         begin
            if SDL.Events.Events.Poll (Event) then

               case Event.Common.Event_Type is
                  when Quit => exit;


                     --  Click to exit
--                  when Mouse_Down => return;

--              if (event.type == SDL_MOUSEBUTTONDOWN)
--                      break;

--              if (event.type & (SDL_KEYUP | SDL_KEYDOWN))
--              {
--                      Uint16  *x, *y;
--                      Uint8   *keys = SDL_GetKeyState(&i);
--                      if(keys[SDLK_ESCAPE])
--                              break;

--                      if(keys[SDLK_LSHIFT] || keys[SDLK_RSHIFT])
--                      {
--                              x = &border.w;
--                              y = &border.h;
--                      }
--                      else
--                      {
--                              x = &border.x;
--                              y = &border.y;
--                      }

--                      if(keys[SDLK_UP])
--                              -- *y;
--                      else if(keys[SDLK_DOWN])
--                              ++ *y;
--                      if(keys[SDLK_LEFT])
--                              -- *x;
--                      else if(keys[SDLK_RIGHT])
--                              ++ *x;
--              }
               when others => null;
               end case;
            end if;
         end; -- Event

         --  Calculate time since last update
         declare
            use Ada.Real_Time;
         begin
            Tick2      := Ada.Real_Time.Clock;
            Delta_Time := To_Duration (Tick2 - Tick1);
            Tick1      := tick2;
            Time       := Time + Long_Float (Delta_Time);
         end;

         --  Set background velocity
         declare
            use Ada.Numerics.Elementary_Functions;
         begin
            if Num_Of_Layers > 1 then
               Layer_Vel (Layers (num_of_layers - 1),
                          Sin (Float (Time) * 0.00011) * BACKGROUND_VEL,
                          Cos (Float (Time) * 0.00013) * BACKGROUND_VEL);
            end if;
         end;

         --  Animate all layers
         for I in 0 .. Num_Of_Layers - 1 loop
            Layer_Animate (Layers (I), Float (Delta_Time));
         end loop;

         --  Reset rendering statistics
         for I in 0 .. Num_Of_Layers - 1 loop
            Layer_Reset_Stats (Layers (I));
         end loop;

         --  Render layers (recursive!)
         Layer_Render (Layers (0), screen, border);

         Total_Calls      := 0;
         Total_Blits      := 0;
         Total_Recursions := 0;
         Total_Pixels     := 0;

         if Verbose >= 1 then
            New_Line;
            Put_Line ("layer    calls    blits recursions pixels");
         end if;

         if Verbose = 3 then
            for I in 0 .. Num_Of_Layers - 1 loop
               Natural_IO.Put (I);
               Natural_IO.Put (Layers (I).Calls);
               Natural_IO.Put (Layers (I).Blits);
               Natural_IO.Put (Layers (I).Recursions);
               Natural_IO.Put (Layers (I).Pixels);
               New_Line;
            end loop;
         end if;

         for I in 0 .. Num_Of_Layers - 1 loop
            Total_Calls      := total_calls + Layers (I).calls;
            Total_Blits      := total_blits + Layers (I).blits;
            Total_Recursions := total_recursions + Layers (I).Recursions;
            Total_Pixels     := Total_Pixels + Layers (I).pixels;
         end loop;

         if Total_Calls > Peak_Calls then
            Peak_Calls := Total_Calls;
         end if;

         if total_blits > Peak_Blits then
            peak_blits := total_blits;
         end if;

         if total_recursions > Peak_Recursions then
            peak_recursions := total_recursions;
         end if;

         if total_pixels > Peak_Pixels then
            peak_pixels := total_pixels;
         end if;

         if Verbose >= 2 then
            Put ("TOTAL:  ");
            Natural_IO.Put (Total_Calls);
            Natural_IO.Put (Total_Blits);
            Natural_IO.Put (Total_Recursions);
            Natural_IO.Put (Total_Pixels);
            New_Line;
         end if;

         if Verbose >= 1 then
            Put ("PEAK:   ");
            Natural_IO.Put (Peak_Calls);
            Natural_IO.Put (Peak_Blits);
            Natural_IO.Put (Peak_Recursions);
            Natural_IO.Put (Peak_Pixels);
            New_Line;
         end if;

            --  Draw "title" tile in upper left corner
--              SDL_SetClipRect(screen, NULL);
         declare
            Dummy_Pixels : Integer;
         begin
            Draw_Tile (Screen, Tiles, 2, 2, '4', Dummy_Pixels);
         end;

         --  Make changes visible
         Window.Update_Surface;

         --  Let operating system breath
         delay 0.001;
      end loop;

      Put_Line ("Statistics: (All figures per rendered frame.)");
      Put ("        calls      = "); Natural_IO.Put (Peak_Calls); New_Line;
      Put ("        blits      = "); Natural_IO.Put (Peak_Blits); New_Line;
      Put ("        recursions = "); Natural_IO.Put (Peak_Recursions); New_Line;
      Put ("        pixels     = "); Natural_IO.Put (Peak_Pixels); New_Line;

      --SDL_FreeSurface(tiles);
      --Tiles.Finalize;
      SDL.Finalise;

   end Main;


   -----------------------------------------------------------
   --      layer_t functions
   -----------------------------------------------------------

   ----------
   -- Init --
   ----------

   procedure Layer_Init (Layer        : out Layer_Type;
                         Map          :     Map_Data_Access;
                         Tiles        :     Surface;
                         Opaque_Tiles :     Surface) is
   begin
      Layer := (Next         => null,
                Pos_X        => 0.0,
                Pos_Y        => 0.0,
                Vel_X        => 0.0,
                Vel_Y        => 0.0,
                Map          => Map,
                Tiles        => Tiles,
                Opaque_Tiles => Opaque_Tiles,
                Link         => null,
                Flags        => (others => False),
                Ratio        => 1.0,
                others       => 0);
   end Layer_Init;
   ----------
   -- Next --
   ----------

   procedure Layer_Next (LR      : in out Layer_Type;
                         Next_Lr :        Layer_Access) is
   begin
      lr.next := next_lr;
   end Layer_Next;

   ---------
   -- Pos --
   ---------

   procedure Layer_Pos (LR   : in out Layer_Type;
                        X, Y :        Float) is
   begin
      lr.pos_x := x;
      lr.pos_y := y;
   end Layer_Pos;

   ---------
   -- Vel --
   ---------

   procedure Layer_Vel (LR   : in out Layer_Type;
                        X, Y :        Float) is
   begin
      lr.vel_x := x;
      lr.vel_y := y;
   end Layer_Vel;

   procedure X_Do_Limit_Bounce (LR : in out Layer_Type) is
      Maxx : constant Float := Float (MAP_W * TILE_W - SCREEN_W);
      Maxy : constant Float := Float (MAP_H * TILE_H - SCREEN_H);
   begin
      if lr.pos_x >= Maxx then

         --  v.out = - v.in
         lr.vel_x := -lr.vel_x;

         -- Mirror over right limit. We need to do this
         -- to be totally accurate, as we're in a time
         -- discreet system! Ain't that obvious...? ;-)

         lr.pos_x := maxx * 2.0 - lr.pos_x;

      elsif lr.pos_x <= 0.0 then

         --  Basic physics again...
         lr.vel_x := -lr.vel_x;
         --  Mirror over left limit
         lr.pos_x := -lr.pos_x;
      end if;

      if lr.pos_y >= Maxy then
         lr.vel_y := -lr.vel_y;
         lr.pos_y := maxy * 2.0 - lr.pos_y;
      elsif lr.pos_y <= 0.0 then
         lr.vel_y := -lr.vel_y;
         lr.pos_y := -lr.pos_y;
      end if;
   end X_Do_Limit_Bounce;

   -------------
   -- Animate --
   -------------

   procedure Layer_Animate (LR : in out Layer_Type;
                            DT :        Float) is
   begin
      if lr.Flags.Linked then
         lr.pos_x := lr.link.pos_x * lr.ratio;
         lr.pos_y := lr.link.pos_y * lr.ratio;
      else
         lr.Pos_X := lr.pos_x + dt * lr.vel_x;
         lr.pos_y := lr.Pos_Y + dt * lr.vel_y;
         if lr.Flags.LIMIT_BOUNCE then
            X_Do_Limit_Bounce (lr);
         end if;
      end if;
   end Layer_Animate;


   procedure Layer_Limit_Bounce (LR : in out Layer_Type) is
   begin
      lr.Flags.LIMIT_BOUNCE := True;
   end Layer_Limit_Bounce;

   ----------
   -- Link --
   ----------

   procedure Layer_Link (LR    : in out Layer_Type;
                         To_Lr :        Layer_Access;
                         Ratio :        Float) is
   begin
      lr.Flags.Linked := True; --  |= TL_LINKED;
      lr.link  := to_lr;
      lr.ratio := ratio;
   end Layer_Link;

   ------------
   -- Render --
   ------------

   procedure Layer_Render (LR     : in out Layer_Type;
                           Screen : in out Surface;
                           Rect   :        Rectangle)
   is
      use SDL.Video.Rectangles, SDL.C;
      max_x, Max_Y         : integer;
      map_pos_x, Map_Pos_Y : integer;
      mx, my, Mx_Start     : integer;
      fine_x, Fine_Y       : integer;

      Pos        : Rectangle;
      Local_Clip : Rectangle;
   begin
      lr.calls := lr.calls + 1;

      --  Set up clipping
      --  (Note that we must first clip "rect" to the
      --  current cliprect of the screen - or we'll screw
      --  clipping up as soon as we have more that two
      --  layers!)

      if Rect /= SDL.Video.Rectangles.Null_Rectangle Then

         pos        := screen.clip_rectangle;
         local_clip := Rect; -- *rect;

         --  Convert to (x2,y2)
         pos.Width := pos.width + pos.x;
         pos.height := pos.Height + pos.y;
         local_clip.Width  := local_clip.width + local_clip.x;
         local_clip.height := local_clip.Height + local_clip.y;
         if local_clip.x < pos.X then
            local_clip.x := pos.x;
         end if;
         if local_clip.y < pos.Y then
            local_clip.y := pos.y;
         end if;
         if local_clip.width > pos.Width then
            local_clip.width := pos.width;
         end if;
         if local_clip.height > pos.Height then
            local_clip.height := pos.height;
         end if;

         -- Convert result back to w, h
         local_clip.Width := local_clip.width - local_clip.x;
         local_clip.height := local_clip.Height - local_clip.y;

         -- Check if we actually have an area left!
         if Local_Clip.Width = 0 or Local_Clip.Height = 0 then
            return;
         end if;

         --  Set the final clip rect
         Screen.Set_Clip_Rectangle (local_clip);

      else
         Screen.Set_Clip_Rectangle (Null_Rectangle);
         local_clip := screen.clip_rectangle;
      end if;

      pos.Width  := TILE_W;
      pos.Height := TILE_H;

      --  Position of clip rect in map space
      map_pos_x := Integer (lr.pos_x + Float (screen.clip_rectangle.X));
      map_pos_y := Integer (lr.pos_y + Float (screen.clip_rectangle.Y));

      --  The calculations would break with negative map coords...
      if map_pos_x < 0 then
         Map_Pos_X := Map_Pos_X + MAP_W * TILE_W * (-Map_Pos_X / (MAP_W * TILE_W) + 1);
      end if;

      if map_pos_y < 0 then
         Map_Pos_Y := Map_Pos_Y + MAP_H * TILE_H * (-Map_Pos_Y / (MAP_H * TILE_H) + 1);
      end if;

      --  Position on map in tiles
      Mx := Map_Pos_X / TILE_W;
      My := Map_Pos_Y / TILE_H;

      --  Fine position - pixel offset; up to 1 tile - 1 pixel
      Fine_X := Map_Pos_X mod TILE_W;
      Fine_Y := Map_Pos_Y mod TILE_H;

      --  Draw all visible tiles
      Max_X := Integer (Screen.Clip_Rectangle.X + Screen.Clip_Rectangle.Width);
      Max_Y := Integer (Screen.Clip_Rectangle.Y + Screen.Clip_Rectangle.Height);
      Mx_Start := Mx;

      Pos.Y := Screen.Clip_Rectangle.Y - SDL.Coordinate (Fine_Y);
      while pos.y < SDL.Coordinate (Max_Y) loop
         Mx    := Mx_Start;
         My    := My mod MAP_H;
         Pos.X := screen.clip_rectangle.x - SDL.Coordinate (Fine_X);
         while pos.X < SDL.Coordinate (Max_X) loop
            declare
               Tile : Character;
               Tm   : Tile_Kind; --Integer;
            begin
               mx := Mx mod MAP_W;
               tile := lr.map(My, Mx);
               tm   := tile_mode(tile);

               if (Tm /= TM_OPAQUE) and (lr.Next /= null) then

                  Lr.Recursions := Lr.Recursions + 1;
                  --  Recursive call !!!
                  Layer_Render (Lr.Next.all, screen, pos);
                  Screen.Set_Clip_Rectangle (local_clip);
               end if;

               if tm /= TM_EMPTY then
                  declare
                     Tiles  : Surface;
                     Pixels : Integer;
                  begin
                     if tm = TM_OPAQUE then
                        Tiles := lr.Opaque_Tiles;
                     else
                        Tiles := lr.Tiles;
                     end if;
                     Lr.Blits := Lr.Blits + 1;

                     Draw_Tile (Screen, Tiles,
                                Integer (Pos.X),
                                Integer (Pos.Y),
                                Tile, Pixels);

                     Lr.Pixels := Lr.pixels + Pixels;
                  end;
               end if;
            end;
            Mx    := Mx + 1;
            Pos.X := Pos.X + TILE_W;
         end loop;
         My    := My + 1;
         pos.Y := pos.y + TILE_H;
      end loop;
   end Layer_Render;

   -----------------
   -- Reset_Stats --
   -----------------

   procedure Layer_Reset_Stats (LR : in out Layer_Type) is
   begin
      lr.calls      := 0;
      lr.blits      := 0;
      lr.recursions := 0;
      lr.pixels     := 0;
   end Layer_Reset_Stats;


end Parallax_3;
