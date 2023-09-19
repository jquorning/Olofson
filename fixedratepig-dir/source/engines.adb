--------------------------------------------------------------
--  Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions;

with Interfaces;

with SDL.Video.Surfaces.Makers;
with SDL.Video.Pixel_Formats;
with SDL.Images.IO;

package body Engines is

   PIG_MAX_SPRITES : constant := 1024;
   --  Size of sprite frame table

   package Rectangles renames SDL.Video.Rectangles;
   package Surfaces   renames SDL.Video.Surfaces;

   subtype int is SDL.C.int;

   Null_Rectangle : constant Rectangle := Rectangles.Null_Rectangle;
   Null_Surface   : constant Surface   := Surfaces.Null_Surface;

   function "=" (Left, Right : Surface) return Boolean renames Surfaces."=";

   use type int;

   procedure Close_Object (Object : in out Game_Object);
   --  Actually remove an objects. Used internally,
   --  to remove objects that have been marked for
   --  destruction.

   procedure Run_Timers (Engine : in out Game_Engine'Class;
                         Object : in out Game_Object);

   procedure Test_Offscreen (Engine : in out Game_Engine;
                             Object : in out Game_Object;
                             Sprite :        PIG_Sprite_Access);

   function Sqrt (F : Float) return Float
     renames Ada.Numerics.Elementary_Functions.Sqrt;

   procedure Sprite_Sprite_One (Object   : not null Object_Access;
                                Object_2 : not null Object_Access;
                                T        : Float;
                                Hitdist  : Float);
   --  Test for stationary sprite/sprite collision

   procedure Test_Sprite_Sprite (Engine : in out Game_Engine;
                                 Object :        not null Object_Access;
                                 Sprite :        PIG_Sprite_Access);
   --  Check Object against all subsequent objects in the list.
   --  The testing is step size limited so that neither object
   --  moves more than 25% of the collision distance between tests.
   --  (25% should be sufficient for correct direction flags.)

   function Check_Tile (Map  : not null Pig_Map_Access;
                        X, Y : Pixels;
                        Mask : Sides) return Sides;
   --  Returns a non-zero value if the tile at (x, y) is marked for
   --  collisions on the side indicated by 'mask'.

   procedure Test_Sprite_Map (Engine : in out Game_Engine;
                              Object : in out Game_Object;
                              Sprite :        PIG_Sprite_Access);

   procedure Run_Logic (Engine : in out Game_Engine'Class);

   procedure Tile_Area (Engine : in out Game_Engine;
                        Area   :        Rectangle);

   procedure Remove_Sprites (Engine : in out Game_Engine);

   procedure Draw_Sprites (Engine : in out Game_Engine);

   procedure Show_Rects (Engine : in out Game_Engine;
                         Table  :        Dirty_Table);

   function Get_Object (Engine : in out Game_Engine)
                       return not null Object_Access;

   procedure Free_Object (Object : in out Game_Object);


   Clean_Object : constant Game_Object :=
     (Owner     => null, Id => 0, I_Base => 0, Image => 0,
      Interpol  => (Gimage  => 0,
                    Ox | Gx => 0.0,
                    Oy | Gy => 0.0),
      Tile_Mask => No_Side,
      Hit_Mask  => 0, Hit_Group => 0,
      Timer     => (0, 0, 0),
      Age       => 0, Score => 0, Power => 0, Target => 0,
      State     => Object_States'First,
      Handler   => Null_Handler'Access, others => 0.0);

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Engine : in out Game_Engine)
   is
   begin
      Engine.Self := Engine'Unchecked_Access;

      --  Video stuff
      Engine.Screen  := Null_Surface;
      Engine.Buffer  := Null_Surface;    --  For h/w surface displays
      Engine.Surfac  := Null_Surface;    --  Where to render to
      Engine.Pages   := 1;               --  # of display VRAM buffers
      Engine.View    := Null_Rectangle;  --  Viewport pos & size (pixels)

      --  Dirty
      Dirty.Create (Engine.Dirty (Zero), Size => 128);
      Dirty.Create (Engine.Dirty (One),  Size =>   0);
      Dirty.Create (Engine.Dirty (Work), Size => 256);
      --  One table for each page and one work dirtytable

      Engine.Page := Zero;
      --  Current page (double buffer)

      --  "Live" switches
      Engine.Interpolation   := True;
      Engine.Direct          := False;    --  True: Render directly to screen
      Engine.Show_Dirtyrects := False;

      --  Time
      Engine.Time  := 0.0;  --  Logic time (frames)
      Engine.Frame := 0;    --  Logic time; integer part

      --  Background graphics
      Engine.Map   := null;

      --  Objects
      Engine.Objects           := Object_Lists.Empty_List;
      Engine.Object_Id_Counter := 0;

      --  Sprites
      Engine.Sprite_Last       := 0;
      Engine.Sprites           := null;
      Engine.Sprites           := new Sprite_Array'(Sprite_Index'First
                                                      .. PIG_MAX_SPRITES - 1 => null);
      --      Engine.Screen            := Screen;

      if False then
         --      if((pe->screen->flags & SDL_HWSURFACE) == SDL_HWSURFACE)
         --      {
         null;
--              pe->buffer = SDL_CreateRGBSurface(SDL_SWSURFACE,
--                              screen->w, screen->h,
--                              screen->format->BitsPerPixel,
--                              screen->format->Rmask,
--                              screen->format->Gmask,
--                              screen->format->Bmask,
--                              screen->format->Amask);
--              if(!pe->buffer)
--              {
--                      pig_close(pe);
--                      return NULL;
--              }
--              pe->surface = pe->buffer;
--      }
      else
         null;
         --         Engine.Surface := Screen;
      end if;
      --  Engine.Pages := 1;
      --  + ((screen->flags & SDL_DOUBLEBUF) == SDL_DOUBLEBUF);

--      Engine.View.Width    := Engine.Surface.Size.Width;
--      Engine.View.Height   := Engine.Surface.Size.Height;

      --      return Engine;
   end Initialize; -- Clean_Engine;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Engine : in out Game_Engine) is
   begin
      null;
      --  {
      --      if(pe->sprites)
      --      {
      --              int i;
      --              for(i = 0; i < pe->nsprites; ++i)
      --                      if(pe->sprites[i])
      --                      {
      --                              if(pe->sprites[i]->surface)
      --                                      SDL_FreeSurface(pe->sprites[i]->surface);
      --                              free(pe->sprites[i]);
      --                      }
      --              free(pe->sprites);
      --      }
      --      while(pe->objects)
      --              close_object(pe->objects);
      --      if(pe->map)
      --              pig_map_close(pe->map);
      --      if(pe->buffer)
      --              SDL_FreeSurface(pe->buffer);
      --  if Engine.Pagedirty (0) /= null then
      --     Dirty.Close (Engine.Pagedirty (0).all);
      --  end if;
      --  if Engine.Pagedirty (1) /= null then
      --     Dirty.Close (Engine.Pagedirty (1).all);
      --  end if;
      --  if Engine.Workdirty /= null then
      --     Dirty.Close (Engine.Workdirty.all);
      --  end if;
      --      free(pe);
      --   end Pig_Close;
   end Finalize;

   -----------
   -- Setup --
   -----------

   procedure Setup (Engine : in out Game_Engine;
                    Self   :        Engine_Access;
                    Screen :        Surface;
                    Pages  :        Positive)
   is
   begin
      Engine.Screen := Screen;
      Engine.Surfac := Screen;

      if Pages > 1 then
         Dirty.Create (Engine.Dirty (One), Size => 128);
      end if;
   end Setup;

   ------------------
   -- Set_Viewport --
   ------------------

   procedure Set_Viewport (Engine : in out Game_Engine'Class;
                           X, Y   :        Pixels;
                           Width  :        Pixels;
                           Height :        Pixels)
   is
      subtype int is SDL.C.int;
   begin
      Engine.View := (X      => int (X),
                      Y      => int (Y),
                      Width  => int (Width),
                      Height => int (Height));
   end Set_Viewport;

   --------------------
   -- Create_Sprites --
   --------------------

   procedure Create_Sprites (Engine        : in out Game_Engine'Class;
                             Filename      :        String;
                             Width, Height :        Pixels;
                             Handle        :    out Sprite_Index)
   is
      None : constant SDL.Video.Blend_Modes := SDL.Video.None;

      Surface_Load : Surface;
   begin
      SDL.Images.IO.Create (Surface_Load, Filename);

      --  Disable blending, so we get the alpha channel COPIED!
      Surface_Load.Set_Alpha_Blend (0);
      Surface_Load.Set_Blend_Mode  (None);
      Surface_Load.Set_Colour_Key  ((0, 0, 0, Alpha => 0));

      Handle := Engine.Sprite_Last + 1;

      declare
         Surface_Width  : constant Pixels := Pixels (Surface_Load.Size.Width);
         Surface_Height : constant Pixels := Pixels (Surface_Load.Size.Height);
         Sprite_Width   : constant Pixels := (if Width  /= 0 then Width  else Surface_Width);
         Sprite_Height  : constant Pixels := (if Height /= 0 then Height else Surface_Height);
         Last_X         : constant Pixels := Surface_Width  / Sprite_Width;
         Last_Y         : constant Pixels := Surface_Height / Sprite_Height;
      begin
         for Y in 1 .. Last_Y loop
            for X in 1 .. Last_X loop
               declare
                  subtype C_int is SDL.C.int;

                  Source_Area    : Rectangle;
                  Target_Area    : Rectangle := (0, 0, 0, 0);
                  Surface_Sprite : Surface;

                  Sprite : constant not null PIG_Sprite_Access :=
                    new PIG_Sprite'(Width  => Sprite_Width,
                                    Height => Sprite_Height,
                                    Hot_X  => Sprite_Width  / 2,
                                    Hot_Y  => Sprite_Height / 2,
                                    Radius => (Sprite_Width + Sprite_Height) / 5,
                                    Surfac => Null_Surface);
               begin
--                      if(pe->nsprites >= PIG_MAX_SPRITES)
--                      {
--                              fprintf(stderr, "Sprite bank full!\n");
--                              return -1;
--                      }
--                      s = (PIG_sprite *)calloc(1, sizeof(PIG_sprite));
                  Surfaces.Makers.Create
                    (Surface_Sprite,
                     Size       => (C_int (Sprite_Width),
                                    C_int (Sprite_Height)),
                     BPP        => 32,
                     Red_Mask   => 16#FF000000#,
                     Green_Mask => 16#00FF0000#,
                     Blue_Mask  => 16#0000FF00#,
                     Alpha_Mask => 16#000000FF#);
                  Surface_Sprite.Set_Alpha_Blend (0);
                  Surface_Sprite.Set_Blend_Mode  (None);

                  Source_Area := (X      => C_int (Sprite_Width  * (X - 1)),
                                  Y      => C_int (Sprite_Height * (Y - 1)),
                                  Width  => C_int (Sprite_Width),
                                  Height => C_int (Sprite_Height));

                  Surfaces.Blit (Source      => Surface_Load,
                                 Source_Area => Source_Area,
                                 Self        => Surface_Sprite,
                                 Self_Area   => Target_Area);

                  Surface_Sprite.Set_Alpha_Blend (0); --  (SDL_ALPHA_OPAQUE);
                  Surface_Sprite.Set_Blend_Mode  (None);
                  --  SDL_SRCALPHA or SDL_RLEACCEL);
                  Sprite.Surfac := Surface_Sprite;
               --                      s->surface = SDL_DisplayFormatAlpha(tmp2);
--                      if(!s->surface)
--                      {
--                              fprintf(stderr, "Could not convert sprite %d"
--                                              " of '%s'!\n",
--                                              count, filename);
--                              return -1;
--                      end if;
               --  Tmp2.Free;  --                      SDL_FreeSurface(tmp2);
                  Engine.Sprite_Last := Engine.Sprite_Last + 1;
                  Engine.Sprites (Engine.Sprite_Last) := Sprite;
               end;
            end loop;
         end loop;
      end;
   end Create_Sprites;

   -----------------
   -- Set_Hotspot --
   -----------------

   procedure Set_Hotspot (Engine : in out Game_Engine'Class;
                          Frame  :        Sprite_Index;
                          Hot_X  :        Pixels;
                          Hot_Y  :        Pixels)
   is
   begin
      if Frame > Engine.Sprite_Last then
         Ada.Text_IO.Put_Line ("Frame: " & Frame'Image);
         return;  --              return -1;
      end if;

      declare
         Sprite : not null PIG_Sprite_Access renames Engine.Sprites (Frame);
      begin
         case Hot_X is
            when Unchanged =>  null;
            when Minimum   =>  Sprite.Hot_X := 0;
            when Center    =>  Sprite.Hot_X := Sprite.Width / 2;
            when Maximum   =>  Sprite.Hot_X := Sprite.Width;
            when others    =>  Sprite.Hot_X := Hot_X;
         end case;

         case Hot_Y is
            when Unchanged =>  null;
            when Minimum   =>  Sprite.Hot_Y := 0;
            when Center    =>  Sprite.Hot_Y := Sprite.Height / 2;
            when Maximum   =>  Sprite.Hot_Y := Sprite.Height;
            when others    =>  Sprite.Hot_Y := Hot_Y;
         end case;
      end;
   end Set_Hotspot;

   ----------------
   -- Pig_Radius --
   ----------------

   procedure Pig_Radius (Engine : in out Game_Engine;
                         Frame  :        Sprite_Index;
                         Radius :        Pixels)
   is
   begin
--      if((frame < 0 ) || (frame >= pe->nsprites))
--              return -1;
      Engine.Sprites (Frame).Radius := Radius;
--      return 0;
   end Pig_Radius;

   ---------------
   -- Pig_Start --
   ---------------

   procedure Pig_Start (Engine : in out Game_Engine;
                        Frame  :        Integer)
   is
   begin
      Engine.Time  := Long_Float (Frame);
      Engine.Frame := Frame;
      for Object of Engine.Objects loop
         Object.Interpol.Gx := Position_X (Object.X);
         Object.Interpol.Ox := Position_X (Object.X);
         Object.Interpol.Gy := Position_Y (Object.Y);
         Object.Interpol.Oy := Position_Y (Object.Y);
         Object.Interpol.Gimage := Sprite_Counts (Object.I_Base + Object.Image);
      end loop;
   end Pig_Start;

   ----------------
   -- Run_Timers --
   ----------------

   procedure Run_Timers (Engine : in out Game_Engine'Class;
                         Object : in out Game_Object)
   is
      pragma Unreferenced (Engine);
   begin
      for I in Object.Timer'Range loop
         if Object.Timer (I) /= 0 then

            Object.Timer (I) := Object.Timer (I) - 1;
            if Object.Timer (I) = 0 then
               declare
                  To_Kind : constant array (Timer_Id) of Pig_Events :=
                    (1 => Timer_1, 2 => Timer_2, 3 => Timer_3);

                  Event : Pig_Event;
               begin
                  Event.Kind := To_Kind (I);
                  Object.Handler (Object, Event);
                  if Object.Id = 0 then
                     return;
                  end if;
               end;
            end if;
         end if;
      end loop;
   end Run_Timers;

   --------------------
   -- Test_Offscreen --
   --------------------

   procedure Test_Offscreen (Engine : in out Game_Engine;
                             Object : in out Game_Object;
                             Sprite :        PIG_Sprite_Access)
   is
      Hot_X   : constant Pixels := (if Sprite /= null then Sprite.Hot_X  else 0);
      Hot_Y   : constant Pixels := (if Sprite /= null then Sprite.Hot_Y  else 0);
      Width   : constant Pixels := (if Sprite /= null then Sprite.Width  else 0);
      Height  : constant Pixels := (if Sprite /= null then Sprite.Height else 0);

      Hit : constant Sides :=
          (Top    =>  Pixels (Object.Y) - Hot_Y < -Height,
           Bottom =>  Pixels (Object.Y) - Hot_Y >= Pixels (Engine.View.Height),
           Left   =>  Pixels (Object.X) - Hot_X < -Width,
           Right  =>  Pixels (Object.X) - Hot_X >= Pixels (Engine.View.Width));

      Dx : constant Position_X := Position_X (Object.X) - Object.Interpol.Ox;
      Dy : constant Position_Y := Position_Y (Object.Y) - Object.Interpol.Oy;
      Event : Pig_Event;
   begin
      if Hit = No_Side then
         return;
      end if;

      if Hit.Top then
         Event.Collision.Y := 0;
         if Dy /= 0.0 then
            Event.Collision.X := Pixels (Object.Interpol.Ox
                                     - Dx * Position_X (Object.Interpol.Oy / Dy));
         end if;

      elsif Hit.Bottom then
         Event.Collision.Y := Pixels (Engine.View.Height - 1);
         if Dy /= 0.0 then
            Event.Collision.X := Pixels (Object.Interpol.Ox + Dx * Position_X (
                                       (Position_Y (Event.Collision.Y)
                                        - Object.Interpol.Oy) / Dy));
         end if;
      end if;

      if Hit.Left then
         Event.Collision.X := 0;
         if Dx /= 0.0 then
            Event.Collision.Y := Pixels (Object.Interpol.Oy
                                     - Dy * Position_Y (Object.Interpol.Ox / Dx));
         end if;

      elsif Hit.Right then
         Event.Collision.X := Pixels (Engine.View.Width - 1);
         if Dx not in -0.01 .. 0.01 then
            Event.Collision.Y
               := Pixels (Object.Interpol.Oy + Dy *
                         (Position_Y (Event.Collision.X)
                          - Position_Y (Object.Interpol.Ox)) / Position_Y (Dx));
         end if;
      end if;

      Event.Collision.Hit := Hit;
      Event.Kind          := Offscreen;
      Object.Handler (Object, Event);
   end Test_Offscreen;

   -----------------------
   -- Sprite_Sprite_One --
   -----------------------

   procedure Sprite_Sprite_One (Object   : not null Object_Access;
                                Object_2 : not null Object_Access;
                                T        :          Float;
                                Hitdist  :          Float)
   is
      Event : Pig_Event;
      Hit   : Sides;
      IX    : constant Float := Float (Object.Interpol.Ox)   * (1.0 - T) + Object.X   * T;
      IY    : constant Float := Float (Object.Interpol.Oy)   * (1.0 - T) + Object.Y   * T;
      IX2   : constant Float := Float (Object_2.Interpol.Ox) * (1.0 - T) + Object_2.X * T;
      IY2   : constant Float := Float (Object_2.Interpol.Oy) * (1.0 - T) + Object_2.Y * T;
      Dx    : constant Float := IX - IX2;
      Dy    : constant Float := IY - IY2;
      D_Square : constant Float := Dx * Dx + Dy * Dy;
   begin
      if D_Square >= Hitdist * Hitdist then
         return;         --  Nothing... -->
      end if;

      if abs D_Square < 1.0 then
         Hit := All_Sides;
      else
         declare
            D   : constant Float := Sqrt (D_Square);
            Dx2 : constant Float := Dx / D;
            Dy2 : constant Float := Dy / D;
         begin
            Hit.Left   := Dx2 < -0.707;
            Hit.Right  := Dx2 >  0.707;
            Hit.Top    := Dy2 < -0.707;
            Hit.Bottom := Dy2 >  0.707;
         end;
      end if;
      Event.Kind         := Hit_Object;
      Event.Collision.Ff := 0.0;

      Event.Collision.X   := Pixels (IX);
      Event.Collision.Y   := Pixels (IY);
      Event.Collision.Hit := Hit;

      if True then --      if Object.Hitmask and Object_2.Hitgroup then
         Event.Obj := Object_2;
         Object.Handler (Object.all, Event);
      end if;

      if True then --      if Object_2.Id and (Object_2.Hitmask and Object.Hitgroup) then
         Event.Collision.X   := Pixels (IX2);
         Event.Collision.Y   := Pixels (IY2);
         Event.Collision.Hit := (Right  => Hit.Left,
                                 Left   => Hit.Right,
                                 Bottom => Hit.Top,
                                 Top    => Hit.Bottom);
         Event.Obj := Object;
         Object_2.Handler (Object_2.all, Event);
      end if;
   end Sprite_Sprite_One;

   ------------------------
   -- Test_Sprite_Sprite --
   ------------------------

   procedure Test_Sprite_Sprite (Engine : in out   Game_Engine;
                                 Object : not null Object_Access;
                                 Sprite :          PIG_Sprite_Access)
   is
      Object_2 : Object_Access;
      Next_2   : constant Object_Access := null; --  NOT CORRECT !!! ???
   begin
      return;
      pragma Warnings (Off);
--      Object_2 := Object.Next;
      while Object_2 /= null loop
--            Next_2 := Object_2.Next;
         exit when Object.Id = 0 or Object_2.Id = 0;

            --  Check collision groups and masks
         if False
           --               (Object  .Hitmask and Object_2.Hitgroup) or
           --               (Object_2.Hitmask and Object  .Hitgroup)
         then
            declare
               --  Calculate minimum distance
               Image     : constant Sprite_Index :=
                 Sprite_Counts (Object_2.I_Base + Object_2.Image);

               Hitdist_1 : constant Float :=
                 Float (if Sprite /= null then Sprite.Radius else 0);

               Hitdist_2 : constant Float := Hitdist_1
                   + (if Image in Sprite_Index'First .. Engine.Sprite_Last - 1
                        then Float (Engine.Sprites (Image).Radius)
                        else 0.0);

               Hit_Dist : constant Float := Float'Max (1.0, Hitdist_2);

               --  Calculate number of testing steps
               D_Max_1 : constant Float :=
                 Float'Max (abs (Float (Object  .Interpol.Ox) - Object  .X),
                            abs (Float (Object  .Interpol.Oy) - Object  .Y));
               D_Max_2 : constant Float :=
                 Float'Max (abs (Float (Object_2.Interpol.Ox) - Object_2.X),
                            abs (Float (Object_2.Interpol.Oy) - Object_2.Y));
               D_Max   : constant Float := Float'Max (D_Max_1, D_Max_2);
               Delta_T : constant Float := (if D_Max > 1.0
                                              then Hit_Dist / (D_Max * 4.0)
                                              else 1.0);
               T : Float;
            begin
               --  Sweep test!
               T := 0.0;
               while T < 1.0 loop
                  Sprite_Sprite_One (Object, Object_2, T, Hit_Dist);
                  T := T + Delta_T;
               end loop;
            end;
         end if;
         Object_2 := Next_2;
      end loop;
      pragma Warnings (On);
   end Test_Sprite_Sprite;

   ----------------
   -- Check_tile --
   ----------------

   function Check_Tile (Map  : not null Pig_Map_Access;
                        X, Y : Pixels;
                        Mask : Sides) return Sides
   is
      Mx, My : Tiles;
   begin
      --  Must check < 0 first! (Division rounds
      --  towards zero - not downwards.)
      if X < 0 or Y < 0 then
         return No_Side;
      end if;

      Mx := Tiles (X / Map.Tile_Width);
      My := Tiles (Y / Map.Tile_Height);
      if Mx >= Map.Width or My >= Map.Height then
         return No_Side;
      end if;

      declare
         Hit : constant Sides := Map.Hit (Mx, My);
      begin
         return
           (Top    => Hit.Top    and Mask.Top,
            Bottom => Hit.Bottom and Mask.Bottom,
            Left   => Hit.Left   and Mask.Left,
            Right  => Hit.Right  and Mask.Right);
      end;
   end Check_Tile;

   ------------------
   -- Pig_Test_Map --
   ------------------

   function Pig_Test_Map (Engine : Game_Engine;
                          X, Y   : Pixels) return Sides
   is
      Mx, My : Tiles;
   begin
      if X < 0 or Y < 0 then
         return No_Side;
      end if;
      Mx := Tiles (X / Engine.Map.Tile_Width);
      My := Tiles (Y / Engine.Map.Tile_Height);
      if Mx >= Engine.Map.Width or My >= Engine.Map.Height then
         return No_Side;
      end if;
      return Engine.Map.Hit (Mx, My);
   end Pig_Test_Map;

   Lci : aliased Collision_Info;

   -------------------------
   -- Pig_Text_Map_Vector --
   -------------------------

   function Pig_Test_Map_Vector (Engine    : in out Game_Engine;
                                 X1, Y1    :        Pixels;
                                 X2, Y2    :        Pixels;
                                 Mask      :        Sides;
                                 Collision :        Collision_Info_Access)
                                return Sides
     --  Simple implementation that checks only for top edge collisions.
     --  (Full top/bottom/left/right checks with proper handling of
     --  corners and rows of tiles is a lot more complicated, so I'll
     --  leave that out for now, rather than hacking something simple
     --  but incorrect.)
   is
      Collision_2 : constant not null Collision_Info_Access
           := (if Collision /= null then Collision else Lci'Access);

      Map  : constant not null Pig_Map_Access := Engine.Map;

      X, Y : Pixels;
      Dist : Pixels := 2_000_000_000;
   begin
      Collision_2.Hit := No_Side;
      if Mask.Top and Y1 < Y2 then

         --  Test for tiles that can be hit from the top
         Y := Y1 + Map.Tile_Height - Y1 mod Map.Tile_Height;
         while Y <= Y2 loop
            X := X1 + (X2 - X1) * (Y - Y1) / (Y2 - Y1);
            if Check_Tile (Map, X, Y + 1, Top_Side) /= No_Side then
               Dist := (X - X1) * (X - X1) + (Y - Y1) * (Y - Y1);
               Collision_2.X       := X;
               Collision_2.Y       := Y - 1;
               Collision_2.Hit.Top := True;
               exit;
            end if;
            Y := Y + Map.Tile_Height;
         end loop;
      end if;

      if Collision_2.Hit /= No_Side then
         Collision_2.Ff := Sqrt (Float ((X2 - X1) * (X2 - X1) +
                                 (Y2 - Y1) * (Y2 - Y1) / Dist));
      end if;
      return Collision_2.Hit;
   end Pig_Test_Map_Vector;

   ---------------------
   -- Test_Sprite_Map --
   ---------------------

   procedure Test_Sprite_Map (Engine : in out Game_Engine;
                              Object : in out Game_Object;
                              Sprite :        PIG_Sprite_Access)
   is
      pragma Unreferenced (Sprite);
      Collision : aliased Collision_Info;
      Event     : Pig_Event;
   begin
      if No_Side /= Pig_Test_Map_Vector
                     (Engine,
                      Pixels (Object.Interpol.Ox), Pixels (Object.Interpol.Oy),
                      Pixels (Object.X),     Pixels (Object.Y),
                      Object.Tile_Mask, Collision'Unchecked_Access)
      then
         Event.Collision := Collision;
         Event.Kind      := Hit_Tile;
         Object.Handler (Object, Event);
      end if;
   end Test_Sprite_Map;

   ---------------
   -- Run_Logic --
   ---------------

   procedure Run_Logic (Engine : in out Game_Engine'Class)
   is
      use Object_Lists;
      Object_Cursor, Next_Cursor : Cursor;
      Image  : Sprite_Counts;
   begin
      --  Shift logic coordinates
      for Object of Engine.Objects loop
         Object.Interpol.Ox := Position_X (Object.X);
         Object.Interpol.Oy := Position_Y (Object.Y);
      end loop;

      Before_Objects (Engine);

--      for Object of Engine.Objects loop
      Object_Cursor := Engine.Objects.First;
      while Object_Cursor /= No_Element loop
         declare
            Event : Pig_Event;
         begin
            --  We must grab the next pointer before
            --  we call any event handlers, as they
            --  may cause objects to remove themselves!
            Next_Cursor := Next (Object_Cursor);

            Event.Kind := Preframe;
--            Object.Handler (Object.all, Event);
            Element (Object_Cursor).Handler (Element (Object_Cursor).all, Event);
         end;
         Object_Cursor := Next_Cursor;
      end loop;

      Object_Cursor := Engine.Objects.First;
      while Object_Cursor /= No_Element loop
         declare
            Object : constant Object_Access := Element (Object_Cursor);
            Sprite : PIG_Sprite_Access;
         begin
            --  next = po->next;
            Image := Sprite_Counts (Object.I_Base + Object.Image);
            if Image in Sprite_Index'First .. Engine.Sprite_Last then
               Sprite := Engine.Sprites (Image);
            else
               Sprite := null;
            end if;

            --  Move!
            Object.Vx := Object.Vx + Object.Ax;
            Object.Vy := Object.Vy + Object.Ay;
            Object.X  := Object.X  + Object.Vx;
            Object.Y  := Object.Y  + Object.Vy;

            --  Check and handle events
            if Object.Handler /= null then
               Run_Timers (Engine, Object.all);

               if Object.Id /= 0 then
                  Test_Offscreen (Game_Engine (Engine), Object.all, Sprite);
               end if;

               if True then --  Object.Id /= 0 and (Object.Hitmask or Object.Hitgroup) then
                  Test_Sprite_Sprite (Game_Engine (Engine), Object, Sprite);
               end if;

               if Object.Id /= 0 and Object.Tile_Mask /= No_Side then
                  Test_Sprite_Map (Game_Engine (Engine), Object.all, Sprite);
               end if;
            end if;
         end;
         Next (Object_Cursor);
      end loop;

      for Object of Engine.Objects loop
         if Object.Id /= 0 then
            declare
               Event : Pig_Event;
            begin
               Event.Kind := Postframe;
               Object.Handler (Object.all, Event);
               Object.Age := Object.Age + 1;
            end;
         end if;
      end loop;

      After_Objects (Engine);

   end Run_Logic;

   -----------------
   -- Pig_Animate --
   -----------------

   procedure Pig_Animate (Engine : in out Game_Engine'Class;
                          Frames :        Float)
   is
      --  Advance logic time
      I : constant Integer := Integer (Long_Float'Floor (Engine.Time + Long_Float (Frames))
                                         - Long_Float'Floor (Engine.Time));
   begin
      for Count in reverse 0 .. I loop
         Run_Logic (Engine);
         Engine.Frame := Engine.Frame + 1;
      end loop;
      Engine.Time := Engine.Time + Long_Float (Frames);
   end Pig_Animate;

   ---------------
   -- Pig_Dirty --
   ---------------

   procedure Pig_Dirty (Engine : in out Game_Engine;
                        Area   :        Rectangle)
   is
      function "=" (Left, Right : Rectangle) return Boolean
      renames Rectangles."=";

      R : Rectangle;
   begin
      R.X      := 0;
      R.Y      := 0;
      R.Width  := Engine.Surfac.Size.Width;
      R.Height := Engine.Surfac.Size.Height;
      if Area /= Null_Rectangle then
         Dirty.Intersect (Area, R);
      end if;

      if R.Width /= 0 and R.Height /= 0 then
         Dirty.Add (Engine.Dirty (Engine.Page), R);
      end if;

   end Pig_Dirty;

   ---------------
   -- Tile_Area --
   ---------------

   procedure Tile_Area (Engine : in out Game_Engine;
                        Area   :        Rectangle)
   is
      Tile_Width  : Pixels renames Engine.Map.Tile_Width;
      Tile_Height : Pixels renames Engine.Map.Tile_Height;
      Area_Right  : constant Pixels := Pixels (Area.X + Area.Width);
      Area_Top    : constant Pixels := Pixels (Area.Y + Area.Height);
      Start_X     : constant Tiles  := Tiles (Pixels (Area.X) / Tile_Width);
      Start_Y     : constant Tiles  := Tiles (Pixels (Area.Y) / Tile_Height);
      Max_X       : constant Tiles  := Tiles'Min (Tiles ((Area_Right + Tile_Width - 1)
                                                         / Tile_Width),
                                                  Engine.Map.Width  - 1);
      Max_Y       : constant Tiles  := Tiles'Min (Tiles ((Area_Top + Tile_Height - 1)
                                                         / Tile_Height),
                                                  Engine.Map.Height - 1);
      Tilesperrow : constant Tiles  := Tiles (Pixels (Engine.Map.Tile.Size.Width)
                                                / Tile_Width);

   begin
      Engine.Surfac.Set_Clip_Rectangle ((X      => Area.X + Engine.View.X,
                                         Y      => Area.Y + Engine.View.Y,
                                         Width  => Area.Width,
                                         Height => Area.Height));

      for Y in Start_Y .. Max_Y loop
         for X in Start_X .. Max_X loop
            declare
               subtype int is SDL.C.int;
               C2   : constant Tiles := Tiles (Engine.Map.Map (X, Y));

               From : Rectangle :=
                 (X      => int (C2 mod Tilesperrow) * int (Tile_Width),
                  Y      => int (C2 / Tilesperrow)   * int (Tile_Height),
                  Width  => int (Tile_Width),
                  Height => int (Tile_Height));

               To   : Rectangle :=
                 (X      => int (Engine.View.X) + int (Pixels (X) * Tile_Width),
                  Y      => int (Engine.View.Y) + int (Pixels (Y) * Tile_Height),
                  others => 0);
            begin
               Surfaces.Blit (Source      => Engine.Map.Tile,
                              Source_Area => From,
                              Self        => Engine.Surfac,
                              Self_Area   => To);
            end;
         end loop;
      end loop;
   end Tile_Area;

   --------------------
   -- Remove_Sprites --
   --------------------

   procedure Remove_Sprites (Engine : in out Game_Engine)
     --  Remove all objects, using the information that
     --  remains from the last frame. The actual removal
     --  is done by drawing over the sprites with tiles
     --  from the map.
     --
     --  We assume that most objects don't overlap. If
     --  they do that a lot, we could use a "dirty map"
     --  to avoid rendering the same tiles multiple times
     --  in the overlapping areas.
   is
      subtype int is SDL.C.int;
   begin
      for Object of Engine.Objects loop
         if Object.Interpol.Gimage in Sprite_Index'First .. Engine.Sprite_Last then
            declare
               Sprite : constant not null PIG_Sprite_Access :=
                 Engine.Sprites (Sprite_Index (Object.Interpol.Gimage));

               Area   : Rectangle :=
                 (X      => int (Object.Interpol.Gx) - int (Sprite.Hot_X),
                  Y      => int (Object.Interpol.Gy) - int (Sprite.Hot_Y),
                  Width  => int (Sprite.Width),
                  Height => int (Sprite.Height));
            begin
               Dirty.Intersect (Engine.View, Area);
               if Area.Width /= 0 and Area.Height /= 0 then
                  Tile_Area (Engine, Area);
               end if;
            end;

            --  Delete dead objects *after* they've
            --  been removed from the rendering buffer!
            if Object.Id = 0 then
               Close_Object (Object.all);
            end if;
         end if;
      end loop;
   end Remove_Sprites;

   ------------------
   -- Draw_Sprites --
   ------------------

   procedure Draw_Sprites (Engine : in out Game_Engine)
   is
      Old_Dirty : constant Page_Index := Work;
      Fframe    : constant Float := Float (Engine.Time
                                          - Long_Float'Floor (Engine.Time));
   begin
      Engine.Surfac.Set_Clip_Rectangle (Engine.View);

      --  Swap the work and display/back page dirtytables
      Engine.Work := Engine.Page;
      Engine.Page := Old_Dirty;

      --  Clear the display/back page dirtytable
      Engine.Dirty (Engine.Work).Last := 0;

      --  Update positions and render all objects
      for Object of Engine.Objects loop

         --  Calculate graphic coordinates
         if Engine.Interpolation then
            Object.Interpol.Gx
              := Position_X (Float (Object.Interpol.Ox) * (1.0 - Fframe) + Object.X * Fframe);
            Object.Interpol.Gy
              := Position_Y (Float (Object.Interpol.Oy) * (1.0 - Fframe) + Object.Y * Fframe);
         else
            Object.Interpol.Gx := Position_X (Object.X);
            Object.Interpol.Gy := Position_Y (Object.Y);
         end if;
         Object.Interpol.Gimage := Sprite_Counts (Object.I_Base + Object.Image);

         --  Render the sprite!
         if Object.Interpol.Gimage in Sprite_Index'First .. Engine.Sprite_Last then
            declare
               subtype int is SDL.C.int;

               Sprite      : constant not null PIG_Sprite_Access :=
                 Engine.Sprites (Sprite_Index (Object.Interpol.Gimage));

               Source_Area : Rectangle := (0, 0, 0, 0);

               Target_Area : Rectangle :=
                 (X => int (Float (Object.Interpol.Gx) - Float (Sprite.Hot_X)
                              + Float (Engine.View.X)),
                  Y => int (Float (Object.Interpol.Gy) - Float (Sprite.Hot_Y)
                              + Float (Engine.View.Y)),
                  others => 0);
            begin
               Surfaces.Blit
                 (Source      => Engine.Sprites (Object.Interpol.Gimage).Surfac,
                  Source_Area => Source_Area,
                  Self        => Engine.Surfac,
                  Self_Area   => Target_Area);
               --
               --  We use the clipped rect for the dirtyrect!
               --
               if Target_Area.Width /= 0 and Target_Area.Height /= 0 then
                  Dirty.Add (Engine.Dirty (Zero), Target_Area);
               end if;
            end;
         end if;
      end loop;

      --  Merge the display/back page table into the work table
      Dirty.Merge_Tables (Engine.Dirty (Engine.Work), Engine.Dirty (Old_Dirty));
   end Draw_Sprites;

   -----------------
   -- Pig_Refresh --
   -----------------

   procedure Pig_Refresh (Engine : in out Game_Engine) is
   begin
      Remove_Sprites (Engine);
      Draw_Sprites   (Engine);
   end Pig_Refresh;

   ---------------------
   -- Pig_Refresh_All --
   ---------------------

   procedure Pig_Refresh_All (Engine : in out Game_Engine) is
   begin
      Tile_Area (Engine, Engine.View);
      Pig_Dirty (Engine, Null_Rectangle);
      Draw_Sprites (Engine);
   end Pig_Refresh_All;

   -----------------------
   -- Clean_Object_List --
   -----------------------

   procedure Clean_Object_List (Engine : in out Game_Engine) is
      use Object_Lists;
      Cur : Cursor := Engine.Objects.First;
   begin
      while Cur /= No_Element loop
         if Element (Cur).Id = 0 then
            Engine.Objects.Delete (Cur);
         end if;
         Next (Cur);
      end loop;
   end Clean_Object_List;

   ----------------
   -- Show_Rects --
   ----------------

   procedure Show_Rects (Engine : in out Game_Engine;
                         Table  :        Dirty_Table)
   is
      package Pixel_Formats renames SDL.Video.Pixel_Formats;

      subtype Pixel_Depths  is Surfaces.Pixel_Depths;
      subtype Colour_Masks  is Surfaces.Colour_Masks;
      subtype Raw_Pixel     is Interfaces.Unsigned_32;
      subtype Format_Access is Pixel_Formats.Pixel_Format_Access;

      Color : Raw_Pixel;
   begin
      if Engine.Buffer = Null_Surface then
         declare
            Format : constant not null Format_Access :=
              Engine.Screen.Pixel_Format;
         begin
            Surfaces.Makers.Create --  RGBSurface
              (Engine.Buffer,
--            SDL_SWSURFACE,
               Engine.Screen.Size, -- .Width, Pe.Screen.Size.Height,
               Pixel_Depths (Format.Bits), --  BPP, --  Bits_Per_Pixel,
               Red_Mask   => Colour_Masks (Format.Red_Mask),
               Green_Mask => Colour_Masks (Format.Green_Mask),
               Blue_Mask  => Colour_Masks (Format.Blue_Mask),
               Alpha_Mask => Colour_Masks (Format.Alpha_Mask));
         end;
         if Engine.Buffer = Null_Surface then
            return;
         end if;
         Engine.Surfac := Engine.Buffer;
         Tile_Area (Engine, Engine.View);
      end if;
      if Engine.Buffer = Null_Surface then
         return;
      end if;

      Engine.Direct := False;

      for I in 1 .. Table.Last loop
         declare
            R  : Rectangle;
            R2 : Rectangle;
         begin
            R        := Table.Rects (I);
            R.X      := R.X - 32;
            R.Y      := R.Y - 32;
            R.Width  := R.Width  + 64;
            R.Height := R.Height + 64;
            R2 := R;
            Surfaces.Blit (Source      => Engine.Buffer,
                           Source_Area => R2,
                           Self        => Engine.Screen,
                           Self_Area   => R);
         end;
      end loop;

      Color := Pixel_Formats.To_Pixel
        (Engine.Screen.Pixel_Format, 255, 0, 255);

      for I in 1 .. Table.Last loop
         declare
            R : Rectangle;
         begin
            R := Table.Rects (I);
            R.Height := 1;
            Engine.Screen.Fill (R, Color);

            R.Y := R.Y + Table.Rects (I).Height - 1;
            Engine.Screen.Fill (R, Color);

            R := Table.Rects (I);
            R.Width := 1;
            Engine.Screen.Fill (R, Color);

            R.X := R.X + Table.Rects (I).Width - 1;
            Engine.Screen.Fill (R, Color);
         end;
      end loop;
   end Show_Rects;

   --------------
   -- Pig_Flip --
   --------------

   procedure Pig_Flip (Engine : in out Game_Engine;
                       Win    : in out Window)
   is
      Table : Dirty_Table renames Engine.Dirty (Engine.Work);
   begin
--      Engine.Surface.Set_Clip_Rectangle (Null_Rectangle);

      if Engine.Show_Dirtyrects then
         Show_Rects (Engine, Table);
         for I in 1 .. Table.Last loop
            declare
               Rect : Rectangle renames Table.Rects (I);
            begin
               Rect.X      := Rect.X - 32;
               Rect.Y      := Rect.Y - 32;
               Rect.Width  := Rect.Width + 64;
               Rect.Height := Rect.Height + 64;
               Dirty.Intersect (Engine.Buffer.Clip_Rectangle, Rect);
            end;
         end loop;

      elsif Engine.Surfac = Engine.Buffer then
         for I in 1 .. Table.Last loop
            declare
               Rect_Copy : Rectangle := Table.Rects (I);
            begin
               Surfaces.Blit (Engine.Screen, Table.Rects (I),
                              Engine.Buffer, Rect_Copy);
            end;
         end loop;
      end if;

--      if((Engine.screen->flags & SDL_HWSURFACE) == SDL_HWSURFACE) then
      if False then
--         SDL_Flip (Engine.Screen);
         if Engine.Pages > 1 then
            Engine.Page := (if Engine.Page = One then Zero else One);
         end if;
      else
         Win.Update_Surface_Rectangles (Table.Rects.all);
      end if;

      if Engine.Direct then
         Engine.Surfac := Engine.Screen;
      elsif Engine.Buffer = Null_Surface then
         Engine.Surfac := Engine.Screen;
      else
         Engine.Surfac := Engine.Buffer;
      end if;

   end Pig_Flip;

   ----------------------
   -- Pig_Draw_Sprites --
   ----------------------

   procedure Pig_Draw_Sprite (Engine : in out Game_Engine;
                              Frame  :        Sprite_Index;
                              X, Y   :        Pixels)
   is
      subtype int is SDL.C.int;
      DR : Rectangle;
      SA : Rectangle := (0, 0, 0, 0);
   begin
      --      if(frame >= pe->nsprites)
--              return;
      DR.X := int (X - Engine.Sprites (Frame).Hot_X + Pixels (Engine.View.X));
      DR.Y := int (Y - Engine.Sprites (Frame).Hot_Y + Pixels (Engine.View.Y));
      Surfaces.Blit (Source      => Engine.Sprites (Frame).Surfac,
                     Source_Area => SA,
                     Self        => Engine.Surfac,
                     Self_Area   => DR);
   exception
      when Surfaces.Surface_Error =>
         Ada.Text_IO.Put_Line ("Surface_Error hit");
         raise;
   end Pig_Draw_Sprite;

   ------------------------------------------------------------
   --    Map
   ------------------------------------------------------------

   ------------------
   -- Pig_Map_Open --
   ------------------

   function Pig_Map_Open (Engine : not null Engine_Class_Access;
                          Width  : Tiles;
                          Height : Tiles)
                         return not null Pig_Map_Access
   is
   begin
      if Engine.Map /= null then
         Pig_Map_Close (Engine.Map.all);
      end if;

      Engine.Map := new
        PIG_Map'(Owner       => Engine,
                 Width       => Width,
                 Height      => Height,
                 Map         => new Map_Array (0 .. Width - 1,
                                               0 .. Height - 1),
                 Hit         => new Hit_Array (0 .. Width - 1,
                                               0 .. Height - 1),
                 Tile_Width  => 0,
                 Tile_Height => 0,
                 Tile        => Null_Surface,
                 Hitinfo     => (others => (others => False))
                );

      Engine.Map.Hit.all := (others => (others => No_Side));
      Engine.Map.Map.all := (others => (others => 0));
      return Engine.Map;
   end Pig_Map_Open;

   -------------------
   -- Pig_Map_Close --
   -------------------

   procedure Pig_Map_Close (Map : in out PIG_Map) is
--  void pig_map_close(PIG_map *pm)
   begin
      null;
--      PIG_engine *pe = pm->owner;
--      if(pm->tiles)
--              SDL_FreeSurface(pm->tiles);
--      free(pm->hit);
--      free(pm->map);
--      free(pe->map);
--      pe->map = NULL;
   end Pig_Map_Close;

   -------------------
   -- Pig_Map_Tiles --
   -------------------

   procedure Pig_Map_Tiles (Map      : in out PIG_Map;
                            Filename :        String;
                            Width    :        Pixels;
                            Height   :        Pixels;
                            Result   :    out Integer)
   is
      pragma Unreferenced (Result);
--  int pig_map_tiles(PIG_map *pm, const char *filename, int tw, int th)
--  {
      Surfac : Surface;
   begin
      Map.Tile_Width  := Width;
      Map.Tile_Height := Height;
      SDL.Images.IO.Create (Surfac, Filename);
--      if(!tmp)
--      {
--              fprintf(stderr, "Could not load '%s'!\n", filename);
--              return -1;
--      }
--      pm->tiles = SDL_DisplayFormat(tmp);
      Map.Tile := Surfac;
      --      if(!pm->tiles)
--      {
--              fprintf(stderr, "Could not convert '%s'!\n", filename);
--              return -1;
--      }
--      SDL_FreeSurface(tmp);
--      return 0;
   end Pig_Map_Tiles;

   -----------------------
   -- Pig_Map_Collision --
   -----------------------

   procedure Pig_Map_Collisions (Map   : in out PIG_Map;
                                 First :        Tile_Index;
                                 Count :        Natural;
                                 Hit   :        Sides)
   is
      Count_2 : Tile_Index := Tile_Index (Count);
   begin

      if First + Count_2 > 255 then
         Count_2 := 255 - First;
      end if;

      for I in First .. First + Count_2 - 1 loop
         Map.Hitinfo (I) := Hit;
      end loop;

   end Pig_Map_Collisions;

   -------------------------
   -- Pig_Map_From_String --
   -------------------------

   procedure Pig_Map_From_String (Map   : in out PIG_Map;
                                  Trans :        String;
                                  Data  :        String)
   is
      Z : Natural;
   begin
      --  Load the map
      Z := 0;
      for Y in 0 .. Map.Height - 1 loop
         for X in 0 .. Map.Width - 1 loop
            declare
               C        : constant Character := Data (Z + 1);
               Position : Natural;
--               F        : Character;
            begin
               Position := Ada.Strings.Fixed.Index (Trans, "" & C);
--                      f = strchr(trans, c);
--                      if(!f)
               if Position = 0 then
                  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                        "Character '" & C & "' not in" &
                                          " the translation string!");
                  raise Constraint_Error;
               end if;
               Map.Map (X, Y) := Tile_Index (Position - Trans'First);
               Z := Z + 1;
            end;
         end loop;
      end loop;

      --  Generate collision map
      for Y in 0 .. Map.Height - 1 loop
         for X in 0 .. Map.Width - 1 loop
            Map.Hit (X, Y) :=
              Map.Hitinfo (Map.Map (X, Y));
         end loop;
      end loop;

   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Map string too short!");
         raise;
   end Pig_Map_From_String;

   ------------------------------------------------------------
   --      Object
   ------------------------------------------------------------

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Engine : in out Game_Engine) return not null Object_Access
   is
      Object : constant not null Object_Access := new Game_Object'(Clean_Object);
   begin
      --      if(pe->object_pool)
--      {
--              po = pe->object_pool;
--              pe->object_pool = po->next;
--              memset(po, 0, sizeof(PIG_object));
--      }
--      else
--      {

      --              po = (PIG_object *)calloc(1, sizeof(PIG_object));
--              if(!po)
--                      return NULL;
--      }
      Engine.Object_Id_Counter := Engine.Object_Id_Counter + 1;
      Object.Id := Engine.Object_Id_Counter;
      return Object;
   end Get_Object;

   -----------------
   -- Free_Object --
   -----------------

   procedure Free_Object (Object : in out Game_Object) is
   begin
--      Object.Owner.Objects.Delete (Object);
--      po->prev = NULL;
--      po->next = po->owner->object_pool;
--      po->owner->object_pool = po;
      Object.Id := 0;
   end Free_Object;

   -----------------
   -- Open_Object --
   -----------------

   function Open_Object (Engine : in out Game_Engine;
                         X, Y   :        Pixels;
                         Last   :        Boolean)
                        return not null Object_Access
   is
      Base   : constant Engine_Access := Engine_Access (Engine.Self);
      Object : constant not null Object_Access := Get_Object (Base.all);
   begin
      Object.Owner     := Engine.Self;
      Object.Tile_Mask := All_Sides;
      Object.Hit_Mask  := 0;
      Object.Hit_Group := 0;

      if Last then
         Engine.Objects.Append (Object);
      else
         Engine.Objects.Prepend (Object);
      end if;

      Object.X           := Float (X);
      Object.Y           := Float (Y);
      Object.Interpol.Ox := Position_X (X);
      Object.Interpol.Oy := Position_Y (Y);

      return Object;
   end Open_Object;

   ------------------
   -- Close_Object --
   ------------------

   procedure Close_Object (Object : in out Game_Object)
   is
   begin
--      if(po == po->owner->objects)
--              po->owner->objects = po->next;
--      else if(po->prev)
--              po->prev->next = po->next;
--      if(po->next)
--              po->next->prev = po->prev;
      Free_Object (Object);
   end Close_Object;

   -------------------
   -- Unlink_Object --
   -------------------

   procedure Unlink_Object (Object : in out Game_Object) is
   begin
      pragma Warnings (Off);
      if Object.Id = 0 then
         null;
--         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
--                               "Object %p closed more than once!"); -- , po);
      end if;
      pragma Warnings (On);
      Object.Id := 0;     --  Mark for eventual removal and destruction
   end Unlink_Object;

   ------------------------
   -- Unlink_All_Objects --
   ------------------------

   procedure Unlink_All_Objects (Engine : in out Game_Engine) is
   begin
--        while not Engine.Objects.Is_Empty loop
--           Close_Object (Engine.Objects);
--        end loop;
      null;
   end Unlink_All_Objects;

   -----------------
   -- Find_Object --
   -----------------

   function Find_Object (Start : in out Game_Object;
                         Id    :        Object_Id) return Object_Access
   is
--  PIG_object *pig_object_find(PIG_object *start, int id)
--      Pob, Pof : Object_Access;
   begin
      for Object of Start.Owner.Objects loop
         if Object.Id = Id then
            return Object;
         end if;
      end loop;
--        if Start /= null then
--           Pob := Start;
--           Pof := Start;
--        else
--           Pof := Start.Owner.Objects;
--           while Pof /= null loop
--              if Pof.Id = Id then
--                 return Pof;
--              end if;
--              Pof := Pof.Next;
--           end loop;
--           return null;
--        end if;

--        loop
--           if Pob /= null then
--              if Pob.Id = Id then
--                 return pob;
--              end if;
--              Pob := Pob.Prev;
--           end if;

--           if Pof then
--              if Pof.Id = Id then
--                 return pof;
--              end if;
--              Pof := Pof.Next;
--           else
--              if Pob = null then
--                 return null;
--              end if;
--           end if;
--        end loop;
--      Ada.Text_IO.Put_Line ("ERROR");
      return null;
--      raise Program_Error;
   end Find_Object;

end Engines;
