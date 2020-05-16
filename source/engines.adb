--------------------------------------------------------------
--    Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--  Copyright (C) 2020 Jesper Quorning
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

   function Check_Tile (Map  : not null PIG_Map_Access;
                        X, Y : Pixels;
                        Mask : Pig_Sides) return Pig_Sides;
   --  Returns a non-zero value if the tile at (x, y) is marked for
   --  collisions on the side indicated by 'mask'.

   procedure Test_Sprite_Map (Engine : in out Game_Engine;
                              Object : in out Game_Object;
                              Sprite :        PIG_Sprite_Access);

   procedure Run_Logic (Engine : in out Game_Engine'Class);

   procedure Tile_Area (Engine : in out Game_Engine;
                        Area   :        SDL_Rectangle);

   procedure Remove_Sprites (Engine : in out Game_Engine);

   procedure Draw_Sprites (Engine : in out Game_Engine);

   procedure Show_Rects (Engine : in out Game_Engine;
                         Table  :        Dirty.Table_Type);

   function Get_Object (Engine : in out Game_Engine)
                       return not null Object_Access;

   procedure Free_Object (Object : in out Game_Object);


   Clean_Object : constant Game_Object :=
     (Owner => null, Id => 0, Ibase => 0, Image => 0,
      Ip    => (Gimage => 0, others => 0.0),
      Tilemask => PIG_None,
      Hitmask  => 0, Hitgroup => 0,
      Timer    => (0, 0, 0),
      Age      => 0, Score => 0, Power => 0, Target => 0,
      State    => Object_States'First,
      Handler  => Null_Handler'Access, others => 0.0);


   ------------------------------------------------------------
   --      Engine
   ------------------------------------------------------------
   overriding
   procedure Initialize (Engine : in out Game_Engine)
   is
      use SDL.Video.Surfaces;
      use SDL.Video.Rectangles;
   begin
      Engine.Self    := null;

      --  Video stuff
      Engine.Screen  := Null_Surface;
      Engine.Buffer  := Null_Surface;    --  For h/w surface displays
      Engine.Surface := Null_Surface;    --  Where to render to
      Engine.Pages   := 1;               --  # of display VRAM buffers
      Engine.View    := Null_Rectangle;  --  Viewport pos & size (pixels)

      --  Dirty
      Engine.Page      := 0;                                --  Current page (double buffer)
      Engine.Pagedirty := (0 => Dirty.Create (Size => 128),
                           1 => null);                      --  One table for each page
      Engine.Workdirty := Dirty.Create (Size => 256);       --  The work dirtytable

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


   overriding
   procedure Finalize (Engine : in out Game_Engine) is
      use Dirty;
   begin
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
      if Engine.Pagedirty (0) /= null then
         Dirty.Close (Engine.Pagedirty (0));
      end if;
      if Engine.Pagedirty (1) /= null then
         Dirty.Close (Engine.Pagedirty (1));
      end if;
      if Engine.Workdirty /= null then
         Dirty.Close (Engine.Workdirty);
      end if;
--      free(pe);
--   end Pig_Close;
   end Finalize;

   procedure Setup (Engine : in out Game_Engine;
                    Self   :        Engine_Access;
                    Screen :        SDL_Surface;
                    Pages  :        Positive)
   is
   begin
      Engine.Self    := Self;
      Engine.Screen  := Screen;
      Engine.Surface := Screen;  -- JQ ???

      if Pages > 1 then
         Engine.Pagedirty (1) := Dirty.Create (Size => 128);
      end if;
   end Setup;

   procedure Set_Viewport (Engine : in out Game_Engine'Class;
                           X, Y   :        Pixels;
                           Width  :        Pixels;
                           Height :        Pixels)
   is
      use SDL.C;
   begin
      Engine.View := (X      => int (X),
                      Y      => int (Y),
                      Width  => int (Width),
                      Height => int (Height));
   end Set_Viewport;


   procedure Create_Sprites (Engine        : in out Game_Engine'Class;
                             Filename      :        String;
                             Width, Height :        Pixels;
                             Handle        :    out Sprite_Index)
   is
      Surface_Load : SDL_Surface;
   begin
      SDL.Images.IO.Create (Surface_Load, Filename);

      --  Disable blending, so we get the alpha channel COPIED!
      Surface_Load.Set_Alpha_Blend (0);               --      SDL_SetAlpha(tmp, 0, 0);
      Surface_Load.Set_Blend_Mode  (SDL.Video.None);  --      SDL_SetAlpha(tmp, 0, 0);
--      Surface_Load.Set_Colour_Key  ((0, 0, 0, Alpha => 0), Enable => True);

      Handle := Engine.Sprite_Last + 1;

      declare
         use SDL.C;
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
                  Source_Area    : SDL_Rectangle;
                  Target_Area    : SDL_Rectangle := (0, 0, 0, 0);
                  Surface_Sprite : SDL_Surface;

                  Sprite : constant not null PIG_Sprite_Access :=
                    new PIG_Sprite'(Width   => Sprite_Width,
                                    Height  => Sprite_Height,
                                    Hot_X   => Sprite_Width  / 2,
                                    Hot_Y   => Sprite_Height / 2,
                                    Radius  => (Sprite_Width + Sprite_Height) / 5,
                                    Surface => SDL.Video.Surfaces.Null_Surface);
               begin
--                      if(pe->nsprites >= PIG_MAX_SPRITES)
--                      {
--                              fprintf(stderr, "Sprite bank full!\n");
--                              return -1;
--                      }
--                      s = (PIG_sprite *)calloc(1, sizeof(PIG_sprite));
                  SDL.Video.Surfaces.Makers.Create (Surface_Sprite,
                                                    Size       => (int (Sprite_Width),
                                                                   int (Sprite_Height)),
                                                    BPP        => 32,
                                                    Red_Mask   => 16#FF000000#,
                                                    Green_Mask => 16#00FF0000#,
                                                    Blue_Mask  => 16#0000FF00#,
                                                    Alpha_Mask => 16#000000FF#);
                  Surface_Sprite.Set_Alpha_Blend (0);
                  Surface_Sprite.Set_Blend_Mode  (SDL.Video.None);
                  Source_Area := (X      => int (Sprite_Width  * (X - 1)),
                                  Y      => int (Sprite_Height * (Y - 1)),
                                  Width  => int (Sprite_Width),
                                  Height => int (Sprite_Height));
                  SDL.Video.Surfaces.Blit (Source      => Surface_Load,
                                           Source_Area => Source_Area,
                                           Self        => Surface_Sprite,
                                           Self_Area   => Target_Area);
                  Surface_Sprite.Set_Alpha_Blend (0); --  (SDL_ALPHA_OPAQUE);
                  Surface_Sprite.Set_Blend_Mode  (SDL.Video.None);
                  --  SDL_SRCALPHA or SDL_RLEACCEL);
                  Sprite.Surface := Surface_Sprite;
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
            when PIG_UNCHANGED =>  null;
            when PIG_MIN       =>  Sprite.Hot_X := 0;
            when PIG_CENTER    =>  Sprite.Hot_X := Sprite.Width / 2;
            when PIG_MAX       =>  Sprite.Hot_X := Sprite.Width;
            when others        =>  Sprite.Hot_X := Hot_X;
         end case;

         case Hot_Y is
            when PIG_UNCHANGED =>  null;
            when PIG_MIN       =>  Sprite.Hot_Y := 0;
            when PIG_CENTER    =>  Sprite.Hot_Y := Sprite.Height / 2;
            when PIG_MAX       =>  Sprite.Hot_Y := Sprite.Height;
            when others        =>  Sprite.Hot_Y := Hot_Y;
         end case;
      end;
   end Set_Hotspot;


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


   procedure Pig_Start (Engine : in out Game_Engine;
                        Frame  :        Integer)
   is
   begin
      Engine.Time  := Long_Float (Frame);
      Engine.Frame := Frame;
      for Object of Engine.Objects loop
         Object.Ip.Gx := Object.X;
         Object.Ip.Ox := Object.X;
         Object.Ip.Gy := Object.Y;
         Object.Ip.Oy := Object.Y;
         Object.Ip.Gimage := Sprite_Counts (Object.Ibase + Object.Image);
      end loop;
   end Pig_Start;


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
                  To_Kind : constant array (Timer_Id) of PIG_Events := (1 => PIG_TIMER_1,
                                                                        2 => PIG_TIMER_2,
                                                                        3 => PIG_TIMER_3);
                  Event : PIG_Event;
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


   procedure Test_Offscreen (Engine : in out Game_Engine;
                             Object : in out Game_Object;
                             Sprite :        PIG_Sprite_Access)
   is
      use SDL.C;
      Hot_X   : constant Pixels := (if Sprite /= null then Sprite.Hot_X  else 0);
      Hot_Y   : constant Pixels := (if Sprite /= null then Sprite.Hot_Y  else 0);
      Width   : constant Pixels := (if Sprite /= null then Sprite.Width  else 0);
      Height  : constant Pixels := (if Sprite /= null then Sprite.Height else 0);

      Sides : constant Pig_Sides :=
          (Top    =>  Pixels (Object.Y) - Hot_Y < -Height,
           Bottom =>  Pixels (Object.Y) - Hot_Y >= Pixels (Engine.View.Height),
           Left   =>  Pixels (Object.X) - Hot_X < -Width,
           Right  =>  Pixels (Object.X) - Hot_X >= Pixels (Engine.View.Width));

      Dx : constant Float := Object.X - Object.Ip.Ox;
      Dy : constant Float := Object.Y - Object.Ip.Oy;
      Event : PIG_Event;
   begin
      if Sides = PIG_None then
         return;
      end if;

      if Sides.Top then
         Event.Cinfo.Y := 0;
         if Dy /= 0.0 then
            Event.Cinfo.X := Pixels (Object.Ip.Ox - Dx * Object.Ip.Oy / Dy);
         end if;

      elsif Sides.Bottom then
         Event.Cinfo.Y := Pixels (Engine.View.Height - 1);
         if Dy /= 0.0 then
            Event.Cinfo.X := Pixels (Object.Ip.Ox + Dx *
                                       (Float (Event.Cinfo.Y) - Object.Ip.Oy) / Dy);
         end if;
      end if;

      if Sides.Left then
         Event.Cinfo.X := 0;
         if Dx /= 0.0 then
            Event.Cinfo.Y := Pixels (Object.Ip.Oy - Dy * Object.Ip.Ox / Dx);
         end if;

      elsif Sides.Right then
         Event.Cinfo.X := Pixels (Engine.View.Width - 1);
         if Dx not in -0.01 .. 0.01 then
            Event.Cinfo.Y := Pixels (Object.Ip.Oy + Dy *
                                       (Float (Event.Cinfo.X) - Object.Ip.Ox) / Dx);
         end if;
      end if;

      Event.Cinfo.Sides := Sides;
      Event.Kind        := PIG_OFFSCREEN;
      Object.Handler (Object, Event);
   end Test_Offscreen;


   procedure Sprite_Sprite_One (Object   : not null Object_Access;
                                Object_2 : not null Object_Access;
                                T        :          Float;
                                Hitdist  :          Float)
   is
      Event : PIG_Event;
      Sides : Pig_Sides;
      IX    : constant Float := Object.Ip.Ox   * (1.0 - T) + Object.X   * T;
      IY    : constant Float := Object.Ip.Oy   * (1.0 - T) + Object.Y   * T;
      IX2   : constant Float := Object_2.Ip.Ox * (1.0 - T) + Object_2.X * T;
      IY2   : constant Float := Object_2.Ip.Oy * (1.0 - T) + Object_2.Y * T;
      Dx    : constant Float := IX - IX2;
      Dy    : constant Float := IY - IY2;
      D_Square : constant Float := Dx * Dx + Dy * Dy;
   begin
      if D_Square >= Hitdist * Hitdist then
         return;         --  Nothing... -->
      end if;

      if abs D_Square < 1.0 then
         Sides := PIG_All;
      else
         declare
            D   : constant Float := Sqrt (D_Square);
            Dx2 : constant Float := Dx / D;
            Dy2 : constant Float := Dy / D;
         begin
            Sides.Left   := Dx2 < -0.707;
            Sides.Right  := Dx2 >  0.707;
            Sides.Top    := Dy2 < -0.707;
            Sides.Bottom := Dy2 >  0.707;
         end;
      end if;
      Event.Kind     := PIG_HIT_OBJECT;
      Event.Cinfo.Ff := 0.0;

      Event.Cinfo.X     := Pixels (IX);
      Event.Cinfo.Y     := Pixels (IY);
      Event.Cinfo.Sides := Sides;

      if True then --      if Object.Hitmask and Object_2.Hitgroup then
         Event.Obj := Object_2;
         Object.Handler (Object.all, Event);
      end if;

      if True then --      if Object_2.Id and (Object_2.Hitmask and Object.Hitgroup) then
         Event.Cinfo.X := Pixels (IX2);
         Event.Cinfo.Y := Pixels (IY2);
         Event.Cinfo.Sides := (Right  => Sides.Left,
                               Left   => Sides.Right,
                               Bottom => Sides.Top,
                               Top    => Sides.Bottom);
         Event.Obj := Object;
         Object_2.Handler (Object_2.all, Event);
      end if;
   end Sprite_Sprite_One;


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
                 Sprite_Counts (Object_2.Ibase + Object_2.Image);

               Hitdist_1 : constant Float :=
                 Float (if Sprite /= null then Sprite.Radius else 0);

               Hitdist_2 : constant Float := Hitdist_1
                   + (if Image in Sprite_Index'First .. Engine.Sprite_Last - 1
                        then Float (Engine.Sprites (Image).Radius)
                        else 0.0);

               Hit_Dist : constant Float := Float'Max (1.0, Hitdist_2);

               --  Calculate number of testing steps
               D_Max_1 : constant Float := Float'Max (abs (Object  .Ip.Ox - Object  .X),
                                                      abs (Object  .Ip.Oy - Object  .Y));
               D_Max_2 : constant Float := Float'Max (abs (Object_2.Ip.Ox - Object_2.X),
                                                      abs (Object_2.Ip.Oy - Object_2.Y));
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


   function Check_Tile (Map  : not null PIG_Map_Access;
                        X, Y : Pixels;
                        Mask : Pig_Sides) return Pig_Sides
   is
      Mx, My : Tiles;
   begin
      --  Must check < 0 first! (Division rounds
      --  towards zero - not downwards.)
      if X < 0 or Y < 0 then
         return PIG_None;
      end if;

      Mx := Tiles (X / Map.Tile_Width);
      My := Tiles (Y / Map.Tile_Height);
      if Mx >= Map.Width or My >= Map.Height then
         return PIG_None;
      end if;

      declare
         Hit : constant Pig_Sides := Map.Hit (Mx, My);
      begin
         return
           (Top    => Hit.Top    and Mask.Top,
            Bottom => Hit.Bottom and Mask.Bottom,
            Left   => Hit.Left   and Mask.Left,
            Right  => Hit.Right  and Mask.Right);
      end;
   end Check_Tile;


   function Pig_Test_Map (Engine : Game_Engine;
                          X, Y   : Pixels) return Pig_Sides
   is
      Mx, My : Tiles;
   begin
      if X < 0 or Y < 0 then
         return PIG_None;
      end if;
      Mx := Tiles (X / Engine.Map.Tile_Width);
      My := Tiles (Y / Engine.Map.Tile_Height);
      if Mx >= Engine.Map.Width or My >= Engine.Map.Height then
         return PIG_None;
      end if;
      return Engine.Map.Hit (Mx, My);
   end Pig_Test_Map;


   Lci : aliased PIG_Cinfo;
   function Pig_Test_Map_Vector (Engine : in out Game_Engine;
                                 X1, Y1 :        Pixels;
                                 X2, Y2 :        Pixels;
                                 Mask   :        Pig_Sides;
                                 Ci     :        PIG_Cinfo_Access)
                                return Pig_Sides
     --  Simple implementation that checks only for top edge collisions.
     --  (Full top/bottom/left/right checks with proper handling of
     --  corners and rows of tiles is a lot more complicated, so I'll
     --  leave that out for now, rather than hacking something simple
     --  but incorrect.)
   is
      Ci2  : constant not null PIG_Cinfo_Access := (if Ci /= null then Ci else Lci'Access);
      Map  : constant not null PIG_Map_Access   := Engine.Map;
      X, Y : Pixels;
      Dist : Pixels := 2_000_000_000;
   begin
      Ci2.Sides := PIG_None;
      if Mask.Top and Y1 < Y2 then

         --  Test for tiles that can be hit from the top
         Y := Y1 + Map.Tile_Height - Y1 mod Map.Tile_Height;
         while Y <= Y2 loop
            X := X1 + (X2 - X1) * (Y - Y1) / (Y2 - Y1);
            if Check_Tile (Map, X, Y + 1, PIG_Top) /= PIG_None then
               Dist := (X - X1) * (X - X1) + (Y - Y1) * (Y - Y1);
               Ci2.X := X;
               Ci2.Y := Y - 1;
               Ci2.Sides.Top := True;
               exit;
            end if;
            Y := Y + Map.Tile_Height;
         end loop;
      end if;

      if Ci2.Sides /= PIG_None then
         Ci2.Ff := Sqrt (Float ((X2 - X1) * (X2 - X1) +
                                 (Y2 - Y1) * (Y2 - Y1) / Dist));
      end if;
      return Ci2.Sides;
   end Pig_Test_Map_Vector;


   procedure Test_Sprite_Map (Engine : in out Game_Engine;
                              Object : in out Game_Object;
                              Sprite :        PIG_Sprite_Access)
   is
      pragma Unreferenced (Sprite);
      Cinfo : aliased PIG_Cinfo;
      Event : PIG_Event;
   begin
      if PIG_None /= Pig_Test_Map_Vector (Engine,
                                          Pixels (Object.Ip.Ox), Pixels (Object.Ip.Oy),
                                          Pixels (Object.X),     Pixels (Object.Y),
                                          Object.Tilemask, Cinfo'Unchecked_Access)
      then
         Event.Cinfo := Cinfo;
         Event.Kind  := PIG_HIT_TILE;
         Object.Handler (Object, Event);
      end if;
   end Test_Sprite_Map;


   procedure Run_Logic (Engine : in out Game_Engine'Class)
   is
      use Object_Lists;
      Object_Cursor, Next_Cursor : Cursor;
      Image  : Sprite_Counts;
   begin
      --  Shift logic coordinates
      for Object of Engine.Objects loop
         Object.Ip.Ox := Object.X;
         Object.Ip.Oy := Object.Y;
      end loop;

      Before_Objects (Engine);

--      for Object of Engine.Objects loop
      Object_Cursor := Engine.Objects.First;
      while Object_Cursor /= No_Element loop
         declare
            Event : PIG_Event;
         begin
            --  We must grab the next pointer before
            --  we call any event handlers, as they
            --  may cause objects to remove themselves!
            Next_Cursor := Next (Object_Cursor);

            Event.Kind := PIG_PREFRAME;
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
            Image := Sprite_Counts (Object.Ibase + Object.Image);
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

               if Object.Id /= 0 and Object.Tilemask /= PIG_None then
                  Test_Sprite_Map (Game_Engine (Engine), Object.all, Sprite);
               end if;
            end if;
         end;
         Next (Object_Cursor);
      end loop;

      for Object of Engine.Objects loop
         if Object.Id /= 0 then
            declare
               Event : PIG_Event;
            begin
               Event.Kind := PIG_POSTFRAME;
               Object.Handler (Object.all, Event);
               Object.Age := Object.Age + 1;
            end;
         end if;
      end loop;

      After_Objects (Engine);

   end Run_Logic;


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


   procedure Pig_Dirty (Engine : in out Game_Engine;
                        Area   :        SDL_Rectangle)
   is
      use type SDL.C.int;
      use SDL.Video.Rectangles;
      R : Rectangle;
   begin
      R.X      := 0;
      R.Y      := 0;
      R.Width  := Engine.Surface.Size.Width;
      R.Height := Engine.Surface.Size.Height;
      if Area /= Null_Rectangle then
         Dirty.Intersect (Area, R);
      end if;

      if R.Width /= 0 and R.Height /= 0 then
         Dirty.Add (Engine.Pagedirty (Engine.Page).all, R);
      end if;

   end Pig_Dirty;


   procedure Tile_Area (Engine : in out Game_Engine;
                        Area   :        SDL_Rectangle)
   is
      use type SDL.C.int;
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
      Engine.Surface.Set_Clip_Rectangle ((X      => Area.X + Engine.View.X,
                                          Y      => Area.Y + Engine.View.Y,
                                          Width  => Area.Width,
                                          Height => Area.Height));

      for Y in Start_Y .. Max_Y loop
         for X in Start_X .. Max_X loop
            declare
               use SDL.C;
               C2   : constant Tiles := Tiles (Engine.Map.Map (X, Y));

               From : SDL_Rectangle :=
                 (X      => int (C2 mod Tilesperrow) * int (Tile_Width),
                  Y      => int (C2 / Tilesperrow)   * int (Tile_Height),
                  Width  => int (Tile_Width),
                  Height => int (Tile_Height));

               To   : SDL_Rectangle :=
                 (X      => int (Engine.View.X) + int (Pixels (X) * Tile_Width),
                  Y      => int (Engine.View.Y) + int (Pixels (Y) * Tile_Height),
                  others => 0);
            begin
               SDL.Video.Surfaces.Blit (Source      => Engine.Map.Tile,
                                        Source_Area => From,
                                        Self        => Engine.Surface,
                                        Self_Area   => To);
            end;
         end loop;
      end loop;
   end Tile_Area;


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
      use SDL.C;
   begin
      for Object of Engine.Objects loop
         if Object.Ip.Gimage in Sprite_Index'First .. Engine.Sprite_Last then
            declare
               Sprite : constant not null PIG_Sprite_Access :=
                 Engine.Sprites (Sprite_Index (Object.Ip.Gimage));

               Area   : SDL_Rectangle :=
                 (X      => int (Object.Ip.Gx) - int (Sprite.Hot_X),
                  Y      => int (Object.Ip.Gy) - int (Sprite.Hot_Y),
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


   procedure Draw_Sprites (Engine : in out Game_Engine)
   is
      Table  : not null Dirty.Table_Access renames Engine.Workdirty;
      Fframe : constant Float := Float (Engine.Time - Long_Float'Floor (Engine.Time));
   begin
      Engine.Surface.Set_Clip_Rectangle (Engine.View);

      --  Swap the work and display/back page dirtytables
      Engine.Workdirty               := Engine.Pagedirty (Engine.Page);
      Engine.Pagedirty (Engine.Page) := Table;

      --  Clear the display/back page dirtytable
      Table.Last := 0;

      --  Update positions and render all objects
      for Object of Engine.Objects loop

         --  Calculate graphic coordinates
         if Engine.Interpolation then
            Object.Ip.Gx := Object.Ip.Ox * (1.0 - Fframe) + Object.X * Fframe;
            Object.Ip.Gy := Object.Ip.Oy * (1.0 - Fframe) + Object.Y * Fframe;
         else
            Object.Ip.Gx := Object.X;
            Object.Ip.Gy := Object.Y;
         end if;
         Object.Ip.Gimage := Sprite_Counts (Object.Ibase + Object.Image);

         --  Render the sprite!
         if Object.Ip.Gimage in Sprite_Index'First .. Engine.Sprite_Last then
            declare
               use SDL.C;

               Sprite      : constant not null PIG_Sprite_Access :=
                 Engine.Sprites (Sprite_Index (Object.Ip.Gimage));

               Source_Area : SDL_Rectangle := (0, 0, 0, 0);

               Target_Area : SDL_Rectangle :=
                 (X => int (Object.Ip.Gx - Float (Sprite.Hot_X) + Float (Engine.View.X)),
                  Y => int (Object.Ip.Gy - Float (Sprite.Hot_Y) + Float (Engine.View.Y)),
                  others => 0);
            begin
               SDL.Video.Surfaces.Blit (Source      => Engine.Sprites (Object.Ip.Gimage).Surface,
                                        Source_Area => Source_Area,
                                        Self        => Engine.Surface,
                                        Self_Area   => Target_Area);
               --
               --  We use the clipped rect for the dirtyrect!
               --
               if Target_Area.Width /= 0 and Target_Area.Height /= 0 then
                  Dirty.Add (Table.all, Target_Area);
               end if;
            end;
         end if;
      end loop;

      --  Merge the display/back page table into the work table
      Dirty.Merge_Tables (Engine.Workdirty.all, Table.all);
   end Draw_Sprites;


   procedure Pig_Refresh (Engine : in out Game_Engine) is
   begin
      Remove_Sprites (Engine);
      Draw_Sprites   (Engine);
   end Pig_Refresh;


   procedure Pig_Refresh_All (Engine : in out Game_Engine) is
      use SDL.Video.Rectangles;
   begin
      Tile_Area (Engine, Engine.View);
      Pig_Dirty (Engine, Null_Rectangle);
      Draw_Sprites (Engine);
   end Pig_Refresh_All;


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


   procedure Show_Rects (Engine : in out Game_Engine;
                         Table  :        Dirty.Table_Type)
   is
      use SDL.Video.Surfaces;
      Color  : Interfaces.Unsigned_32;
   begin
      if Engine.Buffer = Null_Surface then
         declare
            use SDL.Video.Pixel_Formats;
            Format : constant not null Pixel_Format_Access :=
              Engine.Screen.Pixel_Format;
         begin
            SDL.Video.Surfaces.Makers.Create --  RGBSurface
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
         Engine.Surface := Engine.Buffer;
         Tile_Area (Engine, Engine.View);
      end if;
      if Engine.Buffer = Null_Surface then
         return;
      end if;

      Engine.Direct := False;

      for I in 1 .. Table.Last loop
         declare
            use type SDL.C.int;
            R  : SDL_Rectangle;
            R2 : SDL_Rectangle;
         begin
            R        := Table.Rects (I);
            R.X      := R.X - 32;
            R.Y      := R.Y - 32;
            R.Width  := R.Width  + 64;
            R.Height := R.Height + 64;
            R2 := R;
            SDL.Video.Surfaces.Blit (Source      => Engine.Buffer,
                                     Source_Area => R2,
                                     Self        => Engine.Screen,
                                     Self_Area   => R);
         end;
      end loop;

      Color := SDL.Video.Pixel_Formats.To_Pixel (Engine.Screen.Pixel_Format, 255, 0, 255);
      for I in 1 .. Table.Last loop
         declare
            use SDL.C;
            R : SDL_Rectangle;
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


   procedure Pig_Flip (Engine : in out Game_Engine;
                       Window : in out SDL_Window)
   is
      use SDL.Video.Surfaces;
      use SDL.Video.Rectangles;
      Table : Dirty.Table_Type renames Engine.Workdirty.all;
   begin
--      Engine.Surface.Set_Clip_Rectangle (Null_Rectangle);

      if Engine.Show_Dirtyrects then
         Show_Rects (Engine, Table);
         for I in 1 .. Table.Last loop
            declare
               use type SDL.C.int;
               Rect : Rectangle renames Table.Rects (I);
            begin
               Rect.X      := Rect.X - 32;
               Rect.Y      := Rect.Y - 32;
               Rect.Width  := Rect.Width + 64;
               Rect.Height := Rect.Height + 64;
               Dirty.Intersect (Engine.Buffer.Clip_Rectangle, Rect);
            end;
         end loop;

      elsif Engine.Surface = Engine.Buffer then
         for I in 1 .. Table.Last loop
            declare
               Rect_Copy : Rectangle := Table.Rects (I);
            begin
               SDL.Video.Surfaces.Blit (Engine.Screen, Table.Rects (I),
                                        Engine.Buffer, Rect_Copy);
            end;
         end loop;
      end if;

--      if((Engine.screen->flags & SDL_HWSURFACE) == SDL_HWSURFACE) then
      if False then
--         SDL_Flip (Engine.Screen);
         if Engine.Pages > 1 then
            Engine.Page := 1 - Engine.Page;
         end if;
      else
         Window.Update_Surface_Rectangles (Table.Rects.all);
      end if;

      if Engine.Direct then
         Engine.Surface := Engine.Screen;
      elsif Engine.Buffer = Null_Surface then
         Engine.Surface := Engine.Screen;
      else
         Engine.Surface := Engine.Buffer;
      end if;

   end Pig_Flip;


   procedure Pig_Draw_Sprite (Engine : in out Game_Engine;
                              Frame  :        Sprite_Index;
                              X, Y   :        Pixels)
   is
      use SDL.C;
      DR : SDL_Rectangle;
      SA : SDL_Rectangle := (0, 0, 0, 0);
   begin
      --      if(frame >= pe->nsprites)
--              return;
      DR.X := int (X - Engine.Sprites (Frame).Hot_X + Pixels (Engine.View.X));
      DR.Y := int (Y - Engine.Sprites (Frame).Hot_Y + Pixels (Engine.View.Y));
      SDL.Video.Surfaces.Blit (Source      => Engine.Sprites (Frame).Surface,
                               Source_Area => SA,
                               Self        => Engine.Surface,
                               Self_Area   => DR);
   exception
      when SDL.Video.Surfaces.Surface_Error =>
         Ada.Text_IO.Put_Line ("Surface_Error hit");
         raise;
   end Pig_Draw_Sprite;


   ------------------------------------------------------------
   --    Map
   ------------------------------------------------------------

   function Pig_Map_Open (Engine : Game_Engine_Class;
                          Width  : Tiles;
                          Height : Tiles)
                         return not null PIG_Map_Access
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
                 Tile        => SDL.Video.Surfaces.Null_Surface,
                 Hitinfo     => (others => (others => False))
                );

      Engine.Map.Hit.all := (others => (others => PIG_None));
      Engine.Map.Map.all := (others => (others => 0));
      return Engine.Map;
   end Pig_Map_Open;


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


   procedure Pig_Map_Tiles (Map      : in out PIG_Map;
                            Filename :        String;
                            Width    :        Pixels;
                            Height   :        Pixels;
                            Result   :    out Integer)
   is
      pragma Unreferenced (Result);
--  int pig_map_tiles(PIG_map *pm, const char *filename, int tw, int th)
--  {
      Surface : SDL_Surface;
   begin
      Map.Tile_Width  := Width;
      Map.Tile_Height := Height;
      SDL.Images.IO.Create (Surface, Filename);
--      if(!tmp)
--      {
--              fprintf(stderr, "Could not load '%s'!\n", filename);
--              return -1;
--      }
--      pm->tiles = SDL_DisplayFormat(tmp);
      Map.Tile := Surface;
      --      if(!pm->tiles)
--      {
--              fprintf(stderr, "Could not convert '%s'!\n", filename);
--              return -1;
--      }
--      SDL_FreeSurface(tmp);
--      return 0;
   end Pig_Map_Tiles;


   procedure Pig_Map_Collisions (Map   : in out PIG_Map;
                                 First :        Natural;
                                 Count :        Natural;
                                 Sides :        Pig_Sides)
   is
--  void pig_map_collisions(PIG_map *pm, unsigned first, unsigned count, PIG_sides sides)
--  {
--      int i;
   begin
      --      if(first > 255)
--              return;
--      if(first + count > 255)
--              count = 255 - first;
--      for(i = first; i < first + count; ++i)
--              pm->hitinfo[i] = sides;
      null;
   end Pig_Map_Collisions;


   --  Load a map from a string (one byte/tile). 'trans'
   --  is a string used for translating 'data' into integer
   --  tile indices. Each position in 'trans' corresponds
   --  to one tile in the tile palette.
   procedure Pig_Map_From_String (Map   : in out PIG_Map;
                                  Trans :        String;
                                  Data  :        String)
   is
      use Ada.Strings.Fixed;
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
               Position := Index (Trans, "" & C);
--                      f = strchr(trans, c);
--                      if(!f)
               if Position = 0 then
                  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                        "Character '" & C & "' not in" &
                                          " the translation string!");
                  raise Constraint_Error;
               end if;
               --  Map.Map (Z) := Trans (Position);  -- F - Trans;
               Map.Map (X, Y) := Tile_Index (Character'Pos (Trans (Position)));  -- F - Trans;
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


   procedure Free_Object (Object : in out Game_Object) is
   begin
--      Object.Owner.Objects.Delete (Object);
--      po->prev = NULL;
--      po->next = po->owner->object_pool;
--      po->owner->object_pool = po;
      Object.Id := 0;
   end Free_Object;


   function Open_Object (Engine : in out Game_Engine'Class;
                         X, Y   :        Pixels;
                         Last   :        Boolean)
                        return not null Object_Access
   is
      Object : constant not null Object_Access := Get_Object (Engine.Self.all);
   begin
      Object.Owner    := Game_Engine_Class (Engine.Self);
      Object.Tilemask := PIG_All;
      Object.Hitmask  := 0;
      Object.Hitgroup := 0;

      if Last then
         Engine.Objects.Append (Object);
      else
         Engine.Objects.Prepend (Object);
      end if;

      Object.X     := Float (X);
      Object.Y     := Float (Y);
      Object.Ip.Ox := Float (X);
      Object.Ip.Oy := Float (Y);

      return Object;
   end Open_Object;


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


   procedure Unlink_All_Objects (Engine : in out Game_Engine) is
   begin
--        while not Engine.Objects.Is_Empty loop
--           Close_Object (Engine.Objects);
--        end loop;
      null;
   end Unlink_All_Objects;


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
