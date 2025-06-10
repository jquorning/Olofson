--------------------------------------------------------------
--  Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with SDL.Video.Palettes;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Textures.Makers;

with SDL.Images.IO;

package body Engines is

   package Rectangles renames SDL.Video.Rectangles;
   package Surfaces   renames SDL.Video.Surfaces;

   subtype int is SDL.C.int;
   use type int;

   Null_Rectangle : constant Rectangle := Rectangles.Null_Rectangle;

   procedure Close_Object (Object : in out Game_Object);
   --  Actually remove an objects. Used internally,
   --  to remove objects that have been marked for
   --  destruction.

   procedure Run_Timers (Engine : in out Game_Engine'Class;
                         Object : in out Game_Object);

   procedure Test_Offscreen (Engine : in out Game_Engine;
                             Object : in out Game_Object;
                             Sprite :        Pig_Sprite);

   function Sqrt (F : Float) return Float
     renames Ada.Numerics.Elementary_Functions.Sqrt;

   procedure Sprite_Sprite_One (Engine   : in out Game_Engine'Class;
                                Object   : not null Object_Access;
                                Object_2 : not null Object_Access;
                                T        : Float;
                                Hit_Dist : Float);
   --  Test for stationary sprite/sprite collision

   procedure Test_Sprite_Sprite (Engine : in out Game_Engine;
                                 Object :        not null Object_Access;
                                 Sprite :        Pig_Sprite);
   --  Check Object against all subsequent objects in the list.
   --  The testing is step size limited so that neither object
   --  moves more than 25% of the collision distance between tests.
   --  (25% should be sufficient for correct direction flags.)

   function Check_Tile (Map  : Pig_Map;
                        X, Y : Pixels;
                        Mask : Sides) return Sides;
   --  Returns a non-zero value if the tile at (x, y) is marked for
   --  collisions on the side indicated by 'mask'.

   procedure Test_Sprite_Map (Engine : in out Game_Engine;
                              Object : in out Game_Object;
                              Sprite :        Pig_Sprite);

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
     (Id => 0, I_Base => 0, Image => 0,
      Interpol  => (Gimage  => 0,
                    Ox | Gx => 0.0,
                    Oy | Gy => 0.0),
      Tile_Mask => No_Side,
      Hit_Mask  => 0, Hit_Group => 0,
      Timer     => (0, 0, 0),
      Age       => 0, Score => 0, Power => 0, Target => 0,
      State     => Object_States'First,
      Handler   => Null_Handler'Access,
      X  | Y    => 0.0,
      Vx | Vy   => 0.0,
      Ax | Ay   => 0.0);

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Engine : in out Game_Engine)
   is
   begin
      --  Video stuff
--      Engine.Buffer  := SDL.Video.Textures.Null_Texture;
--      For h/w surface displays
--      Engine.Surfac  := Null_Surface;    --  Where to render to
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
--      Engine.Map   := null;

      --  Objects
      Engine.Objects           := Object_Lists.Empty_Vector;
      Engine.Object_Id_Counter := 0;

      --  Sprites
      Engine.Sprite_Last       := 0;

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

   end Initialize;

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

   ----------------
   -- Setup_Game --
   ----------------

   procedure Setup_Game (Engine : in out Game_Engine;
                         Win    : in out Window;
                         Pages  :        Positive)
   is
      use SDL.Video;
      use type Renderers.Renderer_Flags;

      Size : SDL.Sizes;
   begin
      Renderers.Makers.Create (Rend   => Engine.Renderer,
                               Window => Win,
                               Flags  => Renderers.Accelerated or
                                         Renderers.Target_Texture);

      Renderers.Set_Draw_Colour  (Engine.Renderer, (Red   => 0,
                                                    Green => 0,
                                                    Blue  => 0,
                                                    Alpha => 0));
      Size := Windows.Get_Size (Win);

      Renderers.Fill
         (Self      => Engine.Renderer,
          Rectangle => (0, 0,
                        Width  => Size.Width,
                        Height => Size.Height));

      if Pages > 1 then
         Dirty.Create (Engine.Dirty (One), Size => 128);
      end if;

      Textures.Makers.Create (Tex      => Engine.Buffer,
                              Renderer => Engine.Renderer,
                              Surface  => Win.Get_Surface);
   end Setup_Game;

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
                             Sprite_Last   :    out Sprite_Index)
   is
      use SDL.Video;

      procedure Build_Sprite (Sprite        : in out Pig_Sprite;
                              X, Y          :        Pixels;
                              Width, Height :        Pixels;
                              Surface       :        Surfaces.Surface);

      procedure Build_Sprite (Sprite        : in out Pig_Sprite;
                              X, Y          :        Pixels;
                              Width, Height :        Pixels;
                              Surface       :        Surfaces.Surface)
      is
         subtype C_int is SDL.C.int;

         Source_Area    : Rectangle;
         Target_Area    : Rectangle := (0, 0, 0, 0);
         Surface_Sprite : Surfaces.Surface;
      begin
         Sprite.Width  := Width;
         Sprite.Height := Height;
         Sprite.Hot_X  := Width  / 2;
         Sprite.Hot_Y  := Height / 2;
         Sprite.Radius := (Width + Height) / 5;

         Surfaces.Makers.Create
           (Surface_Sprite,
            Size       => (C_int (Width),
                           C_int (Height)),
            BPP        => 32,
            Red_Mask   => 16#FF000000#,
            Green_Mask => 16#00FF0000#,
            Blue_Mask  => 16#0000FF00#,
            Alpha_Mask => 16#000000FF#);

         Surfaces.Set_Alpha_Blend (Surface_Sprite, 0);
         Surfaces.Set_Blend_Mode  (Surface_Sprite, None);

         Source_Area := (X      => C_int (Width  * (X - 1)),
                         Y      => C_int (Height * (Y - 1)),
                         Width  => C_int (Width),
                         Height => C_int (Height));

         Surfaces.Blit (Source      => Surface,
                        Source_Area => Source_Area,
                        Self        => Surface_Sprite,
                        Self_Area   => Target_Area);

         Surfaces.Set_Alpha_Blend (Surface_Sprite, 0);
         Surfaces.Set_Blend_Mode  (Surface_Sprite, None);

         Textures.Makers.Create
           (Tex      => Sprite.Textur,
            Renderer => Engine.Renderer,
            Surface  => Surface_Sprite);
      end Build_Sprite;

      Surface_Load : Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Surface_Load, Filename);

      --  Disable blending, so we get the alpha channel COPIED!
      Surface_Load.Set_Alpha_Blend (0);
      Surface_Load.Set_Blend_Mode  (None);
      Surface_Load.Set_Colour_Key  ((0, 0, 0, Alpha => 0));

      Sprite_Last := Engine.Sprite_Last + 1;

      declare
         Surface_Size   : constant SDL.Sizes := Surface_Load.Size;

         Surface_Width  : constant Pixels := Pixels (Surface_Size.Width);
         Surface_Height : constant Pixels := Pixels (Surface_Size.Height);
         Sprite_Width   : constant Pixels := (if Width  /= 0 then Width
                                                             else Surface_Width);
         Sprite_Height  : constant Pixels := (if Height /= 0 then Height
                                                             else Surface_Height);
         Last_X         : constant Pixels := Surface_Width  / Sprite_Width;
         Last_Y         : constant Pixels := Surface_Height / Sprite_Height;
      begin
         for Y in 1 .. Last_Y loop
            for X in 1 .. Last_X loop
               Engine.Sprite_Last := Engine.Sprite_Last + 1;

               Build_Sprite (Sprite  => Engine.Sprites (Engine.Sprite_Last),
                             X       => X,
                             Y       => Y,
                             Width   => Sprite_Width,
                             Height  => Sprite_Height,
                             Surface => Surface_Load);
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
         return;
      end if;

      declare
         Sprite : Pig_Sprite renames Engine.Sprites (Frame);
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
      Engine.Sprites (Frame).Radius := Radius;
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
                  Object.Handler (Engine, Object, Event);
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
                             Sprite :        Pig_Sprite)
   is
      --  What did Sprite = null mean..?  jq ???
      Hot_X   : constant Pixels := Sprite.Hot_X;
      Hot_Y   : constant Pixels := Sprite.Hot_Y;
      Width   : constant Pixels := Sprite.Width;
      Height  : constant Pixels := Sprite.Height;
      --  Hot_X   : constant Pixels := (if Sprite /= null then Sprite.Hot_X  else 0);
      --  Hot_Y   : constant Pixels := (if Sprite /= null then Sprite.Hot_Y  else 0);
      --  Width   : constant Pixels := (if Sprite /= null then Sprite.Width  else 0);
      --  Height  : constant Pixels := (if Sprite /= null then Sprite.Height else 0);

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
      Object.Handler (Engine, Object, Event);
   end Test_Offscreen;

   -----------------------
   -- Sprite_Sprite_One --
   -----------------------

   procedure Sprite_Sprite_One (Engine   : in out Game_Engine'Class;
                                Object   : not null Object_Access;
                                Object_2 : not null Object_Access;
                                T        :          Float;
                                Hit_Dist :          Float)
   is
      function Interpolate
        (Pos_1 : Position;
         Pos_2 : Position;
         T     : Float)     -- range 0.0 .. 1,0
         return Position;

      function Interpolate
        (Pos_1 : Position;
         Pos_2 : Position;
         T     : Float)     -- range 0.0 .. 1,0
         return Position
      is
      begin
         return Position (Float (Pos_1) * (1.0 - T) + Float (Pos_2) * T);
      end Interpolate;

      Event : Pig_Event;
      Hit   : Sides;

      IX    : constant Position := Interpolate (Position (Object.Interpol.Ox),
                                                Object.X,   T);
      IY    : constant Position := Interpolate (Position (Object.Interpol.Oy),
                                                Object.Y,   T);
      IX2   : constant Position := Interpolate (Position (Object_2.Interpol.Ox),
                                                Object_2.X, T);
      IY2   : constant Position := Interpolate (Position (Object_2.Interpol.Oy),
                                                Object_2.Y, T);

      Dx    : constant Position := IX - IX2;
      Dy    : constant Position := IY - IY2;

      D_Square : constant Float := Float (Dx * Dx + Dy * Dy);
   begin
      if D_Square >= Hit_Dist * Hit_Dist then
         return;         --  Nothing... -->
      end if;

      if abs D_Square < 1.0 then
         Hit := All_Sides;
      else
         declare
            D   : constant Position := Position (Sqrt (D_Square));
            Dx2 : constant Float := Float (Dx / D);
            Dy2 : constant Float := Float (Dy / D);
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

      if True then
--      if Object.Hitmask and Object_2.Hitgroup then
         Event.Obj := Object_2;
         Object.Handler (Engine, Object.all, Event);
      end if;

      if True then
--      if Object_2.Id and (Object_2.Hitmask and Object.Hitgroup) then
         Event.Collision.X   := Pixels (IX2);
         Event.Collision.Y   := Pixels (IY2);
         Event.Collision.Hit := (Right  => Hit.Left,
                                 Left   => Hit.Right,
                                 Bottom => Hit.Top,
                                 Top    => Hit.Bottom);
         Event.Obj := Object;
         Object_2.Handler (Engine, Object_2.all, Event);
      end if;
   end Sprite_Sprite_One;

   ------------------------
   -- Test_Sprite_Sprite --
   ------------------------

   procedure Test_Sprite_Sprite (Engine : in out   Game_Engine;
                                 Object : not null Object_Access;
                                 Sprite :          Pig_Sprite)
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

               Hitdist_1 : constant Float := Float (Sprite.Radius);
--                 Float (if Sprite /= null then Sprite.Radius else 0);

               Hitdist_2 : constant Float := Hitdist_1
                   + (if Image in Sprite_Index'First .. Engine.Sprite_Last - 1
                        then Float (Engine.Sprites (Image).Radius)
                        else 0.0);

               Hit_Dist : constant Float := Float'Max (1.0, Hitdist_2);

               --  Calculate number of testing steps
               D_Max_1 : constant Position :=
                 Position'Max (abs (Position (Object  .Interpol.Ox) - Object  .X),
                               abs (Position (Object  .Interpol.Oy) - Object  .Y));
               D_Max_2 : constant Position :=
                 Position'Max (abs (Position (Object_2.Interpol.Ox) - Object_2.X),
                               abs (Position (Object_2.Interpol.Oy) - Object_2.Y));
               D_Max   : constant Position := Position'Max (D_Max_1, D_Max_2);
               Delta_T : constant Float := (if D_Max > 1.0
                                            then Hit_Dist / Float (D_Max * 4.0)
                                            else 1.0);
               T : Float;
            begin
               --  Sweep test!
               T := 0.0;
               while T < 1.0 loop
                  Sprite_Sprite_One (Engine, Object, Object_2, T, Hit_Dist);
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

   function Check_Tile (Map  : Pig_Map;
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

      Map  : Pig_Map renames Engine.Map;

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
                              Sprite :        Pig_Sprite)
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
         Object.Handler (Engine, Object, Event);
      end if;
   end Test_Sprite_Map;

   ---------------
   -- Run_Logic --
   ---------------

   procedure Run_Logic (Engine : in out Game_Engine'Class)
   is
      use Object_Lists;
      Object_Cursor, Next_Cursor : Cursor;
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

            Element (Object_Cursor).Handler (Engine,
                                             Element (Object_Cursor).all,
                                             Event);
         end;
         Object_Cursor := Next_Cursor;
      end loop;

      Object_Cursor := Engine.Objects.First;
      while Object_Cursor /= No_Element loop
         declare
            Object : constant Object_Access := Element (Object_Cursor);
            Image  : constant Sprite_Counts := Sprite_Counts (Object.I_Base
                                                            + Object.Image);
            Sprite : Pig_Sprite renames Engine.Sprites (Image);
         begin
            --  Move!
            Object.Vx := Object.Vx + Speed (Object.Ax);
            Object.Vy := Object.Vy + Speed (Object.Ay);
            Object.X  := Object.X  + Position (Object.Vx);
            Object.Y  := Object.Y  + Position (Object.Vy);

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
               Object.Handler (Engine, Object.all, Event);
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
      Long_Frames : constant Long_Float := Long_Float (Frames);
      Long_Time   : constant Long_Float := Long_Float'Floor (Engine.Time);
      I : constant Integer := Integer (Long_Float'Floor (Engine.Time + Long_Frames)
                                         - Long_Time);
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
      use SDL.Video;
      use type Rectangles.Rectangle;

      Size : SDL.Sizes;
      R    : Rectangle;
   begin
      Renderers.Get_Logical_Size (Engine.Renderer, Size);
      --  This ^ call will not work

      R.X      := 0;
      R.Y      := 0;
      R.Width  := 800; --  Size.Width;  ???
      R.Height := 600; --  Size.Height; ???
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
      use SDL.Video;

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
      Tiles_Per_Row : constant Tiles  := Tiles (Pixels (Engine.Map.Tile.Get_Size.Width)
                                                / Tile_Width);

   begin
      Renderers.Set_Clip (Engine.Renderer,
                          (X      => Area.X + Engine.View.X,
                           Y      => Area.Y + Engine.View.Y,
                           Width  => Area.Width,
                           Height => Area.Height));

      for Y in Start_Y .. Max_Y loop
         for X in Start_X .. Max_X loop
            declare
               subtype int is SDL.C.int;
               C2   : constant Tiles := Tiles (Engine.Map.Map (X, Y));

               From : constant Rectangle :=
                 (X      => int (C2 mod Tiles_Per_Row) * int (Tile_Width),
                  Y      => int (C2 / Tiles_Per_Row)   * int (Tile_Height),
                  Width  => int (Tile_Width),
                  Height => int (Tile_Height));

               To   : constant Rectangle :=
                 (X      => int (Engine.View.X) + int (Pixels (X) * Tile_Width),
                  Y      => int (Engine.View.Y) + int (Pixels (Y) * Tile_Height),
                  others => 0);
            begin
               Renderers.Copy (Self      => Engine.Renderer,
                               Copy_From => Engine.Map.Tile,
                               From      => From,
                               To        => To);
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
               Sprite : Pig_Sprite renames
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
      use SDL.Video;

      Old_Dirty : constant Page_Index := Work;
      Fframe    : constant Float := Float (Engine.Time
                                          - Long_Float'Floor (Engine.Time));
   begin
      Renderers.Set_Clip (Engine.Renderer, Engine.View);

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
              := Position_X (Float (Object.Interpol.Ox) * (1.0 - Fframe)
                             + Float (Object.X) * Fframe);
            Object.Interpol.Gy
              := Position_Y (Float (Object.Interpol.Oy) * (1.0 - Fframe)
                             + Float (Object.Y) * Fframe);
         else
            Object.Interpol.Gx := Position_X (Object.X);
            Object.Interpol.Gy := Position_Y (Object.Y);
         end if;
         Object.Interpol.Gimage := Sprite_Counts (Object.I_Base + Object.Image);

         --  Render the sprite!
         if Object.Interpol.Gimage in Sprite_Index'First .. Engine.Sprite_Last then
            declare
               subtype int is SDL.C.int;

               Sprite      : Pig_Sprite renames
                 Engine.Sprites (Sprite_Index (Object.Interpol.Gimage));

               Source_Area : constant Rectangle := (0, 0, 0, 0);

               Target_Area : constant Rectangle :=
                 (X => int (Float (Object.Interpol.Gx) - Float (Sprite.Hot_X)
                              + Float (Engine.View.X)),
                  Y => int (Float (Object.Interpol.Gy) - Float (Sprite.Hot_Y)
                              + Float (Engine.View.Y)),
                  others => 0);
            begin
               Renderers.Copy
                 (Self      => Engine.Renderer,
                  Copy_From => Engine.Sprites (Object.Interpol.Gimage).Textur,
                  From      => Source_Area,
                  To        => Target_Area);

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
      Dirty.Merge_Tables (Engine.Dirty (Engine.Work),
                          Engine.Dirty (Old_Dirty));
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
      use SDL.Video;

      Colour : constant Palettes.Colour
             := (Red   => 255,
                 Green => 0,
                 Blue  => 255,
                 Alpha => 128);
   begin
--      if Engine.Buffer_Is_Null then
--  .Internal = null then --  SDL.Video.Textures.Null_Texture then
--         declare
--            Format : constant not null Format_Access :=
--              Engine.Screen.Pixel_Format;
--            Size : SDL.Sizes;
--         begin
--            Renderers.Get_Logical_Size (Engine.Renderer, Size);
--            Textures.Makers.Create
--              (Tex      => Engine.Buffer,
--               Renderer => Engine.Renderer,
--               Format   => null,
--               Kind     => Textures.Target,
--               Size     => Size);
--               Surface  => Engine.Win.Get_Surface);

--             Surfaces.Makers.Create --  RGBSurface
--               (Engine.Buffer,
----            SDL_SWSURFACE,
--                Engine.Screen.Size, -- .Width, Pe.Screen.Size.Height,
--                Pixel_Depths (Format.Bits), --  BPP, --  Bits_Per_Pixel,
--                Red_Mask   => Colour_Masks (Format.Red_Mask),
--                Green_Mask => Colour_Masks (Format.Green_Mask),
--                Blue_Mask  => Colour_Masks (Format.Blue_Mask),
--                Alpha_Mask => Colour_Masks (Format.Alpha_Mask));
--         end;
--         Engine.Buffer_Is_Null := False;
--         if Engine.Buffer = Null_Surface then
--            return;
--         end if;
--         Engine.Surfac := Engine.Buffer;
      Renderers.Copy (Self      => Engine.Renderer,
                      Copy_From => Engine.Buffer);

      Tile_Area (Engine, Engine.View);
--      end if;
--      if Engine.Buffer = Null_Surface then
--         return;
--      end if;

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

            Renderers.Copy (Self      => Engine.Renderer,
                            Copy_From => Engine.Buffer,
                            From      => R2,
                            To        => R);
         end;
      end loop;

      Renderers.Set_Draw_Colour (Engine.Renderer, Colour);

      for I in 1 .. Table.Last loop
         declare
            R : Rectangle;
         begin
            R := Table.Rects (I);
            R.Height := 1;
            Renderers.Fill (Engine.Renderer, R);

            R.Y := R.Y + Table.Rects (I).Height - 1;
            Renderers.Fill (Engine.Renderer, R);

            R := Table.Rects (I);
            R.Width := 1;
            Renderers.Fill (Engine.Renderer, R);

            R.X := R.X + Table.Rects (I).Width - 1;
            Renderers.Fill (Engine.Renderer, R);
         end;
      end loop;
   end Show_Rects;

   -----------------
   -- Pig_Present --
   -----------------

   procedure Pig_Present (Engine : in out Game_Engine;
                          Win    : in out Window)
   is
      use SDL.Video;

      Table : Dirty_Table renames Engine.Dirty (Engine.Work);
   begin
      Renderers.Set_Clip (Engine.Renderer, Null_Rectangle);
      Renderers.Set_Clip (Engine.Renderer, (0, 0, 800, 600 - 56));

      if Engine.Show_Dirtyrects then
         Show_Rects (Engine, Table);
         for I in 1 .. Table.Last loop
            declare
               Rect : Rectangle renames Table.Rects (I);
               Clip : Rectangle;
            begin
               Rect.X      := Rect.X - 32;
               Rect.Y      := Rect.Y - 32;
               Rect.Width  := Rect.Width + 64;
               Rect.Height := Rect.Height + 64;

               Renderers.Get_Clip (Engine.Renderer, Clip);
               Dirty.Intersect (Clip, Rect);
            end;
         end loop;

      else --  if Engine.Surfac = Engine.Buffer then
         for I in 1 .. Table.Last loop
            declare
               Rect_Copy : constant Rectangle := Table.Rects (I);
            begin
               Renderers.Copy (Self      => Engine.Renderer,
                               Copy_From => Engine.Buffer,
                               From      => Table.Rects (I),
                               To        => Rect_Copy);
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
         Renderers.Copy (Engine.Renderer, Copy_From => Engine.Map.Tile);
--         Renderers.Copy (Engine.Renderer,
--                         Copy_From => Text,
--                         From      => (0, 0, 50, 50),
--                         To        => (100, 100, 150, 150));
         --  Experiment by jq

         Renderers.Present (Engine.Renderer);
--         Renderers.Clear (Engine.Renderer);
--         Renderers.Copy (Engine.Renderer,
--                         Copy_From => Text,
--                         From      => (0, 0, 50, 50),
--                         To        => (100, 100, 150, 150));
         --  Experiment by jq

         Renderers.Draw (Engine.Renderer, Table.Rects.all);
      end if;

--      if Engine.Direct then
--         Engine.Surfac := Engine.Screen;
--      elsif Engine.Buffer = Null_Surface then
--         Engine.Surfac := Engine.Screen;
--      else
--         Engine.Surfac := Engine.Buffer;
         Renderers.Copy (Engine.Renderer, Copy_From => Engine.Buffer);
--      end if;

   end Pig_Present;

   ----------------------
   -- Pig_Draw_Sprites --
   ----------------------

   procedure Pig_Draw_Sprite (Engine : in out Game_Engine;
                              Frame  :        Sprite_Index;
                              X, Y   :        Pixels)
   is
      use SDL.Video;
      subtype C_int is SDL.C.int;

      Sprite : Pig_Sprite renames Engine.Sprites (Frame);

      Draw_Rect : constant Rectangle :=
        (X      => C_int (X - Sprite.Hot_X + Pixels (Engine.View.X)),
         Y      => C_int (Y - Sprite.Hot_Y + Pixels (Engine.View.Y)),
         Width  => C_int (Sprite.Width),
         Height => C_int (Sprite.Height));
   begin
      Renderers.Copy (Self      => Engine.Renderer,
                      Copy_From => Engine.Sprites (Frame).Textur,
                      To        => Draw_Rect);
   end Pig_Draw_Sprite;

   ------------------------------------------------------------
   --    Map
   ------------------------------------------------------------

   ------------------
   -- Pig_Map_Open --
   ------------------

   procedure Pig_Map_Open (Engine : in out Game_Engine;
                           Width  :        Tiles;
                           Height :        Tiles)
   is
   begin
      Pig_Map_Close (Engine.Map);

      Engine.Map.Width       := Width;
      Engine.Map.Height      := Height;
      Engine.Map.Map         := new Map_Array (0 .. Width - 1,
                                               0 .. Height - 1);
      Engine.Map.Hit         := new Hit_Array (0 .. Width - 1,
                                               0 .. Height - 1);
      Engine.Map.Tile_Width  := 0;
      Engine.Map.Tile_Height := 0;

      Engine.Map.Hit_Info    := (others => (others => False));

      Engine.Map.Hit.all     := (others => (others => No_Side));
      Engine.Map.Map.all     := (others => (others => 0));

   end Pig_Map_Open;

   -------------------
   -- Pig_Map_Close --
   -------------------

   procedure Pig_Map_Close (Map : in out Pig_Map)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Map_Array, Map_Array_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation (Hit_Array, Hit_Array_Access);
   begin
      null;
--      PIG_engine *pe = pm->owner;
--      if(pm->tiles)
--              SDL_FreeSurface(pm->tiles);
--      free(pm->hit);
--      free(pm->map);
      Free (Map.Map);
      Free (Map.Hit);
--      free(pe->map);
--      pe->map = NULL;
   end Pig_Map_Close;

   -------------------
   -- Pig_Map_Tiles --
   -------------------

   procedure Pig_Map_Tiles (Engine   : in out Game_Engine;
                            Filename :        String;
                            Width    :        Pixels;
                            Height   :        Pixels)
   is
      use SDL.Video;

      Surface : Surfaces.Surface;
   begin
      Engine.Map.Tile_Width  := Width;
      Engine.Map.Tile_Height := Height;

      SDL.Images.IO.Create (Surface, Filename);

      Textures.Makers.Create (Tex      => Engine.Map.Tile,
                              Renderer => Engine.Renderer,
                              Surface  => Surface);
   end Pig_Map_Tiles;

   -----------------------
   -- Pig_Map_Collision --
   -----------------------

   procedure Pig_Map_Collisions (Map   : in out Pig_Map;
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
         Map.Hit_Info (I) := Hit;
      end loop;

   end Pig_Map_Collisions;

   -------------------------
   -- Pig_Map_From_String --
   -------------------------

   procedure Pig_Map_From_String (Map   : in out Pig_Map;
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
            begin
               Position := Ada.Strings.Fixed.Index (Trans, "" & C);

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
              Map.Hit_Info (Map.Map (X, Y));
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
      Object : constant not null Object_Access := Get_Object (Engine);
   begin
      Object.Tile_Mask := All_Sides;
      Object.Hit_Mask  := 0;
      Object.Hit_Group := 0;

      if Last then
         Engine.Objects.Append (Object);
      else
         Engine.Objects.Prepend (Object);
      end if;

      Object.X           := Position (X);
      Object.Y           := Position (Y);
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

   function Find_Object (Engine :        Game_Engine'Class;
                         Start  : in out Game_Object;
                         Id     :        Object_Id) return Object_Access
   is
      pragma Unreferenced (Start);
--  PIG_object *pig_object_find(PIG_object *start, int id)
--      Pob, Pof : Object_Access;
   begin
      for Object of Engine.Objects loop
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
