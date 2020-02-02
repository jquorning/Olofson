--------------------------------------------------------------
--    Fixed Rate Pig - a fixed logic frame rate demo
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

   --  Size of sprite frame table
   PIG_MAX_SPRITES      : constant := 1024;

   Clean_Engine : constant PIG_Engine :=
     (SDL.Video.Surfaces.Null_Surface,
      SDL.Video.Surfaces.Null_Surface,
      SDL.Video.Surfaces.Null_Surface,
      0, SDL.Video.Rectangles.Null_Rectangle, 0, (null, null),
      False, False, False, 0.0, 0, null, Object_Lists.Empty_List, 0, 0, null, null, null, 0);

   Clean_Object : constant PIG_Object :=
     (Owner => null, Id => 0, Ibase => 0, Image => 0,
      Ip    => (Gimage => 0, others => 0.0),
      Tilemask => PIG_None,
      Hitmask  => 0, Hitgroup => 0,
      Timer    => (0, 0, 0),
      Age      => 0, Score => 0, Power => 0, Target => 0,
      State    => Object_States'First,
      Handler  => null, others => 0.0);

   procedure Close_Object (Object : in out PIG_Object);
   --  Actually remove an objects. Used internally,
   --  to remove objects that have been marked for
   --  destruction.

   ------------------------------------------------------------
   --      Engine
   ------------------------------------------------------------

   procedure Pig_Open (Engine :    out PIG_Engine_Access;
                       Screen : in     SDL.Video.Surfaces.Surface)
   is
   begin
      Engine := new PIG_Engine'(Clean_Engine);
      Engine.Screen   := Screen;
      Engine.Nsprites := 0;
--      if(!pe->screen)
--      {
--              pig_close(pe);
--              return NULL;
--      }
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
         Engine.Surface := Screen;
      end if;
--      pe->pages = 1 + ((screen->flags & SDL_DOUBLEBUF) == SDL_DOUBLEBUF);

      Engine.Interpolation := True;
      Engine.Time          := 0.0;
      Engine.View.Width    := Engine.Surface.Size.Width;
      Engine.View.Height   := Engine.Surface.Size.Height;

      Engine.Sprites := new Sprite_Array (0 .. PIG_MAX_SPRITES - 1);
--      pe->sprites = (PIG_sprite **)calloc(PIG_MAX_SPRITES,
--                      sizeof(PIG_sprite *));
--      if(!pe->sprites)
--      {
--              pig_close(pe);
--              return NULL;
--      }

--      pe->pagedirty[0] = pig_dirty_open(128);
--      pe->workdirty = pig_dirty_open(256);
--      if(!pe->pagedirty[0] || !pe->workdirty)
--      {
--              pig_close(pe);
--              return NULL;
--      }
--      if(pe->pages > 1)
--      {
--              pe->pagedirty[1] = pig_dirty_open(128);
--              if(!pe->pagedirty[1])
--              {
--                      pig_close(pe);
--                      return NULL;
--              }
--      }

      --      return pe;
   end Pig_Open;


   procedure Pig_Close (Pe : in out PIG_Engine) is
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
--      if(pe->pagedirty[0])
--              pig_dirty_close(pe->pagedirty[0]);
--      if(pe->pagedirty[1])
--              pig_dirty_close(pe->pagedirty[1]);
--      if(pe->workdirty)
--              pig_dirty_close(pe->workdirty);
--      free(pe);
   end Pig_Close;

   procedure Pig_Viewport (Engine : in out PIG_Engine;
                           X, Y   : in     Integer;
                           W, H   : in     Integer)
   is
      use SDL;
   begin
      Engine.View.X      := C.int (X);
      Engine.View.Y      := C.int (Y);
      Engine.View.Width  := C.int (W);
      Engine.View.Height := C.int (H);
   end Pig_Viewport;


   procedure Pig_Sprites (Engine   : in out PIG_Engine;
                          Filename : in     String;
                          Sw, Sh   : in     Integer;
                          Result   :    out Integer)
   is
      use SDL.C;
      X, Y, Count, Handle : Integer;
      Surface_Load  : SDL.Video.Surfaces.Surface;
      Sprite_Width  : Integer := Sw;
      Sprite_Height : Integer := Sh;
   begin
      Ada.Text_IO.Put_Line ("Loading: " & Filename);
      begin
         SDL.Images.IO.Create (Surface_Load, Filename);
      exception
         when others =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Could not load '" & Filename & "'!");
            raise;
      end;

      Handle := Engine.Nsprites;

      if Sprite_Width  = 0 then Sprite_Width  := Integer (Surface_Load.Size.Width);  end if;
      if Sprite_Height = 0 then Sprite_Height := Integer (Surface_Load.Size.Height); end if;

      --  Disable blending, so we get the alpha channel COPIED!
      Surface_Load.Set_Alpha_Blend (0);               --      SDL_SetAlpha(tmp, 0, 0);
      Surface_Load.Set_Blend_Mode  (SDL.Video.None);  --      SDL_SetAlpha(tmp, 0, 0);

      Count := 0;

      Y := 0;
      while Y <= Integer (Surface_Load.Size.Height) - Sprite_Height loop
         X := 0;
         while X <= Integer (Surface_Load.Size.Width) - Sprite_Width loop
            declare
               R, SA          : SDL.Video.Rectangles.Rectangle;
               Surface_Sprite : SDL.Video.Surfaces.Surface;
               Sprite         : PIG_Sprite_Access;
            begin
--                      if(pe->nsprites >= PIG_MAX_SPRITES)
--                      {
--                              fprintf(stderr, "Sprite bank full!\n");
--                              return -1;
--                      }
--                      s = (PIG_sprite *)calloc(1, sizeof(PIG_sprite));
               Sprite := new PIG_Sprite;
               --                      if(!s)
               --                              return -1;
               --  end if;
               Sprite.Width  := Sprite_Width;
               Sprite.Height := Sprite_Height;
               Sprite.Hotx   := Sprite_Width  / 2;
               Sprite.Hoty   := Sprite_Height / 2;
               Sprite.Radius := (Sprite_Width + Sprite_Height) / 5;
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
               R.X      := int (X);
               R.Y      := int (Y);
               R.Width  := int (Sprite_Width);
               R.Height := int (Sprite_Height);
               SA := (0, 0, 0, 0);
               SDL.Video.Surfaces.Blit (Source      => Surface_Load,
                                        Source_Area => SA,
                                        Self        => Surface_Sprite,
                                        Self_Area   => R);
--                 SDL.Video.Surfaces.Blit (Self        => Surface_Load,
--                                          Self_Area   => R,
--                                          Source      => Surface_Sprite,
--                                          Source_Area => SA);
               Surface_Sprite.Set_Alpha_Blend (100); --  (SDL_ALPHA_OPAQUE);
               Surface_Sprite.Set_Blend_Mode  (SDL.Video.None); --  SDL_SRCALPHA or SDL_RLEACCEL);
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
               Engine.Sprites (Engine.Nsprites) := Sprite;
               Engine.Nsprites := Engine.Nsprites + 1;
               Count := Count + 1;
            end;
            X := X + Sprite_Width;
         end loop;
         Y := Y + Sprite_Height;
      end loop;
      Ada.Text_IO.Put_Line ("Ending");
      --  Tmp.Free;  --      SDL_FreeSurface(tmp);
      Result := Handle;
--     exception
--        when Program_Error =>
--           Ada.Text_IO.Put_Line ("excep pig_sprites program_error");
--           raise;
--        when Storage_Error =>
--           Ada.Text_IO.Put_Line ("excep pig_sprites storage_error");
--           raise;
--        when Constraint_Error =>
--           Ada.Text_IO.Put_Line ("excep pig_sprites constraint_error");
--           raise;
--        when others =>
--           Ada.Text_IO.Put_Line ("excep pig_sprites others");
--           raise;
   end Pig_Sprites;


   procedure Pig_Hotspot (Engine     : in out PIG_Engine;
                          Frame      : in     Integer;
                          Hotx, Hoty : in     Integer)
   is
   begin
      if Frame not in 0 .. Engine.Nsprites - 1 then
         Ada.Text_IO.Put_Line ("Frame: " & Frame'Image);
         return;  --              return -1;
      end if;

      declare
         Sprite : PIG_Sprite_Access renames Engine.Sprites (Frame);
      begin
         case Hotx is
            when PIG_UNCHANGED =>  null;
            when PIG_MIN       =>  Sprite.Hotx := 0;
            when PIG_CENTER    =>  Sprite.Hotx := Sprite.Width / 2;
            when PIG_MAX       =>  Sprite.Hotx := Sprite.Width;
            when others        =>  Sprite.Hotx := Hotx;
         end case;

         case Hoty is
            when PIG_UNCHANGED =>  null;
            when PIG_MIN       =>  Sprite.Hoty := 0;
            when PIG_CENTER    =>  Sprite.Hoty := Sprite.Height / 2;
            when PIG_MAX       =>  Sprite.Hoty := Sprite.Height;
            when others        =>  Sprite.Hoty := Hoty;
         end case;
      end;
   end Pig_Hotspot;


--  int pig_radius(PIG_engine *pe, int frame, int radius)
--  {
--      if((frame < 0 ) || (frame >= pe->nsprites))
--              return -1;

--      pe->sprites[frame]->radius = radius;
--      return 0;
--  }


   procedure Pig_Start (Engine : in out PIG_Engine;
                        Frame  : in     Integer)
   is
   begin
      Engine.Time  := Long_Float (Frame);
      Engine.Frame := Frame;
      for Object of Engine.Objects loop
         Object.Ip.Gx := Object.X;
         Object.Ip.Ox := Object.X;
         Object.Ip.Gy := Object.Y;
         Object.Ip.Oy := Object.Y;
         Object.Ip.Gimage := Object.Ibase + Object.Image;
      end loop;
   end Pig_Start;


   procedure Run_Timers (Engine : in out PIG_Engine;
                         Object : in out PIG_Object);

   procedure Run_Timers (Engine : in out PIG_Engine;
                         Object : in out PIG_Object)
   is
      pragma Unreferenced (Engine);
   begin
      for I in 0 .. PIG_TIMERS - 1 loop
         if Object.Timer (I) /= 0 then

            Object.Timer (I) := Object.Timer (I) - 1;
            if Object.Timer (I) = 0 then
               declare
                  Event : PIG_Event;
               begin
                  Event.Type_C := PIG_Events'Val (PIG_Events'Pos (PIG_TIMER0) + I);
                  Object.Handler (Object, Event);
                  if Object.Id = 0 then
                     return;
                  end if;
               end;
            end if;
         end if;
      end loop;
   end Run_Timers;


   procedure Test_Offscreen (Pe : in out PIG_Engine;
                             Po : in out PIG_Object;
                             S  : in     PIG_Sprite_Access);
   procedure Test_Offscreen (Pe : in out PIG_Engine;
                             Po : in out PIG_Object;
                             S  : in     PIG_Sprite_Access)
   is
      use SDL.C;
      Event : PIG_Event;
      Hx, Hy, W, H : Integer;
   begin
      if S /= null then
         Hx := S.Hotx;
         Hy := S.Hoty;
         W  := S.Width;
         H  := S.Height;
      else
         Hx := 0; Hy := 0; W := 0; H := 0;
      end if;

      Event.Cinfo.Sides.Top    := Integer (Po.Y) - Hy < -H;
      Event.Cinfo.Sides.Botton := Integer (Po.Y) - Hy >= Integer (Pe.View.Height);
      Event.Cinfo.Sides.Left   := Integer (Po.X) - Hx < -W;
      Event.Cinfo.Sides.Right  := Integer (Po.X) - Hx >= Integer (Pe.View.Width);

      if Event.Cinfo.Sides /= PIG_None then
         declare
            Dx : constant Float := Po.X - Po.Ip.Ox;
            Dy : constant Float := Po.Y - Po.Ip.Oy;
         begin

            if Event.Cinfo.Sides.Top then
               Event.Cinfo.Y := 0;
               if Dy /= 0.0 then
                  Event.Cinfo.X := Integer (Po.Ip.Ox - Dx * Po.Ip.Oy / Dy);
               end if;

            elsif Event.Cinfo.Sides.Botton then
               Event.Cinfo.Y := Integer (Pe.View.Height - 1);
               if Dy /= 0.0 then
                  Event.Cinfo.X := Integer (Po.Ip.Ox + Dx *
                                              (Float (Event.Cinfo.Y) - Po.Ip.Oy) / Dy);
               end if;
            end if;

            if Event.Cinfo.Sides.Left then
               Event.Cinfo.X := 0;
               if Dx /= 0.0 then
                  Event.Cinfo.Y := Integer (Po.Ip.Oy - Dy * Po.Ip.Ox / Dx);
               end if;

            elsif Event.Cinfo.Sides.Right then
               Event.Cinfo.X := Integer (Pe.View.Width - 1);
               if Dx not in -0.01 .. 0.01 then
                  Event.Cinfo.Y := Integer (Po.Ip.Oy + Dy *
                                              (Float (Event.Cinfo.X) - Po.Ip.Ox) / Dx);
               end if;
            end if;

            Event.Type_C := PIG_OFFSCREEN;
            Po.Handler (Po, Event);
         end;
      end if;
   end Test_Offscreen;


--  /* Test for stationary sprite/sprite collision */
--  static void sprite_sprite_one(PIG_object *po, PIG_object *po2, float t, float hitdist)
--  {
--      float dx, dy, dsquare;
--      PIG_event ev;
--      int sides;
--      float ix = po->ip.ox * (1 - t) + po->x * t;
--      float iy = po->ip.oy * (1 - t) + po->y * t;
--      float ix2 = po2->ip.ox * (1 - t) + po2->x * t;
--      float iy2 = po2->ip.oy * (1 - t) + po2->y * t;
--      dx = ix - ix2;
--      dy = iy - iy2;
--      dsquare = dx*dx + dy*dy;
--      if(dsquare >= hitdist*hitdist)
--              return;         /* Nothing... --> */

--      if(fabs(dsquare) < 1)
--              sides = PIG_ALL;
--      else
--      {
--              float d = sqrt(dsquare);
--              dx /= d;
--              dy /= d;
--              if(dx < -0.707)
--                      sides = PIG_LEFT;
--              else if((dx > 0.707))
--                      sides = PIG_RIGHT;
--              else
--                      sides = 0;
--              if(dy < -0.707)
--                      sides |= PIG_TOP;
--              else if((dy > 0.707))
--                      sides |= PIG_BOTTOM;
--      }
--      ev.type = PIG_HIT_OBJECT;
--      ev.cinfo.ff = 0.0;

--      ev.cinfo.x = ix;
--      ev.cinfo.y = iy;
--      ev.cinfo.sides = sides;
--      if(po->hitmask & po2->hitgroup)
--      {
--              ev.obj = po2;
--              po->handler(po, &ev);
--      }

--      if(po2->id && (po2->hitmask & po->hitgroup))
--      {
--              int s;
--              ev.cinfo.x = ix2;
--              ev.cinfo.y = iy2;
--              s = ((sides >> PIG_LEFT_B) & 1) << PIG_RIGHT_B;
--              s |= ((sides >> PIG_RIGHT_B) & 1) << PIG_LEFT_B;
--              s |= ((sides >> PIG_TOP_B) & 1) << PIG_BOTTOM_B;
--              s |= ((sides >> PIG_BOTTOM_B) & 1) << PIG_TOP_B;
--              ev.cinfo.sides = s;
--              ev.obj = po;
--              po2->handler(po2, &ev);
--      }
--  }


--  /*
--   * Check 'po' against all subsequent objects in the list.
--   * The testing is step size limited so that neither object
--   * moves more than 25% of the collision distance between tests.
--   * (25% should be sufficient for correct direction flags.)
--   */
--  static void test_sprite_sprite(PIG_engine *pe, PIG_object *po, PIG_sprite *s)
--  {
--      int image;
--      PIG_object *po2, *next2;
--      for(po2 = po->next; po2; po2 = next2)
--      {
--              float hitdist, d, dmax, t, dt;
--              next2 = po2->next;
--              if(!po->id || !po2->id)
--                      break;

--              /* Check collision groups and masks */
--              if(!(po->hitmask & po2->hitgroup) &&
--                              !(po2->hitmask & po->hitgroup))
--                      continue;

--              /* Calculate minimum distance */
--              hitdist = s ? s->radius : 0;
--              image = po2->ibase + po2->image;
--              if((image >= 0) && (image < pe->nsprites))
--                      hitdist += pe->sprites[image]->radius;
--              if(hitdist < 1)
--                      hitdist = 1;

--              /* Calculate number of testing steps */
--              dmax = fabs(po->ip.ox - po->x);
--              d = fabs(po->ip.oy - po->y);
--              dmax = d > dmax ? d : dmax;
--              d = fabs(po2->ip.ox - po2->x);
--              dmax = d > dmax ? d : dmax;
--              d = fabs(po2->ip.oy - po2->y);
--              dmax = d > dmax ? d : dmax;
--              if(dmax > 1)
--                      dt = hitdist / (dmax * 4);
--              else
--                      dt = 1;

--              /* Sweep test! */
--              for(t = 0; t < 1; t += dt)
--                      sprite_sprite_one(po, po2, t, hitdist);
--      }
--  }


   --  Returns a non-zero value if the tile at (x, y) is marked for
   --  collisions on the side indicated by 'mask'.
   function Check_Tile (M    : in PIG_Map_Access;
                        X, Y : in Integer;
                        Mask : in Pig_Sides) return Boolean;
   function Check_Tile (M    : in PIG_Map_Access;
                        X, Y : in Integer;
                        Mask : in Pig_Sides) return Boolean
   is
      pragma Unreferenced (M, X, Y, Mask);
   --  static __inline__ int check_tile(PIG_map *m, int x, int y, int mask)
--  {
--      int mx, my;
   begin
      --      /*
--       * Must check < 0 first! (Division rounds
--       * towards zero - not downwards.)
--       */
--      if(x < 0 || y < 0)
--              return PIG_NONE;

--      mx = x / m->tw;
--      my = y / m->th;
--      if(mx >= m->w || my >= m->h)
--              return PIG_NONE;

--      return m->hit[my * m->w + mx] & mask;
      return False;
   end Check_Tile;


   function Pig_Test_Map (Engine : in PIG_Engine;
                          X, Y   :    Integer) return Pig_Sides
   is -- Boolean is
      Mx, My : Integer;
   begin
      if X < 0 or Y < 0 then
         return PIG_None;
      end if;
      Mx := X / Engine.Map.Tw;
      My := Y / Engine.Map.Th;
      if Mx >= Engine.Map.Width or My >= Engine.Map.Height then
         return PIG_None;
      end if;
      return Engine.Map.Hit (Mx, My);
   end Pig_Test_Map;


   --  Simple implementation that checks only for top edge collisions.
   --  (Full top/bottom/left/right checks with proper handling of
   --  corners and rows of tiles is a lot more complicated, so I'll
   --  leave that out for now, rather than hacking something simple
   --  but incorrect.)
   Lci : aliased PIG_Cinfo;
   function Pig_Test_Map_Vector (Engine         : in out PIG_Engine;
                                 X1, Y1, X2, Y2 : in     Integer;
                                 Mask           : in     Pig_Sides;
                                 Ci             : in     PIG_Cinfo_Access)
                                return Pig_Sides
   is
      use Ada.Numerics.Elementary_Functions;
      Ci2  : PIG_Cinfo_Access := Ci;
      M    : constant PIG_Map_Access := Engine.Map;
      X, Y : Integer;
      Dist : Integer := 2_000_000_000;
   begin
      if Ci2 = null then
         Ci2 := Lci'Access;
      end if;
      Ci2.Sides := PIG_None;
      if Mask.Top and Y1 < Y2 then

         --  Test for tiles that can be hit from the top
         Y := Y1 + M.Th - Y1 mod M.Th;
         while Y <= Y2 loop
            X := X1 + (X2 - X1) * (Y - Y1) / (Y2 - Y1);
            if Check_Tile (M, X, Y + 1, PIG_Top) then
               Dist := (X - X1) * (X - X1) + (Y - Y1) * (Y - Y1);
               Ci2.X := X;
               Ci2.Y := Y - 1;
               Ci2.Sides.Top := True;
               exit;
            end if;
            Y := Y + M.Th;
         end loop;
      end if;

      if Ci.Sides /= PIG_None then
         Ci.Ff := Sqrt (Float ((X2 - X1) * (X2 - X1) +
                                 (Y2 - Y1) * (Y2 - Y1) / Dist));
      end if;
      return Ci.Sides;
   end Pig_Test_Map_Vector;


   procedure Test_Sprite_Map (Engine : in out PIG_Engine;
                              Object : in out PIG_Object;
                              Sprite : in     PIG_Sprite_Access);

   procedure Test_Sprite_Map (Engine : in out PIG_Engine;
                              Object : in out PIG_Object;
                              Sprite : in     PIG_Sprite_Access)
   is
      pragma Unreferenced (Sprite);
      Cinfo : aliased PIG_Cinfo;
      Event : PIG_Event;
   begin
      if PIG_None /= Pig_Test_Map_Vector (Engine, Integer (Object.Ip.Ox), Integer (Object.Ip.Oy),
                                          Integer (Object.X), Integer (Object.Y),
                                          Object.Tilemask, Cinfo'Unchecked_Access)
      then
         Event.Cinfo  := Cinfo;
         Event.Type_C := PIG_HIT_TILE;
         Object.Handler (Object, Event);
      end if;
   end Test_Sprite_Map;


   procedure Run_Logic (Engine : in out PIG_Engine);
   procedure Run_Logic (Engine : in out PIG_Engine)
   is
      use Object_Lists;
      Object_Cursor, Next_Cursor : Cursor;
      Image  : Integer;
   begin
      --  Shift logic coordinates
      for Object of Engine.Objects loop
         Object.Ip.Ox := Object.X;
         Object.Ip.Oy := Object.Y;
      end loop;

      if Engine.Before_Objects /= null then
         Engine.Before_Objects (Engine);
      end if;

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

            Event.Type_C := PIG_PREFRAME;
--            Object.Handler (Object.all, Event);
            Element (Object_Cursor).Handler (Element (Object_Cursor).all, Event);
         end;
         Object_Cursor := Next_Cursor;
      end loop;

      for Object of Engine.Objects loop
         declare
            Sprite : PIG_Sprite_Access;
         begin
            --  next = po->next;
            Image := Object.Ibase + Object.Image;
            if (Image >= 0) and (Image < Engine.Nsprites) then
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
                  Test_Offscreen (Engine, Object.all, Sprite);
               end if;

--                 if Object.Id /= 0 and (Object.Hitmask or Object.Hitgroup) then
--                    Test_Sprite_Sprite (Engine, Object, sprite);
--                 end if;

               if Object.Id /= 0 and Object.Tilemask /= PIG_None then
                  Test_Sprite_Map (Engine, Object.all, Sprite);
               end if;
            end if;
         end;
      end loop;

      for Object of Engine.Objects loop
--            next = po->next;
         if Object.Id /= 0 then
            declare
               Event : PIG_Event;
            begin
               Event.Type_C := PIG_POSTFRAME;
               Object.Handler (Object.all, Event);
               Object.Age := Object.Age + 1;
            end;
         end if;
      end loop;

      if Engine.After_Objects /= null then
         Engine.After_Objects (Engine);
      end if;

   end Run_Logic;


   procedure Pig_Animate (Engine : in out PIG_Engine;
                          Frames : in     Float)
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


   procedure Pig_Dirty (Engine : in out PIG_Engine;
                        Rect   : in SDL.Video.Rectangles.Rectangle)
   is
      pragma Unreferenced (Rect);
      use type SDL.C.int;
      R : SDL.Video.Rectangles.Rectangle;
   begin
      R.X      := 0;
      R.Y      := 0;
      R.Width  := Engine.Surface.Size.Width;
      R.Height := Engine.Surface.Size.Height;
--        if Rect then
--           Pig_Intersectrect (Dr, R);
--        end if;
      if R.Width /= 0 and R.Height /= 0 then
         null;
         --  Dirty.Pig_Dirty_Add (Engine.Pagedirty (Engine.Page).all, R);
      end if;
   end Pig_Dirty;


   procedure Tile_Area (Engine : in out PIG_Engine;
                        R      : in SDL.Video.Rectangles.Rectangle);

   procedure Tile_Area (Engine : in out PIG_Engine;
                        R      : in SDL.Video.Rectangles.Rectangle)
   is
      use type SDL.C.int;
      Cr : SDL.Video.Rectangles.Rectangle;
      Startx, Starty, Maxx, Maxy, Tilesperrow : Integer;
   begin
      Cr := R;
      Cr.X := Cr.X + Engine.View.X;
      Cr.Y := Cr.Y + Engine.View.Y;
--      Engine.Surface.Set_Clip_Rectangle (Cr);

      Startx := Integer (R.X) / Engine.Map.Tw;
      Starty := Integer (R.Y) / Engine.Map.Th;
      Maxx   := (Integer (R.X) + Integer (R.Width)  + Engine.Map.Tw - 1) / Engine.Map.Tw;
      Maxy   := (Integer (R.Y) + Integer (R.Height) + Engine.Map.Th - 1) / Engine.Map.Th;
      if Maxx > Engine.Map.Width  - 1 then Maxx := Engine.Map.Width  - 1; end if;
      if Maxy > Engine.Map.Height - 1 then Maxy := Engine.Map.Height - 1; end if;
      Tilesperrow := Integer (Engine.Map.Tiles.Size.Width) / Engine.Map.Tw;

      for Y in Starty .. Maxy loop
         for X in Startx .. Maxx loop
            declare
               use SDL.C;
               From, To : SDL.Video.Rectangles.Rectangle;
               C2 : constant Integer := Integer (Engine.Map.Map (X, Y));
            begin
               From.X      := int (C2 mod Tilesperrow * Engine.Map.Tw);
               From.Y      := int (C2 / Tilesperrow   * Engine.Map.Th);
               From.Width  := int (Engine.Map.Tw);
               From.Height := int (Engine.Map.Th);
               To.X        := int (Engine.View.X) + int (X * Engine.Map.Tw);
               To.Y        := int (Engine.View.Y) + int (Y * Engine.Map.Th);
               SDL.Video.Surfaces.Blit (Source      => Engine.Map.Tiles,
                                        Source_Area => From,
                                        Self        => Engine.Surface,
                                        Self_Area   => To);
            end;
         end loop;
      end loop;
   end Tile_Area;


   procedure Remove_Sprites (Engine : in out PIG_Engine);
   procedure Remove_Sprites (Engine : in out PIG_Engine)
   is
      use SDL.C;
      R : SDL.Video.Rectangles.Rectangle;
      S : PIG_Sprite_Access;
--      PIG_object *po, *next;
   begin
      --       * Remove all objects, using the information that
      --       * remains from the last frame. The actual removal
      --       * is done by drawing over the sprites with tiles
      --       * from the map.
      --       *
      --       * We assume that most objects don't overlap. If
      --       * they do that a lot, we could use a "dirty map"
      --       * to avoid rendering the same tiles multiple times
      --       * in the overlapping areas.
      for Object of Engine.Objects loop
--              next = po->next;
         if Object.Ip.Gimage in 0 .. Engine.Nsprites - 1 then

            S := Engine.Sprites (Object.Ip.Gimage);
            R.X      := int (Object.Ip.Gx) - int (S.Hotx);
            R.Y      := int (Object.Ip.Gy) - int (S.Hoty);
            R.Width  := int (S.Width);
            R.Height := int (S.Height);
--              pig_intersectrect(&pe->view, &r);
            if R.Width /= 0 and R.Height /= 0 then
               if R.Y >= 0 then --  JQ
                  Tile_Area (Engine, R);
               end if;
            end if;

            --  Delete dead objects *after* they've
            --  been removed from the rendering buffer!
            if Object.Id = 0 then
               null;
               --  Close_Object (Object);
            end if;
         end if;
      end loop;
   end Remove_Sprites;


   procedure Draw_Sprites (Engine : in out PIG_Engine);

   procedure Draw_Sprites (Engine : in out PIG_Engine)
   is
--      PIG_dirtytable *pdt;
      S      : PIG_Sprite_Access;
--      Object : PIG_Object_Access;
      Fframe : constant Float := Float (Engine.Time - Long_Float'Floor (Engine.Time));
   begin
      Engine.Surface.Set_Clip_Rectangle (Engine.View);
      --  SDL_SetClipRect(pe->surface, &pe->view);

--      /* Swap the work and display/back page dirtytables */
--      pdt = pe->workdirty;
--      pe->workdirty = pe->pagedirty[pe->page];
--      pe->pagedirty[pe->page] = pdt;

--      /* Clear the display/back page dirtytable */
--      pdt->count = 0;

      --  Update positions and render all objects
      for Object of Engine.Objects loop

         --  Calculate graphic coordinates
         if False then --  Engine.Interpolation then
            Object.Ip.Gx := Object.Ip.Ox * (1.0 - Fframe) + Object.X * Fframe;
            Object.Ip.Gy := Object.Ip.Oy * (1.0 - Fframe) + Object.Y * Fframe;
         else
            Object.Ip.Gx := Object.X;
            Object.Ip.Gy := Object.Y;
         end if;
         Object.Ip.Gimage := Object.Ibase + Object.Image;

         --  Render the sprite!
         if Object.Ip.Gimage >= 0 and Object.Ip.Gimage < Engine.Nsprites then
            declare
               use SDL.C;
               Self_Area : SDL.Video.Rectangles.Rectangle := (0, 0, 0, 0);
               Dr        : SDL.Video.Rectangles.Rectangle;
            begin
               S := Engine.Sprites (Object.Ip.Gimage);
               Dr.X := int (Object.Ip.Gx - Float (S.Hotx) + Float (Engine.View.X));
               Dr.Y := int (Object.Ip.Gy - Float (S.Hoty) + Float (Engine.View.Y));
               SDL.Video.Surfaces.Blit (Source      => Engine.Sprites (Object.Ip.Gimage).Surface,
                                        Source_Area => Self_Area,
                                        Self        => Engine.Surface,
                                        Self_Area   => Dr);
--          SDL.Video.Surfaces.Blit (Self        => Engine.Sprites (Object.Ip.Gimage).Surface,
--                                          Self_Area   => Self_Area,
--                                          Source      => Engine.Surface,
--                                          Source_Area => Dr);
               --
               --  We use the clipped rect for the dirtyrect!
               --
--                 if Dr.Width /= 0 and Dr.Height /= 0 then
--                    null;
--                    --  Dirty.pig_dirty_add(pdt, &dr);
--                 end if;
            end;
         end if;
         --              po = po->next;
      end loop;

      --  Merge the display/back page table into the work table
      --      pig_dirty_merge(pe->workdirty, pdt);
   end Draw_Sprites;


   procedure Pig_Refresh (Engine : in out PIG_Engine) is
   begin
      Remove_Sprites (Engine);
      Draw_Sprites   (Engine);
   end Pig_Refresh;


   procedure Pig_Refresh_All (Engine : in out PIG_Engine) is
   begin
      Tile_Area (Engine, Engine.View);
      --      Pig_Dirty (Pe, null);
      Draw_Sprites (Engine);
   end Pig_Refresh_All;


   procedure Show_Rects (Pe  : in out PIG_Engine;
                         Pdt : in     Dirty.PIG_Dirtytable);
   procedure Show_Rects (Pe  : in out PIG_Engine;
                         Pdt : in     Dirty.PIG_Dirtytable)
   is
      use SDL.Video.Surfaces;
      Color  : Interfaces.Unsigned_32;
      Format : SDL.Video.Pixel_Formats.Pixel_Format_Access;
   begin
      if Pe.Buffer = Null_Surface then
         Format := Pe.Screen.Pixel_Format;
         SDL.Video.Surfaces.Makers.Create --  RGBSurface
           (Pe.Buffer,
--            SDL_SWSURFACE,
            Pe.Screen.Size, -- .Width, Pe.Screen.Size.Height,
            Pixel_Depths (Format.Bits), --  BPP, --  Bits_Per_Pixel,
            Red_Mask   => Colour_Masks (Format.Red_Mask),
            Green_Mask => Colour_Masks (Format.Green_Mask),
            Blue_Mask  => Colour_Masks (Format.Blue_Mask),
            Alpha_Mask => Colour_Masks (Format.Alpha_Mask));
         if Pe.Buffer = Null_Surface then
            return;
         end if;
         Pe.Surface := Pe.Buffer;
         Tile_Area (Pe, Pe.View);
      end if;
      if Pe.Buffer = Null_Surface then
         return;
      end if;

      Pe.Direct := False;

      for I in 0 .. Pdt.Count - 1 loop
         declare
            use type SDL.C.int;
            R  : SDL.Video.Rectangles.Rectangle;
            R2 : SDL.Video.Rectangles.Rectangle;
         begin
            R        := Pdt.Rects (I);
            R.X      := R.X - 32;
            R.Y      := R.Y - 32;
            R.Width  := R.Width  + 64;
            R.Height := R.Height + 64;
            R2 := R;
            SDL.Video.Surfaces.Blit (Source      => Pe.Buffer,
                                     Source_Area => R2,
                                     Self        => Pe.Screen,
                                     Self_Area   => R);
         end;
      end loop;

      Color := SDL.Video.Pixel_Formats.To_Pixel (Pe.Screen.Pixel_Format, 255, 0, 255);
      for I in 0 .. Pdt.Count - 1 loop
         declare
            use SDL.C;
            R : SDL.Video.Rectangles.Rectangle;
         begin
            R := Pdt.Rects (I);
            R.Height := 1;
            Pe.Screen.Fill (R, Color);
            R.Y := R.Y + Pdt.Rects (I).Height - 1;
            Pe.Screen.Fill (R, Color);
            R := Pdt.Rects (I);
            R.Width := 1;
            Pe.Screen.Fill (R, Color);
            R.X := R.X + Pdt.Rects (I).Width - 1;
            Pe.Screen.Fill (R, Color);
         end;
      end loop;
   end Show_Rects;


   procedure Pig_Flip (Engine : in out PIG_Engine)
   is
      use SDL.Video.Surfaces;
--      PIG_dirtytable *pdt = pe->workdirty;
--      int i;
   begin
--      SDL_SetClipRect(pe->surface, NULL);

--      if(pe->show_dirtyrects)
--      {
--              show_rects(pe, pdt);
--              for(i = 0; i < pdt->count; ++i)
--              {
--                      pdt->rects[i].x -= 32;
--                      pdt->rects[i].y -= 32;
--                      pdt->rects[i].w += 64;
--                      pdt->rects[i].h += 64;
--                      pig_intersectrect(&pe->buffer->clip_rect, &pdt->rects[i]);
--              }
--      }
--      else if(pe->surface == pe->buffer)
--              for(i = 0; i < pdt->count; ++i)
--                      SDL_BlitSurface(pe->buffer, pdt->rects + i,
--                                      pe->screen, pdt->rects + i);

--      if((pe->screen->flags & SDL_HWSURFACE) == SDL_HWSURFACE)
--      {
--              SDL_Flip(pe->screen);
--              if(pe->pages > 1)
--                      pe->page = 1 - pe->page;
--      }
--      else
--              SDL_UpdateRects(pe->screen, pdt->count, pdt->rects);

      if Engine.Direct then
         Engine.Surface := Engine.Screen;
      elsif Engine.Buffer = Null_Surface then
         Engine.Surface := Engine.Screen;
      else
         Engine.Surface := Engine.Buffer;
      end if;
   end Pig_Flip;


   procedure Pig_Draw_Sprite (Engine : in out PIG_Engine;
                              Frame  : in     Integer;
                              X, Y   : in     Integer)
   is
      use SDL.C;
      DR : SDL.Video.Rectangles.Rectangle;
      SA : SDL.Video.Rectangles.Rectangle := (0, 0, 0, 0);
   begin
      --      if(frame >= pe->nsprites)
--              return;
      DR.X := int (X - Engine.Sprites (Frame).Hotx + Integer (Engine.View.X));
      DR.Y := int (Y - Engine.Sprites (Frame).Hoty + Integer (Engine.View.Y));
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

   procedure Pig_Map_Open (Map           :    out PIG_Map_Access;
                           Engine        : in out PIG_Engine;
                           Width, Height : in     Integer)
   is
--  PIG_map *pig_map_open(PIG_engine *pe, int w, int h)
   begin
      Ada.Text_IO.Put_Line ("Pig_Map_Open");
      if Engine.Map /= null then
         Pig_Map_Close (Engine.Map.all);
      end if;

      Engine.Map := new PIG_Map;
--      if(!pe->map)
--              return NULL;

--      Engine.Map.Owner := Engine;
      Engine.Map.Width  := Width;
      Engine.Map.Height := Height;
      Engine.Map.Hit    := new Hit_Array (0 .. Width - 1, 0 .. Height - 1);
      --(unsigned char *)calloc(w, h);
--      if(!pe->map->hit)
--      {
--              pig_map_close(pe->map);
--              return NULL;
--      }
      Engine.Map.Map := new Map_Array (0 .. Width - 1, 0 .. Height - 1);
      --  (unsigned char *)calloc(w, h);
--      if(!pe->map->map)
--      {
--              pig_map_close(pe->map);
--              return NULL;
--      }
      Map := Engine.Map;
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
                            Filename : in     String;
                            Tw, Th   : in     Integer;
                            Result   :    out Integer)
   is
      pragma Unreferenced (Result);
--  int pig_map_tiles(PIG_map *pm, const char *filename, int tw, int th)
--  {
      Tmp : SDL.Video.Surfaces.Surface;
   begin
      Map.Tw := Tw;
      Map.Th := Th;
      SDL.Images.IO.Create (Tmp, Filename);
--      if(!tmp)
--      {
--              fprintf(stderr, "Could not load '%s'!\n", filename);
--              return -1;
--      }
--      pm->tiles = SDL_DisplayFormat(tmp);
      Map.Tiles := Tmp;
      --      if(!pm->tiles)
--      {
--              fprintf(stderr, "Could not convert '%s'!\n", filename);
--              return -1;
--      }
--      SDL_FreeSurface(tmp);
--      return 0;
   end Pig_Map_Tiles;


   procedure Pig_Map_Collisions (Map   : in out PIG_Map;
                                 First : in     Natural;
                                 Count : in     Natural;
                                 Sides : in     Pig_Sides)
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
                                  Trans : in     String;
                                  Data  : in     String)
   is
      use Ada.Strings.Fixed;
      Z : Natural;
   begin
      Ada.Text_IO.Put_Line ("## 4-1");
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
      Ada.Text_IO.Put_Line ("## 4-2");

      --  Generate collision map
      for Y in 0 .. Map.Height - 1 loop
         for X in 0 .. Map.Width - 1 loop
            Map.Hit (X, Y) :=
              Map.Hitinfo (Map.Map (X, Y));
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("## 4-3 done");
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Map string too short!");
         raise;
   end Pig_Map_From_String;


   ------------------------------------------------------------
   --      Object
   ------------------------------------------------------------

   function Get_Object (Engine : in out PIG_Engine) return PIG_Object_Access;
   function Get_Object (Engine : in out PIG_Engine) return PIG_Object_Access
   is
      Object : PIG_Object_Access;
   begin
      --      if(pe->object_pool)
--      {
--              po = pe->object_pool;
--              pe->object_pool = po->next;
--              memset(po, 0, sizeof(PIG_object));
--      }
--      else
--      {
      Object := new PIG_Object'(Clean_Object);
      --              po = (PIG_object *)calloc(1, sizeof(PIG_object));
--              if(!po)
--                      return NULL;
--      }
      Engine.Object_Id_Counter := Engine.Object_Id_Counter + 1;
      Object.Id := Engine.Object_Id_Counter;
      return Object;
   end Get_Object;


   procedure Free_Object (Object : in out PIG_Object);

   procedure Free_Object (Object : in out PIG_Object) is
   begin
--      Object.Owner.Objects.Delete (Object);
--      po->prev = NULL;
--      po->next = po->owner->object_pool;
--      po->owner->object_pool = po;
      Object.Id := 0;
   end Free_Object;


   function Pig_Object_Open (Engine : in PIG_Engine_Access;
                             X, Y   : in Integer;
                             Last   : in Integer) return PIG_Object_Access
   is
      pragma Unreferenced (Last);
      Object : constant PIG_Object_Access := Get_Object (Engine.all);
   begin
--      if(!po)
--              return NULL;

      Object.Owner    := Engine;
      Object.Tilemask := PIG_All;
      Object.Hitmask  := 0;
      Object.Hitgroup := 0;

      Engine.Objects.Append (Object);
--      if Last /= 0 and pe->objects) then
--              PIG_object *lo = pe->objects;
--              while(lo->next)
--                      lo = lo->next;
--              end loop;
--              po->prev = lo;
--              po->next = NULL;
--              lo->next = po;
--      else
--              po->prev = NULL;
--              po->next = pe->objects;
--              if(po->next)
--                po->next->prev = po;
--              end if;
--              pe->objects = po;
--      end if;

      Object.X  := Float (X);
      Object.Y  := Float (Y);
      Object.Ax := Float (X);
      Object.Ay := Float (Y);

      return Object;
   end Pig_Object_Open;


   procedure Close_Object (Object : in out PIG_Object)
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


   procedure Pig_Object_Close (Object : in out PIG_Object) is
   begin
      if Object.Id = 0 then
         null;
--         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
--                               "Object %p closed more than once!"); -- , po);
      end if;
      Object.Id := 0;     --  Mark for eventual removal and destruction
   end Pig_Object_Close;


   procedure Pig_Object_Close_All (Engine : in out PIG_Engine) is
   begin
--        while not Engine.Objects.Is_Empty loop
--           Close_Object (Engine.Objects);
--        end loop;
      null;
   end Pig_Object_Close_All;


   function Pig_Object_Find (Start : in out PIG_Object;
                             Id    :        Integer) return PIG_Object_Access
   is
      pragma Unreferenced (Start, Id);
--  PIG_object *pig_object_find(PIG_object *start, int id)
--      Pob, Pof : PIG_Object_Access;
   begin
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
      return null;
   end Pig_Object_Find;


end Engines;
