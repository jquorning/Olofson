--------------------------------------------------------------
--        Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;
with Ada.Text_IO;

with Interfaces;

with SDL.Video.Surfaces;
with SDL.Video.Rectangles;
with SDL.Video.Pixel_Formats;
with SDL.Video.Palettes;
with SDL.Video.Windows.Makers;
with SDL.Events.Events;
with SDL.Events.Mice;
with SDL.Events.Keyboards;

with Engines;
with Handlers;
with Signals;

procedure Pig is
   use Handlers;

   ----------------------------------------------------------
   --        Init, load stuff etc
   ----------------------------------------------------------

   procedure Init_All (Game   :    out Game_State_Access;
                       Screen : in out SDL.Video.Surfaces.Surface);
   procedure Dashboard (Game : in out Game_State);
   --  Render the dashboard

   procedure Start_Game (Game   : in out Game_State;
                         Result :    out Integer);
   procedure Handle_Input (Game  : in out Game_State;
                           Event : in out SDL.Events.Events.Events);
   procedure Handle_Keys (Game : in out Game_State);

   procedure Init_All (Game   :    out Game_State_Access;
                       Screen : in out SDL.Video.Surfaces.Surface)
--                            Screen : in out SDL.Video.Windows.Window)
   is
      use type Engines.PIG_Map;
      Clean_Game : constant Game_State := (Engine => null,
                                           Keys   => (others => False),
                                           Nice   => False,
                                           Refresh_Screen => 0,
                                           Jump => 0,
                                           Running => False,
                                           Lives_Wobble => 0.0,
                                           Lives_Wobble_Time => 0.0,
                                           Score_Wobble => 0.0,
                                           Score_Wobble_Time => 0.0,
                                           Dashboard_Time => 0.0,
                                           Player => null,
                                           Start_Time => Ada.Real_Time.Time_First,
                                           others => 0);

      Map              : Engines.PIG_Map_Access;
      Map_Tiles_Result : Integer;
   begin
      Game := new Game_State'(Clean_Game); --  Game_State;
      Game.Nice := True; --  JQ
      --      Game.Pe := new Engine.PIG_Engine;
--      Game.Engine.Nsprites := 0;
      --  if(!gs)
      --         return NULL;

      Game.Start_Time := Ada.Real_Time.Clock;
      Game.Running    := True;

      begin
         Engines.Pig_Open (Game.Engine, Screen);
      exception
         when others =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Could not open the Pig Engine!");
            raise;
            --           Freev (gs);
            --           return NULL;
            return;
      end;
      Game.Engine.Userdata := Handlers.From_Game_State (Game);

      Engines.Pig_Viewport (Game.Engine.all, 0, 0, SCREEN_W, MAP_H * TILE_H);

      Engines.Pig_Sprites (Game.Engine.all, "lifepig.png",    0,  0, Game.Lifepig);
      Engines.Pig_Sprites (Game.Engine.all, "font.png",      44, 56, Game.Scorefont);
      Engines.Pig_Sprites (Game.Engine.all, "glassfont.png", 60, 60, Game.Glassfont);
      Engines.Pig_Sprites (Game.Engine.all, "icons.png",     48, 48, Game.Icons);
      Engines.Pig_Sprites (Game.Engine.all, "stars.png",     32, 32, Game.Stars);
      Engines.Pig_Sprites (Game.Engine.all, "pigframes.png", 64, 48, Game.Pigframes);
      Engines.Pig_Sprites (Game.Engine.all, "evil.png",      48, 48, Game.Evil);
      Engines.Pig_Sprites (Game.Engine.all, "slime.png",     48, 48, Game.Slime);

      for I in Game.Icons .. Game.Icons + 3 * 8 - 1 loop
         Engines.Pig_Hotspot (Game.Engine.all, I, Engines.PIG_CENTER, 45);
      end loop;

      for I in Game.Pigframes .. Game.Pigframes + 12 - 1 loop
         Engines.Pig_Hotspot (Game.Engine.all, I, Engines.PIG_CENTER, 43);
      end loop;

      for I in Game.Evil .. Game.Evil + 16 - 1 loop
         Engines.Pig_Hotspot (Game.Engine.all, I, Engines.PIG_CENTER, 46);
      end loop;

      for I in Game.Slime .. Game.Slime + 16 - 1 loop
         Engines.Pig_Hotspot (Game.Engine.all, I, Engines.PIG_CENTER, 46);
      end loop;

      begin
         Engines.Pig_Map_Open (Map, Game.Engine.all, MAP_W, MAP_H);
      exception
         when others => --  if Pm = null then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Could not create map!");
            Engines.Pig_Close (Game.Engine.all);
            --  Free (gs);
            --  return NULL;
            return;
      end; -- if;

      Engines.Pig_Map_Tiles (Map.all, "tiles.png", TILE_W, TILE_H, Map_Tiles_Result);
      if Map_Tiles_Result < 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Could not load background graphics!");
         Engines.Pig_Close (Game.Engine.all);
         --  Free (gs);
         raise Storage_Error with "Could not load background graphics.";
      end if;

      --  Mark tiles for collision detection
      Engines.Pig_Map_Collisions (Map.all,  0, 12, Engines.PIG_All);   --  Red, green, yellov
      Engines.Pig_Map_Collisions (Map.all, 12, 17, Engines.PIG_None);  --  Sky
      Engines.Pig_Map_Collisions (Map.all, 29,  3, Engines.PIG_All);   --  Single R, G, Y

      Load_Level (Game.all, 0);
   end Init_All;


   ----------------------------------------------------------
   --        Render the dashboard
   ----------------------------------------------------------

   procedure Dashboard (Game : in out Game_State)
   is
      use Ada.Numerics.Elementary_Functions;
      use Ada.Real_Time;
      Pi : constant := Ada.Numerics.Pi;
      R  : SDL.Video.Rectangles.Rectangle;
      V  : Integer;
      X  : Float;
      --   T : Float := SDL_GetTicks * 0.001;
      Now : constant Time := Clock; --  Float := SDL_GetTicks * 0.001;
      T   : constant Float := Float (To_Duration (Now - Game.Start_Time));
   begin
      R.X      := 0;
      R.Y      := SDL.C.int (SCREEN_H - 56);
      R.Width  := SCREEN_W;
      R.Height := 56;
      SDL.Video.Surfaces.Set_Clip_Rectangle (Game.Engine.Surface, R);

      --  Render "plasma bar"
      for I in 0 .. 56 - 1 loop
         declare
            use SDL.Video.Palettes;
            F1, F2, M : Float;
            Cr    : SDL.Video.Rectangles.Rectangle;
            Pixel : Interfaces.Unsigned_32;
         begin
            Cr.X      := 0;
            Cr.Width  := SCREEN_W;
            Cr.Y      := SDL.C.int (SCREEN_H - 56 + I);
            Cr.Height := 1;
            F1 := 0.25 + 0.25 * Sin (T * 1.7 + Float (I) / Float (SCREEN_H * 42));
            F1 := F1 + 0.25 + 0.25 * Sin (-T * 2.1 + Float (I) / Float (SCREEN_H * 66));
            F2 := 0.25 + 0.25 * Sin (T * 3.31 + Float (I) / Float (SCREEN_H * 90));
            F2 := F2 + 0.25 + 0.25 * Sin (-T * 1.1 + Float (I) / Float (SCREEN_H * 154));
            M := Sin (Float (I) * Pi / 56.0);
            M := Sin (M * Pi * 0.5);
            M := Sin (M * Pi * 0.5);
            Pixel := SDL.Video.Pixel_Formats.To_Pixel
              (Format => Game.Engine.Surface.Pixel_Format,
               Red    => (Colour_Component ((128.0 * F1 + 64.0) * M)),
               Green  => (Colour_Component ((64.0 * F1 * F2 + 64.0) * M)),
               Blue   => (Colour_Component ((128.0 * F2 + 32.0) * M)));
            SDL.Video.Surfaces.Fill (Game.Engine.Surface, Cr, Pixel);
         end;
      end loop;

      --  Draw pigs... uh, lives!
      X := -10.0;
      for I in 0 .. Game.Lives - 1 loop
         X := X + 48.0 + Game.Lives_Wobble *
           Sin (Float (Game.Lives_Wobble_Time) * 12.0) * 0.2;
         Engines.Pig_Draw_Sprite
           (Game.Engine.all, Game.Lifepig,
            Integer (X) + Integer (Game.Lives_Wobble *
                                     Sin (Float (Game.Lives_Wobble_Time) * 20.0
                                            + Float (I) * 1.7)),
            SCREEN_H - 56 / 2);
      end loop;

      --  Print score
      X := Float (SCREEN_W + 5);
      V := Game.Score;
      for I in reverse 0 .. 9 loop
         declare
            N : constant Integer := V mod 10;
         begin
            X := X - 39.0 - Game.Score_Wobble *
              Sin (Float (Game.Score_Wobble_Time) * 15.0 + Float (I) * 0.5);
            Engines.Pig_Draw_Sprite (Game.Engine.all, Game.Scorefont + N,
                                     Integer (X),
                                     SCREEN_H - 56 / 2);
            V := V / 10;
            exit when V = 0;
         end;
      end loop;

      Engines.Pig_Dirty (Game.Engine.all, R);
   end Dashboard;


   ----------------------------------------------------------
   --        Game logic event handlers
   ----------------------------------------------------------

   procedure Start_Game (Game   : in out Game_State;
                         Result :    out Integer)
   is
   begin
      if 0 /= Game.Level then
         Result := 0;       -- Already playing! -->
         return;
      end if;

      Game.Score := 0;
      Game.Lives := 5;

      begin
         Load_Level (Game, 1);
      exception
         when others => --  if Load_Result < 0 then
            Result := -1;
            return;
      end; --  if;

      begin
         New_Player (Game, Game.Player);
      exception
         when others =>
            --  if Gs.Player = null then
            Result := -1;
            return;
      end; --  if;

      Result := 0;
   end Start_Game;


   ----------------------------------------------------------
   --        Input; events and game control keys
   ----------------------------------------------------------

   procedure Handle_Input (Game  : in out Game_State;
                           Event : in out SDL.Events.Events.Events)
   is
      use type SDL.Events.Event_Types;
   begin
      --   case Ev.Type_C is
      case Event.Common.Event_Type is

         when SDL.Events.Mice.Button_Up =>
            --  SDL_MOUSEBUTTONUP =>
            null;

         when SDL.Events.Keyboards.Key_Down =>
            --  SDL_KEYDOWN =>
            --         case Ev.key.keysym.Sym is
            case Event.Keyboard.Key_Sym.Key_Code is

            --            when SDLK_UP =>
               when SDL.Events.Keyboards.Code_Up =>
                  Game.Jump := 3;

                  --            when SDLK_F1 =>
               when SDL.Events.Keyboards.Code_F1 =>
                  Game.Engine.Interpolation := not Game.Engine.Interpolation;
                  if Game.Engine.Interpolation then
                     Message (Game, "Interpolation: ON");
                  else
                     Message (Game, "Interpolation: OFF");
                  end if;

                  --            when SDLK_F2 =>
               when SDL.Events.Keyboards.Code_F2 =>
                  Game.Engine.Direct := not Game.Engine.Direct;
                  if Game.Engine.Direct then
                     Message (Game, "Rendering: Direct");
                  else
                     Message (Game, "Rendering: Buffered");
                  end if;

                  --            when SDLK_F3 =>
               when SDL.Events.Keyboards.Code_F3 =>
                  Game.Engine.Show_Dirtyrects := not Game.Engine.Show_Dirtyrects;
                  if Game.Engine.Show_Dirtyrects then
                     Message (Game, "Dirtyrects: ON");
                  else
                     Message (Game, "Dirtyrects: OFF");
                  end if;

                  --            when SDLK_F4 =>
               when SDL.Events.Keyboards.Code_F4 =>
                  Game.Nice := not Game.Nice;
                  if Game.Nice then
                     Message (Game, "Be Nice: ON");
                  else
                     Message (Game, "Be Nice: OFF");
                  end if;

                  --            when SDLK_SPACE =>
               when SDL.Events.Keyboards.Code_Space =>
                  declare
                     Result : Integer;
                  begin
                     Start_Game (Game, Result);
                  end;

               when others =>
                  null;
            end case;

         when SDL.Events.Keyboards.Key_Up => --  SDL_KEYUP =>
                                             --         case Ev.key.keysym.Sym is
            case Event.Keyboard.Key_Sym.Key_Code is

            --            when SDLK_ESCAPE =>
               when SDL.Events.Keyboards.Code_Escape =>
                  Game.Running := False;
               when others =>
                  null;
            end case;

         when SDL.Events.Quit => -- SDL_QUIT =>
            Game.Running := False;

         when others =>
            null;
      end case;
   end Handle_Input;


   procedure Handle_Keys (Game : in out Game_State)
   is
   begin
      null;
   end Handle_Keys;


   ----------------------------------------------------------
   --        Main
   ----------------------------------------------------------
   use Ada.Real_Time;
   Window     : SDL.Video.Windows.Window;
   Screen     : SDL.Video.Surfaces.Surface;
   Game       : Game_State_Access;
   I          : Integer;
   BPP        : Integer := 0;
   Last_Tick  : Ada.Real_Time.Time;
   Start_Time : Ada.Real_Time.Time;
   End_Time   : Ada.Real_Time.Time;
   Dashframe  : Integer;
   logic_fps  : constant Float := 20.0;
--   flags      : Integer := SDL_DOUBLEBUF + SDL_HWSURFACE; -- |
begin
   if not SDL.Initialise (SDL.Enable_Everything) then
      null;
   end if;

--          for(i = 1; i < argc; ++i)
--          {
--                  if(strncmp(argv[i], "-s", 2) == 0)
--                          flags &= ~SDL_DOUBLEBUF;
--                  else if(strncmp(argv[i], "-f", 2) == 0)
--                          flags |= SDL_FULLSCREEN;
--                  else
--                          bpp = atoi(&argv[i][1]);
--          }

   begin
      SDL.Video.Windows.Makers.Create (Window, Width => SCREEN_W, Height => SCREEN_H,
                                       X => 10, Y => 10, Title => "Pig"); -- , bpp, Flags);
--      Screen := SDL_SetVideoMode (SCREEN_W, SCREEN_H, bpp, flags);
      Screen := Window.Get_Surface;
   exception
      when others => --  if Screen = null then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Failed to open screen!");
         return; -- 1;
   end; -- if;

--   SDL_WM_SetCaption ("Fixed Rate Pig", "Pig");
--   SDL_ShowCursor (0);

   begin
      Init_All (Game, Screen);
--     exception
--        when others => --  if not Gs then
--           Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
--                                 "Init_All failed");
--           raise;
--           return; -- 1;
   end; --  if;

--   Game.Keys := SDL_GetKeyState (I);

   Game.Logic_Frames    := 0;
   Game.Rendered_Frames := 0;
   Game.Engine.Before_Objects := Handlers.Before_Objects'Access;

   Engines.Pig_Start (Game.Engine.all, 0);
   Game.Refresh_Screen := Game.Engine.Pages;
   Start_Time := Ada.Real_Time.Clock; --  SDL_GetTicks;
   Last_Tick  := Start_Time;

   while Game.Running loop
      declare
         Tick2   : Ada.Real_Time.Time; -- Integer;
         Frames  : Float;
         Dt      : Duration; -- Ada.Real_Time.Time_Span; --  Duration;
         Ev      : SDL.Events.Events.Events;
      begin
         --  Handle input
         while SDL.Events.Events.Poll (Ev) loop
            Handle_Input (Game.all, Ev);
         end loop;
         Handle_Keys (Game.all);
         if not Signals.Process_Control.Is_Running then
            Game.Running := False;
         end if;

         --  Calculate time since last update
         Tick2   := Ada.Real_Time.Clock;
         --  SDL_GetTicks;
         --  Dt     := Float (tick - last_tick) * 0.001;
         Dt     := Ada.Real_Time.To_Duration (Tick2 - Last_Tick);
         Frames := Float (Dt) * logic_fps;

         --  Run the game logic
         Engines.Pig_Animate (Game.Engine.all, Frames);

         --  Limit the dashboard frame rate to 15 fps
         --  when there's no wobbling going on.
         --
         --  The 'dashframe' deal is about keeping the
         --  pages in sync on a double buffered display.
         if
           Game.Lives_Wobble /= 0.0 or
           Game.Score_Wobble /= 0.0 or
           (Game.Dashboard_Time > Duration (1.0 / 15.0))
         then
            Dashframe := Game.Engine.Pages;
            Game.Dashboard_Time := 0.0;
         end if;
         if Dashframe /= 0 then
            Dashframe := Dashframe - 1;
            Dashboard (Game.all);
         end if;

         --  Update sprites
         if Game.Refresh_Screen /= 0 then
            Game.Refresh_Screen := Game.Refresh_Screen - 1;
            Engines.Pig_Refresh_All (Game.Engine.all);
         else
            Engines.Pig_Refresh (Game.Engine.all);
         end if;

         --  Make the new frame visible
         Engines.Pig_Flip (Game.Engine.all);
         Window.Update_Surface; --  JQ

         --  Update statistics, timers and stuff
         Game.Rendered_Frames   := Game.Rendered_Frames + 1;
         Game.Lives_Wobble_Time := Game.Lives_Wobble_Time + Dt;
         Game.Score_Wobble_Time := Game.Score_Wobble_Time + Dt;
         Game.Dashboard_Time    := Game.Dashboard_Time + Dt;

         Last_Tick := Tick2;
         if Game.Nice then
            delay 0.010;
         end if;
      end;
   end loop;

   SDL.Finalise;

   --  Print some statistics
   End_Time := Ada.Real_Time.Clock; -- SDL_GetTicks;
   I := Integer (Ada.Real_Time.To_Duration (Ada.Real_Time.Time_Span'(End_Time - Start_Time)));
   Ada.Text_IO.Put_Line ("          Total time running: " & I'Image & " ms");
   if I = 0 then
      I := 1;
   end if;
   Ada.Text_IO.Put_Line ("Average rendering frame rate: " &
                           Float (Game.Rendered_Frames * 1000 / I)'Image & " fps");
   Ada.Text_IO.Put_Line ("    Average logic frame rate: " &
                           Float (Game.Logic_Frames * 1000 / I)'Image & " fps");
   Engines.Pig_Close (Game.Engine.all);
end Pig;
