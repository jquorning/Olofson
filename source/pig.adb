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

with Ada.Real_Time;
with Ada.Command_Line;
with Ada.Numerics.Elementary_Functions;
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
with Games;
with Signals;

procedure Pig is
   use Games;

   ----------------------------------------------------------
   --        Init, load stuff etc
   ----------------------------------------------------------

--   function Create_Game (Screen : in SDL.Video.Surfaces.Surface)
--                        return Game_Access;
--   procedure Init_All (Game   :    out not null Game_State_Access;
--                       Screen : in out SDL.Video.Surfaces.Surface);

   procedure Dashboard (Game : in out Game_State);
   --  Render the dashboard

   procedure Start_Game (Game   : in out Game_State;
                         Result :    out Integer);
   procedure Handle_Input (Game  : in out Game_State;
                           Event : in out SDL.Events.Events.Events);
   procedure Handle_Keys (Game : in out Game_State);

   ----------------------------------------------------------
   --        Render the dashboard
   ----------------------------------------------------------

   procedure Dashboard (Game : in out Game_State)
   is
      use Ada.Numerics.Elementary_Functions;
      use Ada.Real_Time;
      Pi   : constant       := Ada.Numerics.Pi;
      Now  : constant Time  := Clock;
      T    : constant Float := Float (To_Duration (Now - Game.Start_Time));

      Clip : constant SDL.Video.Rectangles.Rectangle :=
        (X      => 0,
         Y      => SDL.C.int (SCREEN_H - 56),
         Width  => SCREEN_W,
         Height => 56);
   begin
      Game.Surface.Set_Clip_Rectangle (Clip);

      --  Render "plasma bar"
      for I in 0 .. 56 - 1 loop
         declare
            use SDL.Video.Palettes;

            Line : constant SDL.Video.Rectangles.Rectangle :=
              (X      => 0,
               Width  => SCREEN_W,
               Y      => SDL.C.int (I + SCREEN_H - 56),
               Height => 1);

            F1 : constant Float :=
              0.25 + 0.25 * Sin (T  * 1.7  + Float (I) / Float (SCREEN_H * 42))
              + 0.25 + 0.25 * Sin (-T * 2.1  + Float (I) / Float (SCREEN_H * 66));

            F2 : constant Float :=
              0.25 + 0.25 * Sin (T  * 3.31 + Float (I) / Float (SCREEN_H * 90))
              + 0.25 + 0.25 * Sin (-T * 1.1  + Float (I) / Float (SCREEN_H * 154));

            M_1 : constant Float := Sin (Float (I) * Pi / 56.0);
            M_2 : constant Float := Sin (M_1 * Pi * 0.5);
            M   : constant Float := Sin (M_2 * Pi * 0.5);

            Pixel : constant Interfaces.Unsigned_32 :=
              SDL.Video.Pixel_Formats.To_Pixel
              (Format => Game.Surface.Pixel_Format,
               Red    => (Colour_Component ((128.0 * F1      + 64.0) * M)),
               Green  => (Colour_Component ((64.0  * F1 * F2 + 64.0) * M)),
               Blue   => (Colour_Component ((128.0 * F2      + 32.0) * M)));
         begin
            Game.Surface.Fill (Line, Pixel);
         end;
      end loop;

      --  Draw pigs... uh, lives!
      declare
         X : Float := -10.0;
      begin
         for I in 0 .. Game.Lives - 1 loop
            X := X + 48.0 + Game.Lives_Wobble *
              Sin (Float (Game.Lives_Wobble_Time) * 12.0) * 0.2;
            Game.Pig_Draw_Sprite
              (Game.Lifepig,
               Integer (X) + Integer (Game.Lives_Wobble *
                                        Sin (Float (Game.Lives_Wobble_Time) * 20.0
                                               + Float (I) * 1.7)),
               SCREEN_H - 56 / 2);
         end loop;
      end;

      --  Print score
      declare
         use type Engines.Sprite_Counts;
         X : Float   := Float (SCREEN_W + 5);
         V : Integer := Game.Score;
         N : Engines.Sprite_Counts;
      begin
         for I in reverse 0 .. 9 loop
            N := Engines.Sprite_Counts (V mod 10);
            X := X - 39.0 - Game.Score_Wobble *
              Sin (Float (Game.Score_Wobble_Time) * 15.0 + Float (I) * 0.5);
            Game.Pig_Draw_Sprite (Game.Scorefont + N,
                                  Integer (X),
                                  SCREEN_H - 56 / 2);
            V := V / 10;
            exit when V = 0;
         end loop;
      end;

      Game.Pig_Dirty (Clip);
   end Dashboard;


   ----------------------------------------------------------
   --        Game logic event handlers
   ----------------------------------------------------------

   procedure Start_Game (Game   : in out Game_State;
                         Result :    out Integer)
   is
      Player : Engines.PIG_Object_Access;
   begin
      if Game.Level /= 0 then
         Result := 0;       -- Already playing! -->
         return;
      end if;

      Game.Score := 0;
      Game.Lives := 5;

      Load_Level (Game, 1);

      New_Player (Game, Player);
      Game.Player := Player;

      Result := 0;
   end Start_Game;


   ----------------------------------------------------------
   --        Input; events and game control keys
   ----------------------------------------------------------

   procedure Handle_Input (Game  : in out Game_State;
                           Event : in out SDL.Events.Events.Events)
   is
      use SDL.Events;
   begin
      case Event.Common.Event_Type is

         when Mice.Button_Up =>
            null;

         when Keyboards.Key_Down =>

            case Event.Keyboard.Key_Sym.Scan_Code is

               when Keyboards.Scan_Code_Up =>
                  Game.Jump := 3;

               when Keyboards.Scan_Code_F1 =>
                  Game.Interpolation := not Game.Interpolation;
                  if Game.Interpolation then
                     Message (Game, "Interpolation: ON");
                  else
                     Message (Game, "Interpolation: OFF");
                  end if;

               when Keyboards.Scan_Code_F2 =>
                  Game.Direct := not Game.Direct;
                  if Game.Direct then
                     Message (Game, "Rendering: Direct");
                  else
                     Message (Game, "Rendering: Buffered");
                  end if;

               when Keyboards.Scan_Code_F3 =>
                  Game.Show_Dirtyrects := not Game.Show_Dirtyrects;
                  if Game.Show_Dirtyrects then
                     Message (Game, "Dirtyrects: ON");
                  else
                     Message (Game, "Dirtyrects: OFF");
                  end if;

               when Keyboards.Scan_Code_F4 =>
                  Game.Nice := not Game.Nice;
                  if Game.Nice then
                     Message (Game, "Be Nice: ON");
                  else
                     Message (Game, "Be Nice: OFF");
                  end if;

               when Keyboards.Scan_Code_Space =>
                  declare
                     Result : Integer;
                  begin
                     Start_Game (Game, Result);
                  end;

               when others =>
                  null;
            end case;

         when Keyboards.Key_Up =>

            case Event.Keyboard.Key_Sym.Key_Code is
               when Keyboards.Code_Escape =>
                  Game.Running := False;
               when others =>
                  null;
            end case;

         when Quit =>
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
   procedure Play_Game;

   procedure Play_Game
   is
      use Ada.Real_Time;
      Window     : SDL.Video.Windows.Window;
      Screen     : SDL.Video.Surfaces.Surface;
      Last_Tick  : Ada.Real_Time.Time;
      Start_Time : Ada.Real_Time.Time;
      Dashframe  : Integer;
      Logic_FPS  : constant Float := 20.0;
      --   flags      : Integer := SDL_DOUBLEBUF + SDL_HWSURFACE; -- |
      use type SDL.Init_Flags;
   begin
      if not SDL.Initialise (SDL.Enable_Screen or SDL.Enable_Events) then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Could not initialise SDL library.");
         return;
      end if;

      begin
         SDL.Video.Windows.Makers.Create (Window, Width => SCREEN_W, Height => SCREEN_H,
                                          X => 10, Y => 10, Title => "Pig");
         --  bpp, Flags);
         --  Screen := SDL_SetVideoMode (SCREEN_W, SCREEN_H, bpp, flags);
         Screen := Window.Get_Surface;
      exception
         when others => --  if Screen = null then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Failed to open screen!");
            return; -- 1;
      end; -- if;

      --   SDL_WM_SetCaption ("Fixed Rate Pig", "Pig");
      --   SDL_ShowCursor (0);

      declare
--         Game_Ptr : constant Game_Access := Create_Game (Screen); -- Class; -- Game_State_Access;
         Game : aliased Game_State; --  renames Game_Ptr.all;
      begin
         Game.Setup (Engines.PIG_Engine (Game)'Unchecked_Access, Screen, Pages => 1);
         Game.Create;
--         Init_All (Game, Screen);
         --     exception
         --        when others => --  if not Gs then
         --           Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         --                                 "Init_All failed");
         --           raise;
         --           return; -- 1;
--      end; --  if;

      --   Game.Keys := SDL_GetKeyState (I);

         Game.Logic_Frames    := 0;
         Game.Rendered_Frames := 0;

         Game.Pig_Start (0);
         Game.Refresh_Screen := Game.Pages;
         Start_Time := Ada.Real_Time.Clock;
         Last_Tick  := Start_Time;

         while Game.Running loop
            declare
               Tick2   : Ada.Real_Time.Time;
               Frames  : Float;
               Dt      : Duration;
               Event   : SDL.Events.Events.Events;
            begin
               --  Handle input
               while SDL.Events.Events.Poll (Event) loop
                  Handle_Input (Game, Event);
               end loop;

               Handle_Keys (Game);

               if not Signals.Process_Control.Is_Running then
                  Game.Running := False;
               end if;

               --  Calculate time since last update
               Tick2   := Ada.Real_Time.Clock;
               --  Dt     := Float (tick - last_tick) * 0.001;
               Dt     := Ada.Real_Time.To_Duration (Tick2 - Last_Tick);
               Frames := Float (Dt) * Logic_FPS;

               --  Run the game logic
               Game.Pig_Animate (Frames);

               --  Limit the dashboard frame rate to 15 fps
               --  when there's no wobbling going on.
               --
               --  The 'dashframe' deal is about keeping the
               --  pages in sync on a double buffered display.
               pragma Warnings (Off, "* not a multiple of Small");
               if
                 Game.Lives_Wobble /= 0.0 or
                 Game.Score_Wobble /= 0.0 or
                 Game.Dashboard_Time > Duration (1.0 / 15.0)
               then
                  Dashframe := Game.Pages;
                  Game.Dashboard_Time := 0.0;
               end if;
               pragma Warnings (On, "* not a multiple of Small");

               if Dashframe /= 0 then
                  Dashframe := Dashframe - 1;
                  Dashboard (Game);
               end if;

               --  Update sprites
               if Game.Refresh_Screen /= 0 then
                  Game.Refresh_Screen := Game.Refresh_Screen - 1;
                  Game.Pig_Refresh_All;
               else
                  Game.Pig_Refresh;
               end if;

               --  Make the new frame visible
               Game.Pig_Flip (Window);

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
         Print_Some_Statistics :
         declare
            use Ada.Text_IO;
            End_Time      : constant Time      := Ada.Real_Time.Clock;
            Game_Span     : constant Time_Span := End_Time - Start_Time;
            Game_Duration : constant Duration  := To_Duration (Game_Span);
            Duration_MS   : constant Integer   := 1000 * Integer (Game_Duration);
            Denominator   : constant Integer   := Integer'Max (0, Duration_MS);
            Rendered_FPS  : constant Float := Float (Game.Rendered_Frames * 1000 / Denominator);
            Logical_FPS   : constant Float := Float (Game.Logic_Frames    * 1000 / Denominator);
         begin
            Put_Line ("          Total time running: " & Duration_MS'Image & " ms");
            Put_Line ("Average rendering frame rate: " & Rendered_FPS'Image & " fps");
            Put_Line ("    Average logic frame rate: " & Logical_FPS'Image & " fps");
         end Print_Some_Statistics;
      end;
   end Play_Game;

   BPP           : Integer := 0;
   Double_Buffer : Boolean := False;
   Full_Screen   : Boolean := False;
   pragma Unreferenced (BPP, Double_Buffer, Full_Screen);
begin

   Process_Command_Line :
   for Index in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Argument : String renames Ada.Command_Line.Argument (Index);
      begin
         if    Argument = "-s" then  Double_Buffer := False;
         elsif Argument = "-f" then  Full_Screen   := True;
         else                        BPP := Integer'Value (Argument);
         end if;
      end;
   end loop Process_Command_Line;

   Play_Game;

end Pig;
