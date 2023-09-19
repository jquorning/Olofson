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
with Ada.Numerics.Elementary_Functions;
with Ada.Characters.Handling;

with Interfaces;

with SDL.Video.Surfaces;
with SDL.Video.Rectangles;
with SDL.Video.Pixel_Formats;
with SDL.Video.Palettes;
with SDL.Video.Windows.Makers;
with SDL.Events.Mice;
with SDL.Events.Keyboards;

with Signals;

package body Games is

   subtype Sprite_Counts  is Engines.Sprite_Counts;
   subtype Pig_Map_Access is Engines.Pig_Map_Access;
   subtype Pig_Events     is Engines.Pig_Events;
   subtype Sides          is Engines.Sides;

   function Open_Object (Engine : in out Engines.Game_Engine;
                         X, Y   :        Pixels;
                         Last   :        Boolean)
                        return not null Object_Access
   renames Engines.Open_Object;

   procedure Unlink_Object (Object : in out Game_Object)
     renames Engines.Unlink_Object;

   use type Engines.Pixels;
   use type Sprite_Counts;

   Center   : constant Engines.Magic_Value := Engines.Center;
   Top_Side : constant Engines.Sides       := Engines.Top_Side;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Game : in out Game_State)
   is
   begin
      Engines.Initialize (Game_Engine (Game));

      Game.Keys              := (others => False);
      Game.Nice              := True;
      Game.Refresh_Screen    := 0;
      Game.Jump              := 0;

      Game.Lifepig   := 1;      Game.Scorefont := 1;
      Game.Glassfont := 1;      Game.Icons     := 1;
      Game.Stars     := 1;      Game.Pigframes := 1;
      Game.Evil      := 1;      Game.Slime     := 1;

      Game.Running           := True;
      Game.Level             := 0;

      Game.Lives             := 0;
      Game.Lives_Wobble      := 0.0;
      Game.Lives_Wobble_Time := 0.0;

      Game.Score             := 0;
      Game.Score_Wobble      := 0.0;
      Game.Score_Wobble_Time := 0.0;

      Game.Dashboard_Time    := 0.0;
      Game.Fun_Count         := 0;
      Game.Enemycount        := 0;
      Game.Messages          := 0;
      Game.Player            := null;

      Game.Logic_Frames      := 0;
      Game.Rendered_Frames   := 0;
      Game.Start_Time        := Ada.Real_Time.Clock;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Game : in out Game_State)
   is
   begin
      Engines.Finalize (Game_Engine (Game));
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create return Game_State is
   begin
      return Game : Game_State
      do
         Game.Set_Viewport (0, 0, SCREEN_W, MAP_H * TILE_H);

         Game.Create_Sprites (Asset_Dir & "lifepig.png",    0,  0, Game.Lifepig);
         Game.Create_Sprites (Asset_Dir & "font.png",      44, 56, Game.Scorefont);
         Game.Create_Sprites (Asset_Dir & "glassfont.png", 60, 60, Game.Glassfont);
         Game.Create_Sprites (Asset_Dir & "icons.png",     48, 48, Game.Icons);
         Game.Create_Sprites (Asset_Dir & "stars.png",     32, 32, Game.Stars);
         Game.Create_Sprites (Asset_Dir & "pigframes.png", 64, 48, Game.Pigframes);
         Game.Create_Sprites (Asset_Dir & "evil.png",      48, 48, Game.Evil);
         Game.Create_Sprites (Asset_Dir & "slime.png",     48, 48, Game.Slime);

         declare
            subtype Icons_Range is Sprite_Counts range 0 .. 3 * 8 - 1;
            subtype Pig_Range   is Sprite_Counts range 0 .. 12 - 1;
            subtype Evil_Range  is Sprite_Counts range 0 .. 16 - 1;
            subtype Slime_Range is Sprite_Counts range 0 .. 16 - 1;
         begin
            for I in Icons_Range loop
               Game.Set_Hotspot (Game.Icons + I, Center, 45);
            end loop;

            for I in Pig_Range loop
               Game.Set_Hotspot (Game.Pigframes + I, Center, 43);
            end loop;

            for I in Evil_Range loop
               Game.Set_Hotspot (Game.Evil + I, Center, 46);
            end loop;

            for I in Slime_Range loop
               Game.Set_Hotspot (Game.Slime + I, Center, 46);
            end loop;
         end;

         declare
            use Engines;
            Map_Tiles_Result : Integer;
            Map : constant Pig_Map_Access :=
              Pig_Map_Open (Game.Self, MAP_W, MAP_H);
         begin
            Pig_Map_Tiles (Map.all, Asset_Dir & "tiles.png",
                           TILE_W, TILE_H, Map_Tiles_Result);
            if Map_Tiles_Result < 0 then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "Could not load background graphics!");
               --  Game.Pig_Close; -- (Game.Engine.all);
               --  Free (gs);
               raise Storage_Error with "Could not load background graphics.";
            end if;

            --  Mark tiles for collision detection
            Pig_Map_Collisions (Map.all,  0, 12, All_Sides);   --  Red, green, yellov
            Pig_Map_Collisions (Map.all, 12, 17, No_Side);  --  Sky
            Pig_Map_Collisions (Map.all, 29,  3, All_Sides);   --  Single R, G, Y

            Game.Load_Level (0);
         end;
      end return;
   end Create;

   --------------
   -- Add_Life --
   --------------

   procedure Add_Life (Game : in out Game_State) is
   begin
      Game.Lives := Game.Lives + 1;
      Game.Lives_Wobble := Game.Lives_Wobble + 10.0;
      if Game.Lives_Wobble > 15.0 then
         Game.Lives_Wobble := 15.0;
      end if;
      Game.Lives_Wobble_Time := 0.0;
   end Add_Life;

   -----------------
   -- Remove_Life --
   -----------------

   procedure Remove_Life (Game : in out Game_State)
   is
   begin
      Game.Lives := Game.Lives - 1;
      Game.Lives_Wobble := Game.Lives_Wobble + 10.0;
      if Game.Lives_Wobble > 15.0 then
         Game.Lives_Wobble := 15.0;
      end if;
      Game.Lives_Wobble_Time := 0.0;
   end Remove_Life;

   ----------------
   -- New_Player --
   ----------------

   procedure New_Player (Game   : in out Game_State;
                         Object :    out Object_Access)
   is
   begin
      if Game.Lives = 0 then
         Object := null;
         return;
      end if;
      Object := Open_Object (Game_Engine (Game), SCREEN_W / 2, -50, Last => True);

      Remove_Life (Game);
      Object.I_Base   := Game.Pigframes;
      Object.Handler  := Player_Handler'Access;
      Object.Hit_Mask := GROUP_POWERUP + GROUP_ENEMY;
   end New_Player;

   ----------------
   -- New_Player --
   ----------------

   procedure New_Player (Game : in out Game_State) is
      Dummy : Object_Access;
   begin
      New_Player (Game, Dummy);
   end New_Player;

   -----------------
   -- New_Powerup --
   -----------------

   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   :        Pixels;
                          Speed  :        Integer;
                          Kind   :        Power_Ups;
                          Object :    out not null Object_Access)
   is
   begin
      Object := Open_Object (Game_Engine (Game.Self.all), X, Y, Last => True);

      Game.Enemycount  := Game.Enemycount + 1;
      Object.Score     := Power_Ups'Pos (Kind);
      Object.I_Base    := Game.Icons + Sprite_Counts (8 * Object.Score);
      Object.Target    := Speed;
      Object.Handler   := Powerup_Handler'Access;
      Object.Tile_Mask := Top_Side;
      Object.Hit_Group := GROUP_POWERUP;
   end New_Powerup;

   -----------------
   -- New_Powerup --
   -----------------

   procedure New_Powerup (Game  : in out Game_State;
                          X, Y  :        Pixels;
                          Speed :        Integer;
                          Kind  :        Power_Ups)
   is
      Dummy : Object_Access;
   begin
      New_Powerup (Game, X, Y, Speed, Kind, Dummy);
   end New_Powerup;

   --------------
   -- New_Star --
   --------------

   procedure New_Star (Game   : in out Game_State;
                       X, Y   :        Pixels;
                       Vx, Vy :        Pixels;
                       Object :    out not null Object_Access)
   is
   begin
      Object := Open_Object (Game, X + Vx, Y + Vy, Last => True);

      Object.I_Base  := Game.Stars;
      Object.Ax      := -0.3 * Float (Vx);
      Object.Vx      := Float (Vx * 3);
      Object.Ay      := -0.3 * Float (Vy);
      Object.Vy      := Float (Vy * 3);
      Object.Handler := Star_Handler'Access;
   end New_Star;

   ---------------
   -- New_Start --
   ---------------

   procedure New_Star (Game   : in out Game_State;
                       X, Y   :        Pixels;
                       Vx, Vy :        Pixels)
   is
      Dummy : Object_Access;
   begin
      New_Star (Game, X, Y, Vx, Vy, Dummy);
   end New_Star;

   --------------
   -- New_Evil --
   --------------

   procedure New_Evil (Game   : in out Game_State;
                       X, Y   :        Pixels;
                       Speed  :        Integer;
                       Object :    out not null Object_Access)
   is
   begin
      Object := Open_Object (Game,
                             X * TILE_W,
                             Y * TILE_H, Last => True);

      Game.Enemycount  := Game.Enemycount + 1;
      Object.I_Base    := Game.Evil;
      Object.Target    := Speed;
      Object.Handler   := Evil_Handler'Access;
      Object.Score     := 200;
      Object.Tile_Mask := Top_Side;
      Object.Hit_Group := GROUP_ENEMY;
   end New_Evil;

   ---------------
   -- New_Slime --
   ---------------

   procedure New_Slime (Game   : in out Game_State;
                        X, Y   :        Pixels;
                        Speed  :        Integer;
                        Object :    out not null Object_Access)
   is
   begin
      Object := Open_Object (Game,
                                 X * TILE_W, Y * TILE_H, Last => True);

      Game.Enemycount  := Game.Enemycount + 1;
      Object.I_Base    := Game.Slime;
      Object.Target    := Speed;
      Object.Handler   := Slime_Handler'Access;
      Object.Score     := 300;
      Object.Tile_Mask := Top_Side;
      Object.Hit_Group := GROUP_ENEMY;
   end New_Slime;

   --------------------
   -- New_Chain_Head --
   --------------------

   procedure New_Chain_Head (Game     : in out Game_State;
                             X, Y     :        Pixels;
                             Image    :        Sprite_Index;
                             Target_X :        Integer;
                             Object   :    out not null Object_Access)
   is
   begin
      Object := Open_Object (Game, X, Y, Last => True);

      Object.I_Base  := Image;
      Object.Handler := Chain_Head_Handler'Access;
      Object.Target  := Target_X;
   end New_Chain_Head;

   --------------------
   -- New_Chain_Link --
   --------------------

   procedure New_Chain_Link (Game   : in out Game_State;
                             X, Y   :        Pixels;
                             Image  :        Sprite_Index;
                             Target :        Object_Id;
                             Object :    out not null Object_Access)
   is
   begin
      Object := Open_Object (Game, X, Y, Last => True);

      Object.I_Base  := Image;
      Object.Handler := Chain_Link_Handler'Access;
      Object.Target  := Integer (Target);
   end New_Chain_Link;

   use all type Power_Ups;

   -----------------------
   -- Inc_Score_Nobonus --
   -----------------------

   procedure Inc_Score_Nobonus (Game : in out Game_State;
                                V    :        Integer)
   is
      Vc : Integer := V;
      Os : constant Integer := Game.Score;
   begin
      Game.Score := Game.Score + Vc;
      while Vc /= 0 loop
         Game.Score_Wobble := Game.Score_Wobble + 1.0;
         Vc := Vc / 10;
      end loop;
      if Game.Score_Wobble > 15.0 then
         Game.Score_Wobble := 15.0;
      end if;
      Game.Score_Wobble_Time := 0.0;
      if Os / 10000 /= Game.Score / 10000 then
         New_Powerup (Game, SCREEN_W / 2, -20, -4, Power_Life);
      end if;
   end Inc_Score_Nobonus;

   ---------------
   -- Inc_Score --
   ---------------

   procedure Inc_Score (Game : in out Game_State;
                        V    :        Integer)
   is
      Os : constant Integer := Game.Score;
   begin
      Inc_Score_Nobonus (Game, V);
      if Os / 5000 /= Game.Score / 5000 then
         New_Powerup (Game, SCREEN_W / 2, -20, 8, Power_Bonus_1);
      elsif Os / 1000 /= Game.Score / 1000 then
         New_Powerup (Game, SCREEN_W / 2, -20, -6, Power_Bonus_2);
      end if;
   end Inc_Score;

   -------------
   -- Message --
   -------------

   procedure Message (Game : in out Game_State;
                      Text :        String)
   is
      use Ada.Characters.Handling;

      function To_Frame (C : Character) return Sprite_Counts is
        (Game.Glassfont + Character'Pos (C) - Character'Pos (' '));

      X  : constant Pixels := SCREEN_W + FONT_SPACING;
      Y  : constant Pixels := MAP_H * TILE_H - 30;
      Tx : constant Integer := (SCREEN_W - (Text'Length - 1) * FONT_SPACING) / 2;
      First  : Boolean := True;
      Object : Object_Access := null;
   begin
      for C of To_Upper (Text) loop
         if First then
            First := False;
            New_Chain_Head (Game, X, Y, To_Frame (C), Tx, Object);
         else
            New_Chain_Link (Game, X, Y, To_Frame (C), Object.Id, Object);
         end if;
      end loop;
      Game.Messages := Game.Messages + 1;
   end Message;

   Preframe   : constant Pig_Events := Engines.Preframe;
   Timer_1    : constant Pig_Events := Engines.Timer_1;
   Timer_2    : constant Pig_Events := Engines.Timer_2;
   Timer_3    : constant Pig_Events := Engines.Timer_3;
   Hit_Tile   : constant Pig_Events := Engines.Hit_Tile;
   Hit_Object : constant Pig_Events := Engines.Hit_Object;
   Offscreen  : constant Pig_Events := Engines.Offscreen;
   Postframe  : constant Pig_Events := Engines.Postframe;

   No_Side : constant Sides := Engines.No_Side;

   use all type Engines.Object_States;
   use type Sides;
   use type Object_Access;

   use type SDL.C.int;

   --------------------
   -- Player_Handler --
   --------------------

   procedure Player_Handler (Object : in out Game_Object;
                             Event  :        Pig_Event)
   is
      Game : Game_State renames Game_Access (Object.Owner).all;
   begin
      case Event.Kind is

         when Preframe =>
            case Object.State is
               when Knocked | Dead | Next_Level => null;

               when Waiting =>
                  if 1 = Object.Age then
                     Message (Game, "Get ready!");
                  elsif Object.Age > 50 then
                     Object.State := Falling;
                  end if;

               when Walking =>
                  if Game.Keys (Left) then
                     Object.Ax     := -(20.0 + Object.Vx) * 0.4;
                     Object.Target := 3 + Object.Age mod 4 - 1;
                     if 5 = Object.Target then
                        Object.Target := 3;
                     end if;

                  elsif Game.Keys (Right) then
                     Object.Ax     := (20.0 - Object.Vx) * 0.4;
                     Object.Target := 9 + Object.Age mod 4 - 1;
                     if 11 = Object.Target then
                        Object.Target := 9;
                     end if;

                  else
                     Object.Ax := -Object.Vx * 0.8;
                     if Object.Target >= 6 then
                        Object.Target := (Object.Target + 1) mod
                          PIG_FRAMES;
                     elsif Object.Target /= 0 then
                        Object.Target := Object.Target - 1;
                     end if;
                  end if;

               when Falling =>
                  if Game.Keys (Left) then
                     Object.Ax := -(20.0 + Object.Vx) * 0.2;
                  elsif Game.Keys (Right) then
                     Object.Ax := (20.0 - Object.Vx) * 0.2;
                  else
                     Object.Ax := -Object.Vx * 0.2;
                  end if;
                  Object.Target := (Object.Target + 1) mod PIG_FRAMES;
            end case;

            Object.Timer (1) := 1;

         when Timer_1 =>
            if Object.X < 0.0 then
               Object.X := 0.0;
            elsif Object.X > Float (Object.Owner.View.Width - 1) then
               Object.X := Float (Object.Owner.View.Width - 1);
            end if;

            case Object.State is
               when Waiting => null;

               when Walking =>
                  if Object.Power /= 0 then
                     Object.Power := Object.Power - 1;
                  end if;
                  Object.Image := Sprite_Counts (Object.Target mod PIG_FRAMES);
                  if No_Side = Pig_Test_Map (Game,
                                             Pixels (Object.X),
                                             Pixels (Object.Y + 1.0))
                  then
                     Object.State := Falling;
                     Object.Ay    := Float (GRAV_ACC);
                  end if;

                  if Game.Jump /= 0 or Game.Keys (Up) then
                     Object.Ay    := 0.0;
                     Object.Vy    := -JUMP_SPEED;
                     Object.State := Falling;
                     Game.Jump    := 0;
                  end if;

               when Falling =>
                  if Object.Vy > 2.0 then
                     Object.Power := 3;
                  end if;
                  Object.Ay    := GRAV_ACC;
                  Object.Image := Sprite_Counts (Object.Target);

               when Knocked =>
                  Object.Power  := 0;
                  Object.Ay     := GRAV_ACC;
                  Object.Target := (Object.Target + 2) mod PIG_FRAMES;
                  Object.Image  := Sprite_Counts (Object.Target);
                  Object.Ax     := -Object.Vx * 0.2;

               when Next_Level =>
                  Object.Vx     := (Float (SCREEN_W / 2) - Object.X) * 0.1;
                  Object.Target := (Object.Target + 1) mod PIG_FRAMES;
                  Object.Image  := Sprite_Counts (Object.Target);

               when Dead =>
                  Object.Ax := 0.0;
                  Object.Ay := 0.0;
                  Object.Vx := 0.0;
                  Object.Vy := 0.0;

            end case;
            if Game.Jump /= 0 then
               Game.Jump := Game.Jump - 1;
            end if;

            if Next_Level /= Object.State then
               if Game.Enemycount <= 0 then
                  Message (Game, "Well Done!");
                  Object.State := Next_Level;
                  Object.Vy :=  0.0;
                  Object.Ay := -1.0;
                  Object.Tile_Mask := No_Side;
                  Object.Hit_Group := 0;
                  Object.Timer (2) := 50;
               end if;
            end if;

         when Timer_2 =>
            --  Snap out of KNOCKED mode
            Object.State := Falling;

         when Timer_3 =>
            case Object.State is

               when Next_Level =>
                  Add_Life (Game);
                  Unlink_Object (Object);
                  Load_Level (Game, Map_Type (Game.Level + 1));
                  New_Player (Game);

               when others =>
                  Unlink_Object (Object);
                  declare
                     Object : Object_Access;
                  begin
                     New_Player (Game, Object);
                     if Object = null then
                        Load_Level (Game, 0);
                     end if;
                  end;

            end case;

         when Hit_Tile =>
            if Object.State /= Knocked then
               if Event.Collision.Hit.Top then
                  Object.Y  := Float (Event.Collision.Y);
                  Object.Vy := 0.0;
                  Object.Ay := 0.0;
               end if;
               Object.State := Walking;
            end if;

         when Hit_Object =>
            if Knocked /= Object.State then

               case Event.Obj.Hit_Group is

                  when GROUP_ENEMY =>
                     if
                       (Object.Power /= 0 and Event.Collision.Hit.Top) or
                       (Object.Vy - Event.Obj.Vy) >= 15.0
                     then
                        --  Win: Stomp!
                        Inc_Score (Game, Event.Obj.Score);
                        Event.Obj.Y := Float (Event.Collision.Y + 10);
                        if Object.Vy > 0.0 then
                           Event.Obj.Vy := Object.Vy;
                        else
                           Event.Obj.Vy := 10.0;
                        end if;
                        Event.Obj.Ay        := GRAV_ACC;
                        Event.Obj.Tile_Mask := No_Side;
                        Event.Obj.Hit_Group := 0;

                        if Game.Jump /= 0 or Game.Keys (Up) then
                           --  Mega jump!
                           Object.Vy   := -JUMP_SPEED + 7.0;
                           Game.Jump := 0;
                        else
                           --  Bounce a little
                           Object.Vy := -15.0;
                        end if;
                        Object.Y     := Float (Event.Collision.Y);
                        Object.Ay    := 0.0;
                        Object.State := Falling;
                     else
                        --  Lose: Knocked!
                        Object.Vy        := -15.0;
                        Object.Ay        := GRAV_ACC;
                        Object.State     := Knocked;
                        Object.Timer (1) := 11;
                        New_Star (Game, Pixels (Object.X), Pixels (Object.Y) - 20, -5,  3);
                        New_Star (Game, Pixels (Object.X), Pixels (Object.Y) - 20,  2, -6);
                        New_Star (Game, Pixels (Object.X), Pixels (Object.Y) - 20,  4,  4);
                     end if;

                  when GROUP_POWERUP =>
                     case Event.Obj.Score is

                        when Power_Ups'Pos (Power_Life) =>
                           Add_Life (Game);
                           Message (Game, "Extra Life!");

                        when Power_Ups'Pos (Power_Bonus_1) =>
                           --  Double or 100k bonus!
                           if Game.Score < 100000 then
                              Inc_Score_Nobonus (Game, Game.Score);
                              Message (Game, "Double Score!");
                           else
                              Inc_Score_Nobonus (Game, 100000);
                              Message (Game, "100 000!");
                           end if;

                        when Power_Ups'Pos (Power_Bonus_2) =>
                           Inc_Score_Nobonus (Game, 1000);
                           Message (Game, "1000!");

                        when others => null;
                     end case;
                     Event.Obj.State     := Dead;
                     Event.Obj.Tile_Mask := No_Side;
                     Event.Obj.Hit_Group := 0;
                     Event.Obj.Vy        := -20.0;
                     Event.Obj.Ay        := -2.0;

                  when others =>
                     null;
               end case;
            end if;

         when Offscreen =>
            --  Dead pigs don't care about being off-screen.
            --  A timer is used to remove them, and to continue
            --  the game with a new life.

            if
              Object.State /= Dead and
              Object.Y >= 0.0  -- Above the playfield is ok.
            then
               if Game.Lives /= 0 then
                  Message (Game, "Oiiiiiiink!!!");
               else
                  Message (Game, "Game Over!");
               end if;
               Object.State     := Dead;
               Object.Timer (2) := 50;
            end if;

         when others =>
            null;
      end case;
   end Player_Handler;

   ---------------------
   -- Powerup_Handler --
   ---------------------

   procedure Powerup_Handler (Object : in out Game_Object;
                              Event  :        Pig_Event)
   is
      Game : Game_State renames Game_Access (Object.Owner).all;
   begin
      case Event.Kind is

         when Preframe =>
            if Object.State /= Dead then
               Object.Ax    := (Float (Object.Target) - Object.Vx) * 0.3;
               Object.Ay    := GRAV_ACC;
               Object.Image := Sprite_Counts (Object.Age mod 8);
               Object.Power := Object.Power + 1;
            end if;

         when Hit_Tile =>
            if Object.State /= Dead then
               if Object.Power > 2 then
                  Object.Target := -Object.Target;
               end if;
               Object.Power := 0;
               Object.Vy    := 0.0;
               Object.Ay    := 0.0;
               Object.X     := Float (Event.Collision.X) + Object.Vx;
               Object.Y     := Float (Event.Collision.Y);
            end if;

         when Offscreen =>
            if Object.Y > Float (SCREEN_H) or Object.Y < -100.0 then
               Unlink_Object (Object);
               Game.Enemycount := Game.Enemycount - 1;
            end if;
         when others => null;

      end case;
   end Powerup_Handler;

   ------------------
   -- Star_Handler --
   ------------------

   procedure Star_Handler (Object : in out Game_Object;
                           Event  :        Pig_Event)
   is
   begin
      case Event.Kind is

         when Preframe =>
            if Object.Age >= 8 then
               Unlink_Object (Object);
            else
               Object.Image := Sprite_Counts (Object.Age);
            end if;

         when others => null;
      end case;
   end Star_Handler;

   ------------------
   -- Evil_Handler --
   ------------------

   procedure Evil_Handler (Object : in out Game_Object;
                           Event  :        Pig_Event)
   is
      Game   : Game_State renames Game_Access (Object.Owner).all;
      Look_X : Pixels;
   begin
      case Event.Kind is

         when Preframe =>
            if Dead /= Object.State then
               Object.Ax    := (Float (Object.Target) - Object.Vx) * 0.5;
               Object.Ay    := Float (GRAV_ACC);
               Object.Image := Sprite_Counts (Object.Age mod 16);
            end if;

         when Hit_Tile =>
            if Dead /= Object.State then
               Object.Vy := 0.0;
               Object.Ay := 0.0;
               Object.X  := Float (Event.Collision.X) + Object.Vx;
               Object.Y  := Float (Event.Collision.Y);
            end if;

         when Offscreen =>
            if Object.Y > Float (SCREEN_H) then
               Unlink_Object (Object);
               Game.Enemycount := Game.Enemycount - 1;
            end if;

         when Postframe =>
            if Dead /= Object.State then
               Look_X := 10 + Pixels (abs (Object.Vx * 2.0));
               if Object.Target < 0 then
                  Look_X := -Look_X;
               end if;
               if
                 No_Side = Pig_Test_Map (Game_State (Object.Owner.all),
                                         Pixels (Object.X) + Look_X,
                                         Pixels (Object.Y) + 1)
               then
                  Object.Target := -Object.Target;
               end if;
            end if;

         when others =>
            null;

      end case;
   end Evil_Handler;

   -------------------
   -- Slime_Handler --
   -------------------

   procedure Slime_Handler (Object : in out Game_Object;
                            Event  :        Pig_Event)
   is
--      use Engines;
      Game   : Game_State renames Game_Access (Object.Owner).all;
      Look_X : Pixels;
   begin
      case Event.Kind is

         when Preframe =>
            if Dead /= Object.State then
               Object.Ax    := (Float (Object.Target) - Object.Vx) * 0.2;
               Object.Ay    := GRAV_ACC;
               Object.Image := Sprite_Counts (Object.Age mod 16);
            end if;

         when Hit_Tile =>
            Object.Vy := Float (-(JUMP_SPEED + GRAV_ACC));
            Object.Ay := 0.0;
            Object.Y  := Float (Event.Collision.Y);

         when Offscreen =>
            if Object.Y > Float (SCREEN_H) then
               Unlink_Object (Object);
               Game.Enemycount := Game.Enemycount - 1;
            end if;

         when Postframe =>
            if Dead /= Object.State then
               --  Don't bother looking if we're close to a floor.
               if No_Side = Pig_Test_Map_Vector (Game_State (Object.Owner.all),
                                                 Pixels (Object.X),
                                                 Pixels (Object.Y),
                                                 Pixels (Object.X),
                                                 Pixels (Object.Y + 48.0),
                                                 Top_Side, null)
               then
                  --  Turn around if there's no floor!
                  Look_X := 10 + Pixels (abs (Object.Vx * 4.0));
                  if Object.Target < 0 then
                     Look_X := -Look_X;
                  end if;

                  if
                    No_Side = Pig_Test_Map_Vector
                      (Game_State (Object.Owner.all),
                       Pixels (Object.X) + Look_X,
                       Pixels (Object.Y),
                       Pixels (Object.X) + Look_X, SCREEN_H,
                       Top_Side, null)
                  then
                     Object.Target := -Object.Target;
                  end if;
               end if;
            end if;

         when others =>
            null;
      end case;
   end Slime_Handler;

   ------------------------
   -- Chain_Head_Handler --
   ------------------------

   procedure Chain_Head_Handler (Object : in out Game_Object;
                                 Event  :        Pig_Event)
   is
      use Ada.Numerics.Elementary_Functions;
      Game : Game_State renames Game_Access (Object.Owner).all;

      procedure Do_Timer_2;

      procedure Do_Timer_2 is
         subtype Object_States is Engines.Object_States;
      begin
         case Object.State is
            when Knocked | Dead | Next_Level => null;

            when Waiting =>
               Object.Timer (2) := 35;
               Object.State     := Object_States'Succ (Object.State);

            when Walking =>
               Object.Target    := -SCREEN_W;
               Object.Timer (2) := 50;
               Object.State     := Object_States'Succ (Object.State);
               if Game.Messages > 0 then
                  Game.Messages := Game.Messages - 1;
               end if;

            when Falling =>
               Unlink_Object (Object);

         end case;
      end Do_Timer_2;

   begin
      case Event.Kind is

         when Preframe =>
            Object.Vx := (Float (Object.Target) - Object.X) * 0.3;
            Object.Vy := 15.0 * Cos (Float (Object.Age) * 0.3) - 9.0;
            if
              Game.Messages > 1 and
              Object.State = Walking
            then
               Object.Timer (2) := 0;
            end if;

            if Object.Timer (2) = 0 then
               Do_Timer_2;
            end if;

         when Timer_2 =>
            Do_Timer_2;

         when others => null;

      end case;
   end Chain_Head_Handler;

   ------------------------
   -- Chain_link_handler --
   ------------------------

   procedure Chain_Link_Handler (Object : in out Game_Object;
                                 Event  :        Pig_Event)
   is
      Target : constant Object_Access :=
        Engines.Find_Object (Object, Object_Id (Object.Target));
   begin
      case Event.Kind is

         when Preframe =>
            if Target /= null then
               Object.Vx := ((Target.X + Float (FONT_SPACING)) - Object.X) * 0.6;
               Object.Vy := (Target.Y - Object.Y) * 0.6 - 9.0;
            else
               Unlink_Object (Object);
            end if;

         when others =>
            null;
      end case;
   end Chain_Link_Handler;

   ----------------
   -- Load_Level --
   ----------------

   procedure Load_Level (Game : in out Game_State;
                         Map  :        Map_Type)
   is
      M     : String (1 .. 25 * 17);
      K     : String (1 .. 48);      -- Tile types in Map
      Dummy : Object_Access;
   begin
      Game.Level := Game_Level (Map);
      Game.Unlink_All_Objects;
      Game.Enemycount := 0;
      Game.Messages   := 0;

      case Map is
         when 1 | 2 | 4 =>
            K :=
              ("abcd" & "efgh" & "ijkl" &  --  Red, green, yellow
                 "0123456789ABCDEFG"  &    --  Sky
                 "xyz" &                   --  Single R, G, Y
                 "................");

         when 0 | 3 =>
            K :=
              ("abcd" & "efgh" & "ijkl" &  --  Red, green, yellow
                 "................."  &
                 "xyz"                &    --  Single R, G, Y
                 "-+012345..ABCDEF");      --  Night sky
      end case;

      case Map is

         when 0 =>
            M :=
                ("-------------ad----------" &
                 "-abcd-x-ad--ad-abcd-acd--" &
                 "-x----x--abcd--x----x--x-" &
                 "-abd--x---ad---abd--x--x-" &
                 "-x----x--abcd--x----x--x-" &
                 "-x----x-ad--ad-abcd-abd--" &
                 "----efhad-eh--egh-efh----" &
                 "----y--y-y--y--y--y------" &
                 "++++efh++efgh++y++eh+++++" &
                 "0123y50y2y45y12y45y123450" &
                 "ABCDyFAyCyEFyBCyEFeghDEFA" &
                 "----ijkjl-ijkl--ijkjl----" &
                 "----il--il-il--il--------" &
                 "----ijkjl--il--il-ikl----" &
                 "----il-----il--il--il----" &
                 "----il----ijkl--ijkjl----" &
                 "-------------------------");

         when 1 =>
            M :=
                ("0000000000000000000000000" &
                 "1111111111111111111111111" &
                 "2222222222222222222222222" &
                 "3333333333333333333333333" &
                 "4444444444444444444444444" &
                 "5555555555555555555555555" &
                 "6666666666666666666666666" &
                 "7777777ijkjkjjkjkl7777777" &
                 "8888888888888888888888888" &
                 "9999999999999999999999999" &
                 "abcdAAAAAAAAAAAAAAAAAabcd" &
                 "BBBBBBBBBBBBBBBBBBBBBBBBB" &
                 "CCCCCCCCCCCCCCCCCCCCCCCCC" &
                 "efgfgffgfgfgfgfggffgfgfgh" &
                 "EEEEEEEEEEEEEEEEEEEEEEEEE" &
                 "FFFFFFFFFFFFFFFFFFFFFFFFF" &
                 "GGGGGGGGGGGGGGGGGGGGGGGGG");
            New_Evil (Game,  2, 0, 5, Dummy);
            New_Evil (Game, 22, 0, 5, Dummy);
            New_Evil (Game,  5, 0, 7, Dummy);
            New_Evil (Game, 19, 0, 7, Dummy);

         when 2 =>
            M :=
                ("0000000000000000000000000" &
                 "1111111111111111111111111" &
                 "2222222222222222222222222" &
                 "3333333333333333333333333" &
                 "4444444444xxxxx4444444444" &
                 "5555555555x555x5555555555" &
                 "6666666666x666x6666666666" &
                 "7777777xxxx777xxxx7777777" &
                 "8888888x888888888x8888888" &
                 "9999999x999999999x9999999" &
                 "AAAAAAAxxxxAAAxxxxAAAAAAA" &
                 "BBBBBBBBBBxBBBxBBBBBBBBBB" &
                 "CCCCCCCCCCxCCCxCCCCCCCCCC" &
                 "DDDDDDDDDDxxxxxDDDDDDDDDD" &
                 "EEEEEEEEEEEEEEEEEEEEEEEEE" &
                 "ijklFFFFFFFFFFFFFFFFFijkl" &
                 "GGGijlGilGilGilGilGiklGGG");
            New_Slime (Game,  2, 0, -5, Dummy);
            New_Slime (Game, 22, 0,  5, Dummy);
            New_Evil  (Game,  8, 0,  7, Dummy);
            New_Evil  (Game, 16, 0, -7, Dummy);

         when 3 =>
            M :=
                ("-------------------------" &
                 "-------------------------" &
                 "-------------------------" &
                 "-------------------------" &
                 "ijkl----------efgh-------" &
                 "-------------------------" &
                 "-------------------------" &
                 "z----------------abcbcbbd" &
                 "+++++++++++++++++++++++++" &
                 "01z3450123450123450123450" &
                 "ABCDEFABCefgfgfghFABCDEFA" &
                 "----z--------------------" &
                 "-------------------------" &
                 "------z--------------ijkl" &
                 "-------------------------" &
                 "-------------------------" &
                 "abdefghijkl---efghijklabd");
            New_Slime (Game,  5,  0, -5, Dummy);
            New_Slime (Game, 20, 15, -5, Dummy);
            New_Evil  (Game,  1,  0,  7, Dummy);
            New_Evil  (Game, 20,  0, 10, Dummy);
            New_Evil  (Game, 15,  0,  7, Dummy);

         when 4 =>
            M :=
                ("0000000000000000000000000" &
                 "1111111111111111111111111" &
                 "2222222222222222222222222" &
                 "3333333333333333333333333" &
                 "4444444444444444444444444" &
                 "555555555555z555555555555" &
                 "66666666666ijl66666666666" &
                 "7777777777ijlil7777777777" &
                 "888888888ijlikkl888888888" &
                 "99999999ijkjklikl99999999" &
                 "AAAAAAAikjlijkjkjlAAAAAAA" &
                 "BBBBBBiklijkjlijkjlBBBBBB" &
                 "CCCCCijkjlikkjklikklCCCCC" &
                 "DDDDijklijjklikjkjkklDDDD" &
                 "EEEijkkjkjlikjkjlijjklEEE" &
                 "FFijkjlilijkjklikjlikklFF" &
                 "efggfggfgfgfggfgfgfgfgfgh");
            New_Evil  (Game, 11, 0,   5, Dummy);
            New_Evil  (Game, 10, 0,   6, Dummy);
            New_Evil  (Game,  9, 0,   7, Dummy);
            New_Evil  (Game,  8, 0,   8, Dummy);
            New_Evil  (Game,  7, 0,   9, Dummy);
            New_Evil  (Game,  6, 0,  10, Dummy);
            New_Evil  (Game,  5, 0,  11, Dummy);
            New_Evil  (Game,  4, 0,  12, Dummy);
            New_Evil  (Game,  3, 0,  13, Dummy);
            New_Slime (Game,  1, 0,  16, Dummy);
            New_Slime (Game, 24, 0, -14, Dummy);
      end case;
      Engines.Pig_Map_From_String (Game.Map.all, Trans => K, Data => M);
      Game.Refresh_Screen := Game.Pages;
   end Load_Level;

   --------------------
   -- Before_Objects --
   --------------------

   procedure Before_Objects (Game : in out Game_State)
   is
   begin
      if Game.Lives_Wobble > 0.0 then
         Game.Lives_Wobble := Game.Lives_Wobble * 0.95;
         Game.Lives_Wobble := Game.Lives_Wobble - 0.3;
         if Game.Lives_Wobble < 0.0 then
            Game.Lives_Wobble := 0.0;
         end if;
      end if;

      if Game.Score_Wobble > 0.0 then
         Game.Score_Wobble := Game.Score_Wobble * 0.95;
         Game.Score_Wobble := Game.Score_Wobble - 0.3;
         if Game.Score_Wobble < 0.0 then
            Game.Score_Wobble := 0.0;
         end if;
      end if;
      Game.Logic_Frames := Game.Logic_Frames + 1;

      if 0 = Game.Level then
         case Game.Fun_Count mod 60 is

            when 17 =>  New_Powerup (Game, 250, -20, -10, Power_Life);
            when 29 =>  New_Powerup (Game, 550, -20,  10, Power_Life);
            when 37 =>  New_Powerup (Game, 250, -20,  10, Power_Bonus_2);
            when 51 =>  New_Powerup (Game, 550, -20, -10, Power_Bonus_1);
            when others => null;
         end case;
         if 150 = Game.Fun_Count mod 300 then
            Message (Game, "Press Space!");
         end if;
         Game.Fun_Count := Game.Fun_Count + 1;
      end if;
   end Before_Objects;

   ---------------
   -- Dashboard --
   ---------------

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
      Game.Surfac.Set_Clip_Rectangle (Clip);

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
              (Format => Game.Surfac.Pixel_Format,
               Red    => (Colour_Component ((128.0 * F1      + 64.0) * M)),
               Green  => (Colour_Component ((64.0  * F1 * F2 + 64.0) * M)),
               Blue   => (Colour_Component ((128.0 * F2      + 32.0) * M)));
         begin
            Game.Surfac.Fill (Line, Pixel);
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
               Pixels (X) + Pixels (Game.Lives_Wobble *
                                      Sin (Float (Game.Lives_Wobble_Time) * 20.0
                                             + Float (I) * 1.7)),
               SCREEN_H - 56 / 2);
         end loop;
      end;

      --  Print score
      declare
         X : Float   := Float (SCREEN_W + 5);
         V : Integer := Game.Score;
         N : Engines.Sprite_Counts;
      begin
         for I in reverse 0 .. 9 loop
            N := Engines.Sprite_Counts (V mod 10);
            X := X - 39.0 - Game.Score_Wobble *
              Sin (Float (Game.Score_Wobble_Time) * 15.0 + Float (I) * 0.5);
            Game.Pig_Draw_Sprite (Game.Scorefont + N,
                                  Pixels (X),
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

   ----------------
   -- Start_Game --
   ----------------

   procedure Start_Game (Game : in out Game_State)
   is
      Player : Engines.Object_Access;
   begin
      if Game.Level /= 0 then
         return;                -- Already playing!
      end if;

      Game.Score := 0;
      Game.Lives := 5;

      Load_Level (Game, 1);

      New_Player (Game, Player);
      Game.Player := Player;
   end Start_Game;

   ----------------------------------------------------------
   --        Input; events and game control keys
   ----------------------------------------------------------

   ------------------
   -- Handle_Input --
   ------------------

   procedure Handle_Input (Game  : in out Game_State;
                           Event : in out SDL.Events.Events.Events)
   is
      use SDL.Events.Keyboards;
      use SDL.Events.Mice;
   begin
      case Event.Common.Event_Type is

         when Key_Down =>
            case Event.Keyboard.Key_Sym.Scan_Code is

               when Scan_Code_F1 =>
                  Game.Interpolation := not Game.Interpolation;
                  Message (Game, (if Game.Interpolation
                                    then "Interpolation: ON"
                                    else "Interpolation: OFF"));

               when Scan_Code_F2 =>
                  Game.Direct := not Game.Direct;
                  Message (Game, (if Game.Direct
                                    then "Rendering: Direct"
                                    else "Rendering: Buffered"));

               when Scan_Code_F3 =>
                  Game.Show_Dirtyrects := not Game.Show_Dirtyrects;
                  Message (Game, (if Game.Show_Dirtyrects
                                    then"Dirtyrects: ON"
                                    else "Dirtyrects: OFF"));

               when Scan_Code_F4 =>
                  Game.Nice := not Game.Nice;
                  Message (Game, (if Game.Nice
                                    then "Be Nice: ON"
                                    else "Be Nice: OFF"));

               when Scan_Code_Up    =>  Game.Jump := 3;
               when Scan_Code_Space =>  Start_Game (Game);
               when others          =>  null;
            end case;

         when Key_Up =>
            case Event.Keyboard.Key_Sym.Key_Code
               is
               when Code_Escape =>  Game.Running := False;
               when others      =>  null;
            end case;

         when SDL.Events.Quit   =>  Game.Running := False;

         when Button_Up =>  null;
         when others    =>  null;
      end case;
   end Handle_Input;

   -----------------
   -- Handle_Keys --
   -----------------

   procedure Handle_Keys (Game : in out Game_State)
   is
   begin
      null;
   end Handle_Keys;

   ---------------
   -- Play_Game --
   ---------------

   procedure Play_Game (Double_Buffer : Boolean;
                        Full_Screen   : Boolean;
                        BPP           : Positive)
   is
      pragma Unreferenced (Double_Buffer, Full_Screen, BPP);
      use Ada.Real_Time;
      Window     : SDL.Video.Windows.Window;
      Screen     : SDL.Video.Surfaces.Surface;
      Last_Tick  : Ada.Real_Time.Time;
      Start_Time : Ada.Real_Time.Time;
      Dashframe  : Integer;
      Logic_FPS  : constant Float := 20.0;
      --   flags      : Integer := SDL_DOUBLEBUF + SDL_HWSURFACE; -- |
      use type SDL.Init_Flags;
      Sdl_Flags : constant := (SDL.Enable_Screen
                                 + SDL.Enable_Audio -- Workaround, clean cleanup
                                 + SDL.Enable_Events);
      procedure Sdl_Initialise;

      procedure Sdl_Initialise is
      begin
         if not SDL.Initialise (Sdl_Flags) then
            raise Program_Error with "Could not initialise SDL library.";
         end if;
      end Sdl_Initialise;

   begin

      Sdl_Initialise;

      declare
         use SDL.Video.Windows;
      begin
         Makers.Create
           (Window,
            Title    => "Fixed Rate Pig Game",
            Position => (10, 10),
            Size     => (SCREEN_W, SCREEN_H),
            Flags    => SDL.Video.Windows.Windowed);
         --  Screen := SDL_SetVideoMode (SCREEN_W, SCREEN_H, bpp, flags);
         Screen := Window.Get_Surface;

         Screen.Fill (Area   => (0, 0,
                                 Width  => Screen.Size.Width,
                                 Height => Screen.Size.Height),
                      Colour => 16#00_00_00_00#);
      end;

      declare
         Game : aliased Game_State := Create;
      begin
         Game.Setup (Self   => Engines.Game_Engine (Game)'Unchecked_Access,
                     Screen => Screen,
                     Pages  => 1);
--         Game.Create;
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

               --  Clean object list
               Game.Clean_Object_List;

               --  Make the new frame visible
               Game.Pig_Flip (Window);

               --  Update statistics, timers and stuff
               Game.Rendered_Frames   := Game.Rendered_Frames + 1;
               Game.Lives_Wobble_Time := Game.Lives_Wobble_Time + Dt;
               Game.Score_Wobble_Time := Game.Score_Wobble_Time + Dt;
               Game.Dashboard_Time    := Game.Dashboard_Time + Dt;

               Last_Tick := Tick2;
               if Game.Nice then
                  delay 0.050;
               end if;
            end;
         end loop;

         SDL.Finalise;

         --  Print some statistics
         Print_Some_Statistics :
         declare
            package Float_IO is new Ada.Text_IO.Float_IO (Float);
            use Ada.Text_IO, Float_IO;
            End_Time      : constant Time      := Ada.Real_Time.Clock;
            Game_Span     : constant Time_Span := End_Time - Start_Time;
            Game_Duration : constant Duration  := To_Duration (Game_Span);
            Duration_MS   : constant Float     := 1000.0 * Float (Game_Duration);
            Duration_S    : constant Float     := 0.001  * Float'Max (1.0, Duration_MS);
            Rendered_FPS  : constant Float     := Float (Game.Rendered_Frames) / Duration_S;
            Logical_FPS   : constant Float     := Float (Game.Logic_Frames)    / Duration_S;
         begin
            Default_Exp := 0;
            Default_Aft := 0;
            Put ("          Total time running: "); Put (Duration_MS);  Put_Line (" ms");
            Default_Aft := 2;
            Put ("Average rendering frame rate: "); Put (Rendered_FPS); Put_Line (" fps");
            Put ("    Average logic frame rate: "); Put (Logical_FPS);  Put_Line (" fps");
         end Print_Some_Statistics;
      end;
   end Play_Game;

end Games;
