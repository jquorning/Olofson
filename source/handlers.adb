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

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions;

with SDL;

package body Handlers is

   ----------------------------------------------------------
   --        Accounting (score, lives etc)
   ----------------------------------------------------------

   procedure Add_Life (Game : in out Game_State) is
   begin
      Game.Lives := Game.Lives + 1;
      Game.Lives_Wobble := Game.Lives_Wobble + 10.0;
      if Game.Lives_Wobble > 15.0 then
         Game.Lives_Wobble := 15.0;
      end if;
      Game.Lives_Wobble_Time := 0.0;
   end Add_Life;


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


   procedure New_Player (Game   : in out Game_State;
                         Object :    out Engines.PIG_Object_Access)
   is
      --        PIG_object *po;
      --        if(!gs->lives)
      --                return NULL;
   begin
      Object := Engines.Pig_Object_Open (Game.Engine, SCREEN_W / 2, -50, 1);
      --        if(!po)
      --                return NULL;

      Remove_Life (Game);
      Object.Ibase   := Game.Pigframes;
      Object.Handler := Handlers.Player_Handler'Access;
      Object.Hitmask := GROUP_POWERUP + GROUP_ENEMY;
   end New_Player;


   procedure New_Player (Game : in out Game_State) is
      Dummy : Engines.PIG_Object_Access;
   begin
      New_Player (Game, Dummy);
   end New_Player;


   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engines.Power_Ups;
                          Object :    out Engines.PIG_Object_Access)
   is
   begin
      Object := Engines.Pig_Object_Open (Game.Engine, X, Y, 1);
      --        if(!po)
      --                return NULL;

      Game.Enemycount := Game.Enemycount + 1;
      Object.Score    := Engines.Power_Ups'Pos (Type_C);
      Object.Ibase    := Game.Icons + 8 * Object.Score;
      Object.Target   := Speed;
      Object.Handler  := Handlers.Powerup_Handler'Access;
      Object.Tilemask := Engines.PIG_Top;
      Object.Hitgroup := GROUP_POWERUP;
      --        return po;
   end New_Powerup;


   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engines.Power_Ups)
   is
      Dummy : Engines.PIG_Object_Access;
   begin
      New_Powerup (Game, X, Y, Speed, Type_C, Dummy);
   end New_Powerup;


   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer;
                       Object :    out Engines.PIG_Object_Access)
   is
   begin
      Object := Engines.Pig_Object_Open (Game.Engine, X + Vx, Y + Vy, 1);
      --        if(!po)
      --                return NULL;

      Object.Ibase   := Game.Stars;
      Object.Ax      := -0.3 * Float (Vx);
      Object.Vx      := Float (Vx * 3);
      Object.Ay      := -0.3 * Float (Vy);
      Object.Vy      := Float (Vy * 3);
      Object.Handler := Handlers.Star_Handler'Access;
   end New_Star;


   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer)
   is
      Dummy : Engines.PIG_Object_Access;
   begin
      New_Star (Game, X, Y, Vx, Vy, Dummy);
   end New_Star;


   procedure New_Evil (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Speed  : in     Integer;
                       Object :    out Engines.PIG_Object_Access)
   is
   begin
      Object := Engines.Pig_Object_Open (Game.Engine,
                                         X * TILE_W,
                                         Y * TILE_H, 1);
      --        if(!po)
      --                return NULL;

      Game.Enemycount := Game.Enemycount + 1;
      Object.Ibase    := Game.Evil;
      Object.Target   := Speed;
      Object.Handler  := Handlers.Evil_Handler'Access;
      Object.Score    := 200;
      Object.Tilemask := Engines.PIG_Top;
      Object.Hitgroup := GROUP_ENEMY;
   end New_Evil;


   procedure New_Slime (Game   : in out Game_State;
                        X, Y   : in     Integer;
                        Speed  : in     Integer;
                        Object :    out Engines.PIG_Object_Access)
   is
   begin
      Object := Engines.Pig_Object_Open (Game.Engine,
                                        X * TILE_W, Y * TILE_H, 1);
      --        if(!po)
      --                return NULL;

      Game.Enemycount := Game.Enemycount + 1;
      Object.Ibase    := Game.Slime;
      Object.Target   := Speed;
      Object.Handler  := Handlers.Slime_Handler'Access;
      Object.Score    := 300;
      Object.Tilemask := Engines.PIG_Top;
      Object.Hitgroup := GROUP_ENEMY;
   end New_Slime;


   procedure New_Chain_Head (Game     : in out Game_State;
                             X, Y     : in     Integer;
                             Image    : in     Integer;
                             Target_X : in     Integer;
                             Object   :    out Engines.PIG_Object_Access)
   is
   begin
      Object := Engines.Pig_Object_Open (Game.Engine, X, Y, 1);
      --        if(!po)
      --                return NULL;

      Object.Ibase   := Image;
      Object.Handler := Handlers.Chain_Head_Handler'Access;
      Object.Target  := Target_X;
   end New_Chain_Head;


   procedure New_Chain_Link (Game   : in out Game_State;
                             X, Y   : in     Integer;
                             Image  : in     Integer;
                             Target : in     Integer;
                             Object :    out Engines.PIG_Object_Access)
   is
   begin
      Object := Engines.Pig_Object_Open (Game.Engine, X, Y, 1);
      --        if(!po)
      --                return NULL;

      Object.Ibase   := Image;
      Object.Handler := Handlers.Chain_Link_Handler'Access;
      Object.Target  := Target;
   end New_Chain_Link;


   procedure Inc_Score_Nobonus (Game : in out Game_State; V : in Integer)
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
         New_Powerup (Game, SCREEN_W / 2, -20, -4, Engines.Power_Life);
      end if;
   end Inc_Score_Nobonus;


   procedure Inc_Score (Game : in out Game_State; V : in Integer)
   is
      Os : constant Integer := Game.Score;
   begin
      Inc_Score_Nobonus (Game, V);
      if Os / 5000 /= Game.Score / 5000 then
         New_Powerup (Game, SCREEN_W / 2, -20, 8, Engines.Power_Bonus_1);
      elsif Os / 1000 /= Game.Score / 1000 then
         New_Powerup (Game, SCREEN_W / 2, -20, -6, Engines.Power_Bonus_2);
      end if;
   end Inc_Score;


   procedure Message (Game : in out Game_State; Text : in String)
   is

      function To_Upper (C : in Character) return Character;
      function To_Upper (C : in Character) return Character is
      begin
         case C is
            when 'a' .. 'z' =>
               return Character'Val (Character'Pos (C)
                                       - Character'Pos ('a')
                                       + Character'Pos ('A'));
            when 'A' .. 'Z' =>
               return C;
            when others =>
               return 'X';
         end case;
      end To_Upper;

      X  : constant Integer := SCREEN_W + FONT_SPACING;
      Y  : constant Integer := MAP_H * TILE_H - 30;
      Tx : constant Integer := (SCREEN_W - (Text'Length - 1) * FONT_SPACING) / 2;
      Object : Engines.PIG_Object_Access := null;
   begin
      Ada.Text_IO.Put_Line ("Message : " & Text);
      for I in Text'Range loop
         declare
            C : constant Integer :=
              Character'Pos (To_Upper (Text (I))) - Character'Pos (' ') + Game.Glassfont;
         begin
            if I = Text'First then
               New_Chain_Head (Game, X, Y, C, Tx, Object);
            else
               New_Chain_Link (Game, X, Y, C, Object.Id, Object);
            end if;
            --                if(!po)
            --                        return;
         end;
      end loop;
      Game.Messages := Game.Messages + 1;
   end Message;


   procedure Player_Handler (Object : in out Engines.PIG_Object;
                             Event  : in     Engines.PIG_Event)
   is
      use Engines;
      use type SDL.C.int;
      GP   : constant Game_State_Access := To_Game_State (Object.Owner.Userdata);
      Game : Game_State renames GP.all;
   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
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

            Object.Timer (0) := 1;

         when PIG_TIMER0 =>
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
                  Object.Image := Object.Target mod PIG_FRAMES;
                  if PIG_None = Pig_Test_Map (Game.Engine.all,
                                              Integer (Object.X),
                                              Integer (Object.Y + 1.0))
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
                  Object.Image := Object.Target;

               when Knocked =>
                  Object.Power  := 0;
                  Object.Ay     := GRAV_ACC;
                  Object.Target := (Object.Target + 2) mod PIG_FRAMES;
                  Object.Image  := Object.Target;
                  Object.Ax     := -Object.Vx * 0.2;

               when Next_Level =>
                  Object.Vx     := (Float (SCREEN_W / 2) - Object.X) * 0.1;
                  Object.Target := (Object.Target + 1) mod PIG_FRAMES;
                  Object.Image  := Object.Target;

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
                  Object.Tilemask  := PIG_None; -- 0;
                  Object.Hitgroup  := 0;
                  Object.Timer (2) := 50;
               end if;
            end if;

         when PIG_TIMER1 =>
            --  Snap out of KNOCKED mode
            Object.State := Falling;

         when PIG_TIMER2 =>
            case Object.State is

               when Next_Level =>
                  Add_Life (Game);
                  Pig_Object_Close (Object);
                  Load_Level (Game, Map_Type (Game.Level + 1));
                  New_Player (Game);

               when others =>
                  Pig_Object_Close (Object);
                  declare
                     Object : PIG_Object_Access;
                  begin
                     New_Player (Game, Object);
                 --    if Obj = null then
                 --       Load_Level (gs, 0);
                 --    end if;
                  end;

            end case;

         when PIG_HIT_TILE =>
            if Object.State /= Knocked then
               if Event.Cinfo.Sides.Top then
                  Object.Y  := Float (Event.Cinfo.Y);
                  Object.Vy := 0.0;
                  Object.Ay := 0.0;
               end if;
               Object.State := Walking;
            end if;

         when PIG_HIT_OBJECT =>
            if Knocked /= Object.State then

               case Event.Obj.Hitgroup is

                  when GROUP_ENEMY =>
                     if
                       (Object.Power /= 0 and Event.Cinfo.Sides.Top) or
                       (Object.Vy - Event.Obj.Vy) >= 15.0
                     then
                        --  Win: Stomp!
                        Inc_Score (Game, Event.Obj.Score);
                        Event.Obj.Y := Float (Event.Cinfo.Y + 10);
                        if Object.Vy > 0.0 then
                           Event.Obj.Vy := Object.Vy;
                        else
                           Event.Obj.Vy := 10.0;
                        end if;
                        Event.Obj.Ay       := GRAV_ACC;
                        Event.Obj.Tilemask := PIG_None;
                        Event.Obj.Hitgroup := 0;

                        if Game.Jump /= 0 or Game.Keys (Up) then
                           --  Mega jump!
                           Object.Vy   := -JUMP_SPEED + 7.0;
                           Game.Jump := 0;
                        else
                           --  Bounce a little
                           Object.Vy := -15.0;
                        end if;
                        Object.Y     := Float (Event.Cinfo.Y);
                        Object.Ay    := 0.0;
                        Object.State := Falling;
                     else
                        --  Lose: Knocked!
                        Object.Vy        := -15.0;
                        Object.Ay        := GRAV_ACC;
                        Object.State     := Knocked;
                        Object.Timer (1) := 11;
                        New_Star (Game, Integer (Object.X), Integer (Object.Y) - 20, -5,  3);
                        New_Star (Game, Integer (Object.X), Integer (Object.Y) - 20,  2, -6);
                        New_Star (Game, Integer (Object.X), Integer (Object.Y) - 20,  4,  4);
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
                     Event.Obj.State    := Dead;
                     Event.Obj.Tilemask := PIG_None; -- 0;
                     Event.Obj.Hitgroup := 0;
                     Event.Obj.Vy       := -20.0;
                     Event.Obj.Ay       := -2.0;

                  when others =>
                     null;
               end case;
            end if;

         when PIG_OFFSCREEN =>
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


   procedure Powerup_Handler (Object : in out Engines.PIG_Object;
                              Event  : in     Engines.PIG_Event)
   is
      use Engines;
      GP   : constant Game_State_Access := To_Game_State (Object.Owner.Userdata);
      Game : Game_State renames GP.all;
   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
            if Object.State /= Dead then
               Object.Ax    := (Float (Object.Target) - Object.Vx) * 0.3;
               Object.Ay    := GRAV_ACC;
               Object.Image := Object.Age mod 8;
               Object.Power := Object.Power + 1;
            end if;

         when PIG_HIT_TILE =>
            if Object.State /= Dead then
               if Object.Power > 2 then
                  Object.Target := -Object.Target;
               end if;
               Object.Power := 0;
               Object.Vy    := 0.0;
               Object.Ay    := 0.0;
               Object.X     := Float (Event.Cinfo.X) + Object.Vx;
               Object.Y     := Float (Event.Cinfo.Y);
            end if;

         when PIG_OFFSCREEN =>
            if Object.Y > Float (SCREEN_H) or Object.Y < -100.0 then
               Pig_Object_Close (Object);
               Game.Enemycount := Game.Enemycount - 1;
            end if;
         when others => null;

      end case;
   end Powerup_Handler;


   procedure Star_Handler (Object : in out Engines.PIG_Object;
                           Event  : in     Engines.PIG_Event)
   is
      use Engines;
   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
            if Object.Age >= 8 then
               Pig_Object_Close (Object);
            else
               Object.Image := Object.Age;
            end if;

         when others => null;
      end case;
   end Star_Handler;


   procedure Evil_Handler (Object : in out Engines.PIG_Object;
                           Event  : in     Engines.PIG_Event)
   is
      use Engines;
      GP     : constant Game_State_Access := To_Game_State (Object.Owner.Userdata);
      Game   : Game_State renames GP.all;
      Look_X : Integer;
   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
            if Dead /= Object.State then
               Object.Ax    := (Float (Object.Target) - Object.Vx) * 0.5;
               Object.Ay    := Float (GRAV_ACC);
               Object.Image := Object.Age mod 16;
            end if;

         when PIG_HIT_TILE =>
            if Dead /= Object.State then
               Object.Vy := 0.0;
               Object.Ay := 0.0;
               Object.X  := Float (Event.Cinfo.X) + Object.Vx;
               Object.Y  := Float (Event.Cinfo.Y);
            end if;

         when PIG_OFFSCREEN =>
            if Object.Y > Float (SCREEN_H) then
               Pig_Object_Close (Object);
               Game.Enemycount := Game.Enemycount - 1;
            end if;

         when PIG_POSTFRAME =>
            if Dead /= Object.State then
               Look_X := 10 + Integer (abs (Object.Vx * 2.0));
               if Object.Target < 0 then
                  Look_X := -Look_X;
               end if;
               if
                 PIG_None = Pig_Test_Map (Object.Owner.all,
                                          Integer (Object.X) + Look_X,
                                          Integer (Object.Y) + 1)
               then
                  Object.Target := -Object.Target;
               end if;
            end if;

         when others =>
            null;

      end case;
   end Evil_Handler;


   procedure Slime_Handler (Object : in out Engines.PIG_Object;
                            Event  : in     Engines.PIG_Event)
   is
      use Engines;
      GP     : constant Game_State_Access := To_Game_State (Object.Owner.Userdata);
      Game   : Game_State renames GP.all;
      Look_X : Integer;
   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
            if Dead /= Object.State then
               Object.Ax    := (Float (Object.Target) - Object.Vx) * 0.2;
               Object.Ay    := GRAV_ACC;
               Object.Image := Object.Age mod 16;
            end if;

         when PIG_HIT_TILE =>
            Object.Vy := Float (-(JUMP_SPEED + GRAV_ACC));
            Object.Ay := 0.0;
            Object.Y  := Float (Event.Cinfo.Y);

         when PIG_OFFSCREEN =>
            if Object.Y > Float (SCREEN_H) then
               Pig_Object_Close (Object);
               Game.Enemycount := Game.Enemycount - 1;
            end if;

         when PIG_POSTFRAME =>
            if Dead /= Object.State then
               --  Don't bother looking if we're close to a floor.
               if PIG_None = Pig_Test_Map_Vector (Object.Owner.all,
                                                  Integer (Object.X),
                                                  Integer (Object.Y),
                                                  Integer (Object.X),
                                                  Integer (Object.Y + 48.0),
                                                  PIG_Top, null)
               then
                  --  Turn around if there's no floor!
                  Look_X := 10 + Integer (abs (Object.Vx * 4.0));
                  if Object.Target < 0 then
                     Look_X := -Look_X;
                  end if;
                  if PIG_None = Pig_Test_Map_Vector (Object.Owner.all,
                                                     Integer (Object.X) + Look_X,
                                                     Integer (Object.Y),
                                                     Integer (Object.X) + Look_X, SCREEN_H,
                                                     PIG_Top, null)
                  then
                     Object.Target := -Object.Target;
                  end if;
               end if;
            end if;

         when others =>
            null;
      end case;
   end Slime_Handler;


   procedure Chain_Head_Handler (Object : in out Engines.PIG_Object;
                                 Event  : in     Engines.PIG_Event)
   is
      use Ada.Numerics.Elementary_Functions;
      use Engines;
      GP   : constant Game_State_Access := To_Game_State (Object.Owner.Userdata);
      Game : Game_State renames GP.all;

      procedure Do_Timer_1;
      procedure Do_Timer_1 is
      begin
         case Object.State is
            when Knocked | Dead | Next_Level => null;

            when Waiting =>  -- 0 =>
               Object.Timer (1) := 35;
               Object.State := Engines.Object_States'Succ (Object.State); --  + 1;

            when Walking => -- 1 =>
               Object.Target    := -SCREEN_W;
               Object.Timer (1) := 50;
               Object.State := Engines.Object_States'Succ (Object.State); --  + 1;
               if Game.Messages > 0 then
                  Game.Messages := Game.Messages - 1;
               end if;

            when Falling => -- 2 =>
               Pig_Object_Close (Object);

         end case;
      end Do_Timer_1;

   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
            Object.Vx := (Float (Object.Target) - Object.X) * 0.3;
            Object.Vy := 15.0 * Cos (Float (Object.Age) * 0.3) - 9.0;
            if
              Game.Messages > 1 and
              Object.State = Walking  -- 1
            then
               Object.Timer (1) := 0;
            end if;

            if Object.Timer (1) = 0 then
               Do_Timer_1;
            end if;

         when PIG_TIMER1 =>
            Do_Timer_1;

         when others => null;

      end case;
   end Chain_Head_Handler;


   procedure Chain_Link_Handler (Object : in out Engines.PIG_Object;
                                 Event  : in     Engines.PIG_Event)
   is
      use Engines;
      Target : constant PIG_Object_Access := Pig_Object_Find (Object, Object.Target);
   begin
      case Event.Type_C is

         when PIG_PREFRAME =>
            if Target /= null then
               Object.Vx := ((Target.X + Float (FONT_SPACING)) - Object.X) * 0.6;
               Object.Vy := (Target.Y - Object.Y) * 0.6 - 9.0;
            else
               Pig_Object_Close (Object);
            end if;

         when others =>
            null;
      end case;
   end Chain_Link_Handler;


   procedure Load_Level (Game : in out Game_State; Map : in Map_Type)
   is
      use Ada.Strings.Unbounded;
      M, K  : Unbounded_String;
      Dummy : Engines.PIG_Object_Access;
   begin
      Game.Level := Integer (Map);
      Engines.Pig_Object_Close_All (Game.Engine.all);
      Game.Enemycount := 0;
      Game.Messages   := 0;

      case Map is
         when 1 | 2 | 4 =>
            K := To_Unbounded_String
              ("abcd" & "efgh" & "ijkl" &   --  Red, green, yellov
                 "0123456789ABCDEFG"  &   --  Sky
                 "xyz");                   --  Single R, G, Y

         when 0 | 3 =>
            K := To_Unbounded_String
              ("abcd" & "efgh" & "ijkl" &   --  Red, green, yellov
                 "................."  &
                 "xyz"                &   --  Single R, G, Y
                 "-+012345..ABCDEF");      --  Night sky
      end case;

      case Map is

         when 0 =>
            M := To_Unbounded_String
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
            M := To_Unbounded_String
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
            M := To_Unbounded_String
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
            M := To_Unbounded_String
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
            M := To_Unbounded_String
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
      Engines.Pig_Map_From_String (Game.Engine.Map.all, To_String (K), To_String (M));
      Game.Refresh_Screen := Game.Engine.Pages;
   end Load_Level;


   procedure Before_Objects (Engine : in out Engines.PIG_Engine)
   is
      use Engines;
      Game : constant Game_State_Access := Handlers.To_Game_State (Engine.Userdata);
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

            when 17 =>
               New_Powerup (Game.all, 250, -20, -10, Power_Life);
            when 29 =>
               New_Powerup (Game.all, 550, -20, 10, Power_Life);
            when 37 =>
               New_Powerup (Game.all, 250, -20, 10, Power_Bonus_2);
            when 51 =>
               New_Powerup (Game.all, 550, -20, -10, Power_Bonus_1);
            when others => null;
         end case;
         if 150 = Game.Fun_Count mod 300 then
            Message (Game.all, "Press Space!");
         end if;
         Game.Fun_Count := Game.Fun_Count + 1;
      end if;
   end Before_Objects;


end Handlers;
