--
--
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions;

with SDL;

package body Handlers is

   ----------------------------------------------------------
   --        Accounting (score, lives etc)
   ----------------------------------------------------------

   procedure Add_Life (Gs : in out Game_State) is
   begin
      Gs.Lives := Gs.Lives + 1;
      Gs.Lives_Wobble := Gs.Lives_Wobble + 10.0;
      if Gs.Lives_Wobble > 15.0 then
         Gs.Lives_Wobble := 15.0;
      end if;
      Gs.Lives_Wobble_Time := 0.0;
   end Add_Life;


   procedure Remove_Life (Gs : in out Game_State)
   is
   begin
      Gs.Lives := Gs.Lives - 1;
      Gs.Lives_Wobble := Gs.Lives_Wobble + 10.0;
      if Gs.Lives_Wobble > 15.0 then
         Gs.Lives_Wobble := 15.0;
      end if;
      Gs.Lives_Wobble_Time := 0.0;
   end Remove_Life;


   procedure New_Player (Game   : in out Game_State;
                         Object :    out Engine.PIG_Object_Access)
   is
      --        PIG_object *po;
      --        if(!gs->lives)
      --                return NULL;
   begin
      Object := Engine.Pig_Object_Open (Game.Pe, SCREEN_W / 2, -50, 1);
      --        if(!po)
      --                return NULL;

      Remove_Life (Game);
      Object.Ibase   := Game.Pigframes;
      Object.Handler := Handlers.Player_Handler'Access;
      Object.Hitmask := GROUP_POWERUP + GROUP_ENEMY;
   end New_Player;


   procedure New_Player (Game : in out Game_State) is
      Dummy : Engine.PIG_Object_Access;
   begin
      New_Player (Game, Dummy);
   end New_Player;


   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engine.Power_Ups;
                          Object :    out Engine.PIG_Object_Access)
   is
   begin
      Object := Engine.Pig_Object_Open (Game.Pe, X, Y, 1);
      --        if(!po)
      --                return NULL;

      Game.Enemycount := Game.Enemycount + 1;
      Object.Score    := Engine.Power_Ups'Pos (Type_C);
      Object.Ibase    := Game.Icons + 8 * Object.Score;
      Object.Target   := Speed;
      Object.Handler  := Handlers.Powerup_Handler'Access;
      Object.Tilemask := Engine.PIG_Top;
      Object.Hitgroup := GROUP_POWERUP;
      --        return po;
   end New_Powerup;


   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engine.Power_Ups)
   is
      Dummy : Engine.PIG_Object_Access;
   begin
      New_Powerup (Game, X, Y, Speed, Type_C, Dummy);
   end New_Powerup;


   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer;
                       Object :    out Engine.PIG_Object_Access)
   is
   begin
      Object := Engine.Pig_Object_Open (Game.Pe, X + Vx, Y + Vy, 1);
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
      Dummy : Engine.PIG_Object_Access;
   begin
      New_Star (Game, X, Y, Vx, Vy, Dummy);
   end New_Star;


   procedure New_Evil (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Speed  : in     Integer;
                       Object :    out Engine.PIG_Object_Access)
   is
   begin
      Object := Engine.Pig_Object_Open (Game.Pe,
                                        X * TILE_W,
                                        Y * TILE_H, 1);
      --        if(!po)
      --                return NULL;

      Game.Enemycount := Game.Enemycount + 1;
      Object.Ibase    := Game.Evil;
      Object.Target   := Speed;
      Object.Handler  := Handlers.Evil_Handler'Access;
      Object.Score    := 200;
      Object.Tilemask := Engine.PIG_Top;
      Object.Hitgroup := GROUP_ENEMY;
   end New_Evil;


   procedure New_Slime (Game   : in out Game_State;
                        X, Y   :        Integer;
                        Speed  : in     Integer;
                        Object :    out Engine.PIG_Object_Access)
   is
   begin
      Object := Engine.Pig_Object_Open (Game.Pe,
                                        X * TILE_W, Y * TILE_H, 1);
      --        if(!po)
      --                return NULL;

      Game.Enemycount := Game.Enemycount + 1;
      Object.Ibase    := Game.Slime;
      Object.Target   := Speed;
      Object.Handler  := Handlers.Slime_Handler'Access;
      Object.Score    := 300;
      Object.Tilemask := Engine.PIG_Top;
      Object.Hitgroup := GROUP_ENEMY;
   end New_Slime;


   procedure New_Chain_Head (Game     : in out Game_State;
                             X, Y     : in     Integer;
                             Image    : in     Integer;
                             Target_X : in     Integer;
                             Object   :    out Engine.PIG_Object_Access)
   is
   begin
      Object := Engine.Pig_Object_Open (Game.Pe, X, Y, 1);
      --        if(!po)
      --                return NULL;

      Object.Ibase   := Image;
      Object.Handler := Handlers.Chain_Head_Handler'Access;
      Object.Target  := Target_X;
   end New_Chain_Head;


   procedure New_Chain_Link (Game     : in out Game_State;
                             X, Y   : in     Integer;
                             Image  : in     Integer;
                             Target : in     Integer;
                             Object      :    out Engine.PIG_Object_Access)
   is
   begin
      Object := Engine.Pig_Object_Open (Game.Pe, X, Y, 1);
      --        if(!po)
      --                return NULL;

      Object.Ibase   := Image;
      Object.Handler := Handlers.Chain_Link_Handler'Access;
      Object.Target  := Target;
   end New_Chain_Link;


   procedure Inc_Score_Nobonus (Gs : in out Game_State; V : in Integer)
   is
      Vc : Integer := V;
      Os : constant Integer := Gs.Score;
   begin
      Gs.Score := Gs.Score + Vc;
      while Vc /= 0 loop
         Gs.Score_Wobble := Gs.Score_Wobble + 1.0;
         Vc := Vc / 10;
      end loop;
      if Gs.Score_Wobble > 15.0 then
         Gs.Score_Wobble := 15.0;
      end if;
      Gs.Score_Wobble_Time := 0.0;
      if Os / 10000 /= Gs.Score / 10000 then
         New_Powerup (Gs, SCREEN_W / 2, -20, -4, Engine.Power_Life);
      end if;
   end Inc_Score_Nobonus;


   procedure Inc_Score (Gs : in out Game_State; V : in Integer)
   is
      Os : constant Integer := Gs.Score;
   begin
      Inc_Score_Nobonus (Gs, V);
      if Os / 5000 /= Gs.Score / 5000 then
         New_Powerup (Gs, SCREEN_W / 2, -20, 8, Engine.Power_Bonus_1);
      elsif Os / 1000 /= Gs.Score / 1000 then
         New_Powerup (Gs, SCREEN_W / 2, -20, -6, Engine.Power_Bonus_2);
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
      Object : Engine.PIG_Object_Access := null;
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


   procedure Player_Handler (Po : in out Engine.PIG_Object;
                             Ev : in     Engine.PIG_Event)
   is
      use Engine;
      use type SDL.C.int;
      Game : constant Game_State_Access := To_Game_State (Po.Owner.Userdata);
      Gs   : Game_State renames Game.all;
   begin
      case Ev.Type_C is

         when PIG_PREFRAME =>
            case Po.State is
               when Knocked | Dead | Next_Level => null;

               when Waiting =>
                  if 1 = Po.Age then
                     Message (Gs, "Get ready!");
                  elsif Po.Age > 50 then
                     Po.State := Falling;
                  end if;

               when Walking =>
                  --  if Gs.Keys (SDL.Events.Keyboards.Code_Left) then
                  if Gs.Keys (Left) then
                     Po.Ax     := -(20.0 + Po.Vx) * 0.4;
                     Po.Target := 3 + Po.Age mod 4 - 1;
                     if 5 = Po.Target then
                        Po.Target := 3;
                     end if;
                  --  elsif Gs.Keys (SDL.Events.Keyboards.Code_Right) then
                  elsif Gs.Keys (Right) then

                     Po.Ax     := (20.0 - Po.Vx) * 0.4;
                     Po.Target := 9 + Po.Age mod 4 - 1;
                     if 11 = Po.Target then
                        Po.Target := 9;
                     end if;
                  else

                     Po.Ax := -Po.Vx * 0.8;
                     if Po.Target >= 6 then
                        Po.Target := (Po.Target + 1) mod
                          PIG_FRAMES;
                     elsif Po.Target /= 0 then
                        Po.Target := Po.Target - 1;
                     end if;
                  end if;

               when Falling =>
                  --  if Gs.Keys (SDL.Events.Keyboards.Code_Left) then
                  if Gs.Keys (Left) then
                     Po.Ax := -(20.0 + Po.Vx) * 0.2;
                  --  elsif Gs.Keys (SDL.Events.Keyboards.Code_Right) then
                  elsif Gs.Keys (Right) then
                     Po.Ax := (20.0 - Po.Vx) * 0.2;
                  else
                     Po.Ax := -Po.Vx * 0.2;
                  end if;
                  Po.Target := (Po.Target + 1) mod PIG_FRAMES;
            end case;

            Po.Timer (0) := 1;

         when PIG_TIMER0 =>
            if Po.X < 0.0 then
               Po.X := 0.0;
            elsif Po.X > Float (Po.Owner.View.Width - 1) then
               Po.X := Float (Po.Owner.View.Width - 1);
            end if;

            case Po.State is
               when Waiting => null;

               when Walking =>
                  if Po.Power /= 0 then
                     Po.Power := Po.Power - 1;
                  end if;
                  Po.Image := Po.Target mod PIG_FRAMES;
                  if PIG_None = Pig_Test_Map (Gs.Pe.all,
                                              Integer (Po.X),
                                              Integer (Po.Y + 1.0))
                  then
                     Po.State := Falling;
                     Po.Ay    := Float (GRAV_ACC);
                  end if;
                  --  if Gs.Jump or Gs.Keys (SDL.Events.Keyboards.Key_Up) then
                  if Gs.Jump /= 0 or Gs.Keys (Up) then
                     Po.Ay    := 0.0;
                     Po.Vy    := -JUMP_SPEED;
                     Po.State := Falling;
                     Gs.Jump  := 0;
                  end if;

               when Falling =>
                  if Po.Vy > 2.0 then
                     Po.Power := 3;
                  end if;
                  Po.Ay    := GRAV_ACC;
                  Po.Image := Po.Target;

               when Knocked =>
                  Po.Power  := 0;
                  Po.Ay     := GRAV_ACC;
                  Po.Target := (Po.Target + 2) mod PIG_FRAMES;
                  Po.Image  := Po.Target;
                  Po.Ax     := -Po.Vx * 0.2;

               when Next_Level =>
                  Po.Vx     := (Float (SCREEN_W / 2) - Po.X) * 0.1;
                  Po.Target := (Po.Target + 1) mod PIG_FRAMES;
                  Po.Image  := Po.Target;

               when Dead =>
                  Po.Ax := 0.0;
                  Po.Ay := 0.0;
                  Po.Vx := 0.0;
                  Po.Vy := 0.0;

            end case;
            if Gs.Jump /= 0 then
               Gs.Jump := Gs.Jump - 1;
            end if;

            if Next_Level /= Po.State then
               if Gs.Enemycount <= 0 then
                  Message (Gs, "Well Done!");
                  Po.State := Next_Level;
                  Po.Vy :=  0.0;
                  Po.Ay := -1.0;
                  Po.Tilemask  := PIG_None; -- 0;
                  Po.Hitgroup  := 0;
                  Po.Timer (2) := 50;
               end if;
            end if;

         when PIG_TIMER1 =>
            --  Snap out of KNOCKED mode
            Po.State := Falling;

         when PIG_TIMER2 =>
            case Po.State is

               when Next_Level =>
                  Add_Life (Gs);
                  Pig_Object_Close (Po);
                  Load_Level (Gs, Map_Type (Gs.Level + 1));
                  New_Player (Gs);

               when others =>
                  Pig_Object_Close (Po);
                  declare
                     Object : PIG_Object_Access;
                  begin
                     New_Player (Gs, Object);
                 --    if Obj = null then
                 --       Load_Level (gs, 0);
                 --    end if;
                  end;

            end case;

         when PIG_HIT_TILE =>
            if Po.State /= Knocked then
               if Ev.Cinfo.Sides.Top then
                  Po.Y  := Float (Ev.Cinfo.Y);
                  Po.Vy := 0.0;
                  Po.Ay := 0.0;
               end if;
               Po.State := Walking;
            end if;

         when PIG_HIT_OBJECT =>
            if Knocked /= Po.State then

               case Ev.Obj.Hitgroup is

                  when GROUP_ENEMY =>
                     if
                       (Po.Power /= 0 and Ev.Cinfo.Sides.Top) or
                       (Po.Vy - Ev.Obj.Vy) >= 15.0
                     then
                        --  Win: Stomp!
                        Inc_Score (Gs, Ev.Obj.Score);
                        Ev.Obj.Y := Float (Ev.Cinfo.Y + 10);
                        if Po.Vy > 0.0 then
                           Ev.Obj.Vy := Po.Vy;
                        else
                           Ev.Obj.Vy := 10.0;
                        end if;
                        Ev.Obj.Ay       := GRAV_ACC;
                        Ev.Obj.Tilemask := PIG_None; -- 0;
                        Ev.Obj.Hitgroup := 0;
                        --  if Gs.jump or Gs.Keys (SDL.Events.Keyboards.Code_Up) then
                        if Gs.Jump /= 0 or Gs.Keys (Up) then
                           --  Mega jump!
                           Po.Vy   := -JUMP_SPEED + 7.0;
                           Gs.Jump := 0;
                        else
                           --  Bounce a little
                           Po.Vy := -15.0;
                        end if;
                        Po.Y     := Float (Ev.Cinfo.Y);
                        Po.Ay    := 0.0;
                        Po.State := Falling;
                     else
                        --  Lose: Knocked!
                        Po.Vy        := -15.0;
                        Po.Ay        := GRAV_ACC;
                        Po.State     := Knocked;
                        Po.Timer (1) := 11;
                        New_Star (Gs, Integer (Po.X), Integer (Po.Y) - 20, -5,  3);
                        New_Star (Gs, Integer (Po.X), Integer (Po.Y) - 20,  2, -6);
                        New_Star (Gs, Integer (Po.X), Integer (Po.Y) - 20,  4,  4);
                     end if;

                  when GROUP_POWERUP =>
                     case Ev.Obj.Score is

                        when Power_Ups'Pos (Power_Life) =>
                           Add_Life (Gs);
                           Message (Gs, "Extra Life!");

                        when Power_Ups'Pos (Power_Bonus_1) =>
                           --  Double or 100k bonus!
                           if Gs.Score < 100000 then
                              Inc_Score_Nobonus (Gs, Gs.Score);
                              Message (Gs, "Double Score!");
                           else
                              Inc_Score_Nobonus (Gs, 100000);
                              Message (Gs, "100 000!");
                           end if;

                        when Power_Ups'Pos (Power_Bonus_2) =>
                           Inc_Score_Nobonus (Gs, 1000);
                           Message (Gs, "1000!");

                        when others => null;
                     end case;
                     Ev.Obj.State    := Dead;
                     Ev.Obj.Tilemask := PIG_None; -- 0;
                     Ev.Obj.Hitgroup := 0;
                     Ev.Obj.Vy       := -20.0;
                     Ev.Obj.Ay       := -2.0;

                  when others =>
                     null;
               end case;
            end if;

         when PIG_OFFSCREEN =>
            --  Dead pigs don't care about being off-screen.
            --  A timer is used to remove them, and to continue
            --  the game with a new life.

            if
              Po.State /= Dead and
              Po.Y >= 0.0  -- Above the playfield is ok.
            then
               if Gs.Lives /= 0 then
                  Message (Gs, "Oiiiiiiink!!!");
               else
                  Message (Gs, "Game Over!");
               end if;
               Po.State     := Dead;
               Po.Timer (2) := 50;
            end if;

         when others =>
            null;
      end case;
   end Player_Handler;


   procedure Powerup_Handler (Po : in out Engine.PIG_Object;
                              Ev : in     Engine.PIG_Event)
   is
      use Engine;
      Game : constant Game_State_Access := To_Game_State (Po.Owner.Userdata);
      Gs   : Game_State renames Game.all;
   begin
      case Ev.Type_C is

         when PIG_PREFRAME =>
            if Po.State /= Dead then
               Po.Ax    := (Float (Po.Target) - Po.Vx) * 0.3;
               Po.Ay    := GRAV_ACC;
               Po.Image := Po.Age mod 8;
               Po.Power := Po.Power + 1;
            end if;

         when PIG_HIT_TILE =>
            if Po.State /= Dead then
               if Po.Power > 2 then
                  Po.Target := -Po.Target;
               end if;
               Po.Power := 0;
               Po.Vy    := 0.0;
               Po.Ay    := 0.0;
               Po.X     := Float (Ev.Cinfo.X) + Po.Vx;
               Po.Y     := Float (Ev.Cinfo.Y);
            end if;

         when PIG_OFFSCREEN =>
            if Po.Y > Float (SCREEN_H) or Po.Y < -100.0 then
               Pig_Object_Close (Po);
               Gs.Enemycount := Gs.Enemycount - 1;
            end if;
         when others => null;

      end case;
   end Powerup_Handler;


   procedure Star_Handler (Po : in out Engine.PIG_Object;
                           Ev : in     Engine.PIG_Event)
   is
      use Engine;
   begin
      case Ev.Type_C is

         when PIG_PREFRAME =>
            if Po.Age >= 8 then
               Pig_Object_Close (Po);
            else
               Po.Image := Po.Age;
            end if;

         when others => null;
      end case;
   end Star_Handler;


   procedure Evil_Handler (Po : in out Engine.PIG_Object;
                           Ev : in     Engine.PIG_Event)
   is
      use Engine;
      Game   : constant Game_State_Access := To_Game_State (Po.Owner.Userdata);
      Gs     : Game_State renames Game.all;
      Look_X : Integer;
   begin
      case Ev.Type_C is

         when PIG_PREFRAME =>
            if Dead /= Po.State then
               Po.Ax    := (Float (Po.Target) - Po.Vx) * 0.5;
               Po.Ay    := Float (GRAV_ACC);
               Po.Image := Po.Age mod 16;
            end if;

         when PIG_HIT_TILE =>
            if Dead /= Po.State then
               Po.Vy := 0.0;
               Po.Ay := 0.0;
               Po.X  := Float (Ev.Cinfo.X) + Po.Vx;
               Po.Y  := Float (Ev.Cinfo.Y);
            end if;

         when PIG_OFFSCREEN =>
            if Po.Y > Float (SCREEN_H) then
               Pig_Object_Close (Po);
               Gs.Enemycount := Gs.Enemycount - 1;
            end if;

         when PIG_POSTFRAME =>
            if Dead /= Po.State then
               Look_X := 10 + Integer (abs (Po.Vx * 2.0));
               if Po.Target < 0 then
                  Look_X := -Look_X;
               end if;
               if
                 PIG_None = Pig_Test_Map (Po.Owner.all,
                                          Integer (Po.X) + Look_X,
                                          Integer (Po.Y) + 1)
               then
                  Po.Target := -Po.Target;
               end if;
            end if;

         when others =>
            null;

      end case;
   end Evil_Handler;


   procedure Slime_Handler (Po : in out Engine.PIG_Object;
                            Ev : in     Engine.PIG_Event)
   is
      use Engine;
      Game   : constant Game_State_Access := To_Game_State (Po.Owner.Userdata);
      Gs     : Game_State renames Game.all;
      Look_X : Integer;
   begin
      case Ev.Type_C is

         when PIG_PREFRAME =>
            if Dead /= Po.State then
               Po.Ax    := (Float (Po.Target) - Po.Vx) * 0.2;
               Po.Ay    := GRAV_ACC;
               Po.Image := Po.Age mod 16;
            end if;

         when PIG_HIT_TILE =>
            Po.Vy := Float (-(JUMP_SPEED + GRAV_ACC));
            Po.Ay := 0.0;
            Po.Y  := Float (Ev.Cinfo.Y);

         when PIG_OFFSCREEN =>
            if Po.Y > Float (SCREEN_H) then
               Pig_Object_Close (Po);
               Gs.Enemycount := Gs.Enemycount - 1;
            end if;

         when PIG_POSTFRAME =>
            if Dead /= Po.State then
               --  Don't bother looking if we're close to a floor.
               if PIG_None = Pig_Test_Map_Vector (Po.Owner.all,
                                                  Integer (Po.X),
                                                  Integer (Po.Y),
                                                  Integer (Po.X),
                                                  Integer (Po.Y + 48.0),
                                                  PIG_Top, null)
               then
                  --  Turn around if there's no floor!
                  Look_X := 10 + Integer (abs (Po.Vx * 4.0));
                  if Po.Target < 0 then
                     Look_X := -Look_X;
                  end if;
                  if PIG_None = Pig_Test_Map_Vector (Po.Owner.all,
                                                     Integer (Po.X) + Look_X,
                                                     Integer (Po.Y),
                                                     Integer (Po.X) + Look_X, SCREEN_H,
                                                     PIG_Top, null)
                  then
                     Po.Target := -Po.Target;
                  end if;
               end if;
            end if;

         when others =>
            null;
      end case;
   end Slime_Handler;


   procedure Chain_Head_Handler (Object : in out Engine.PIG_Object;
                                 Event  : in     Engine.PIG_Event)
   is
      use Ada.Numerics.Elementary_Functions;
      use Engine;
      GP   : constant Game_State_Access := To_Game_State (Object.Owner.Userdata);
      Game : Game_State renames GP.all;

      procedure Do_Timer_1;
      procedure Do_Timer_1 is
      begin
         case Object.State is
            when Knocked | Dead | Next_Level => null;

            when Waiting =>  -- 0 =>
               Object.Timer (1) := 35;
               Object.State := Engine.Object_States'Succ (Object.State); --  + 1;

            when Walking => -- 1 =>
               Object.Target    := -SCREEN_W;
               Object.Timer (1) := 50;
               Object.State := Engine.Object_States'Succ (Object.State); --  + 1;
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


   procedure Chain_Link_Handler (Po : in out Engine.PIG_Object;
                                 Ev : in     Engine.PIG_Event)
   is
      use Engine;
      Target : constant PIG_Object_Access := Pig_Object_Find (Po, Po.Target);
   begin
      case Ev.Type_C is

         when PIG_PREFRAME =>
            if Target /= null then
               Po.Vx := ((Target.X + Float (FONT_SPACING)) - Po.X) * 0.6;
               Po.Vy := (Target.Y - Po.Y) * 0.6 - 9.0;
            else
               Pig_Object_Close (Po);
            end if;

         when others =>
            null;
      end case;
   end Chain_Link_Handler;


   procedure Load_Level (Gs : in out Game_State; Map : in Map_Type)
   is
      use Ada.Strings.Unbounded;
      M, K  : Unbounded_String;
      Dummy : Engine.PIG_Object_Access;
   begin
      Ada.Text_IO.Put_Line ("## 3-1");
      Gs.Level := Integer (Map);
      Engine.Pig_Object_Close_All (Gs.Pe.all);
      Gs.Enemycount := 0;
      Gs.Messages   := 0;
      Ada.Text_IO.Put_Line ("## 3-2");
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
            New_Evil (Gs,  2, 0, 5, Dummy);
            New_Evil (Gs, 22, 0, 5, Dummy);
            New_Evil (Gs,  5, 0, 7, Dummy);
            New_Evil (Gs, 19, 0, 7, Dummy);

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
            New_Slime (Gs,  2, 0, -5, Dummy);
            New_Slime (Gs, 22, 0,  5, Dummy);
            New_Evil  (Gs,  8, 0,  7, Dummy);
            New_Evil  (Gs, 16, 0, -7, Dummy);

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
            New_Slime (Gs,  5,  0, -5, Dummy);
            New_Slime (Gs, 20, 15, -5, Dummy);
            New_Evil  (Gs,  1,  0,  7, Dummy);
            New_Evil  (Gs, 20,  0, 10, Dummy);
            New_Evil  (Gs, 15,  0,  7, Dummy);

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
            New_Evil  (Gs, 11, 0,   5, Dummy);
            New_Evil  (Gs, 10, 0,   6, Dummy);
            New_Evil  (Gs,  9, 0,   7, Dummy);
            New_Evil  (Gs,  8, 0,   8, Dummy);
            New_Evil  (Gs,  7, 0,   9, Dummy);
            New_Evil  (Gs,  6, 0,  10, Dummy);
            New_Evil  (Gs,  5, 0,  11, Dummy);
            New_Evil  (Gs,  4, 0,  12, Dummy);
            New_Evil  (Gs,  3, 0,  13, Dummy);
            New_Slime (Gs,  1, 0,  16, Dummy);
            New_Slime (Gs, 24, 0, -14, Dummy);
      end case;
      Ada.Text_IO.Put_Line ("## 3-3");
      Engine.Pig_Map_From_String (Gs.Pe.Map.all, To_String (K), To_String (M));
      Ada.Text_IO.Put_Line ("## 3-4");
      Gs.Refresh_Screen := Gs.Pe.Pages;
      Ada.Text_IO.Put_Line ("## 3-5 done");
--     exception
--        when Constraint_Error =>
--           Ada.Text_IO.Put_Line ("exception hit CE");
--           raise;
--        when others =>
--           Ada.Text_IO.Put_Line ("exception hit");
--           raise;
   end Load_Level;


   procedure Before_Objects (Pe : in out Engine.PIG_Engine)
   is
      use Engine;
      Game : constant Game_State_Access := Handlers.To_Game_State (Pe.Userdata);
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
