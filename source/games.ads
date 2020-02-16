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

with SDL.Events.Events;

with Engines;

package Games
  with Elaborate_Body
is
   use Engines;

   --  Graphics defines
   SCREEN_W     : constant := 800;
   SCREEN_H     : constant := 600;
   TILE_W       : constant := 32;
   TILE_H       : constant := 32;
   MAP_W        : constant := 25;
   MAP_H        : constant := 17;
   FONT_SPACING : constant := 45;
   PIG_FRAMES   : constant := 12;

   --  World/physics constants
   GRAV_ACC      : constant :=  4.0;
   JUMP_SPEED    : constant := 28.0;

   --  Sprite collision groups
   GROUP_ENEMY   : constant :=  16#0001#;
   GROUP_POWERUP : constant :=  16#0002#;

   type Key_Used  is (Up, Down, Left, Right);
   type Key_Array is array (Key_Used) of Boolean;

   type Game_State is
     new Game_Engine
     with record

        --  I/O
        Keys           : Key_Array;
        Nice           : Boolean;
        Refresh_Screen : Integer;
        Jump           : Integer;

        --  Sprites
        Lifepig   : Sprite_Index;
        Scorefont : Sprite_Index;
        Glassfont : Sprite_Index;
        Icons     : Sprite_Index;
        Stars     : Sprite_Index;
        Pigframes : Sprite_Index;
        Evil      : Sprite_Index;
        Slime     : Sprite_Index;

        --  Global game state
        Running           : Boolean;
        Level             : Integer;
        Lives             : Integer;
        Lives_Wobble      : Float;
        Lives_Wobble_Time : Duration;
        Score             : Integer;
        Score_Wobble      : Float;
        Score_Wobble_Time : Duration;
        Dashboard_Time    : Duration;
        Fun_Count         : Integer;
        Enemycount        : Integer;
        Messages          : Integer;

        --  Objects
        Player            : Object_Access;

        --  Statistics
        Logic_Frames      : Integer;
        Rendered_Frames   : Integer;

        Start_Time        : Ada.Real_Time.Time;
   end record;
   type Game_Access is access all Game_State;
   type Game_Class  is access all Game_State'Class;

   overriding
   procedure Initialize (Game : in out Game_State);

   overriding
   procedure Finalize (Game : in out Game_State);

   procedure Create (Game : in out Game_State);

   procedure Inc_Score (Game : in out Game_State; V : Integer);

   procedure Inc_Score_Nobonus (Game : in out Game_State; V : Integer);

   procedure Message (Game : in out Game_State;
                      Text :        String);

   procedure Add_Life (Game : in out Game_State);

   procedure Remove_Life (Game : in out Game_State);

   procedure New_Player (Game : in out Game_State);

   procedure New_Player (Game   : in out Game_State;
                         Object :    out Object_Access);

   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   :        Pixels;
                          Speed  :        Integer;
                          kind   :        Power_Ups;
                          Object :    out not null Object_Access);

   procedure New_Powerup (Game  : in out Game_State;
                          X, Y  :        Pixels;
                          Speed :        Integer;
                          Kind  :        Power_Ups);

   procedure New_Star (Game   : in out Game_State;
                       X, Y   :        Pixels;
                       Vx, Vy :        Pixels);

   procedure New_Star (Game   : in out Game_State;
                       X, Y   :        Pixels;
                       Vx, Vy :        Pixels;
                       Object :    out not null Object_Access);

   procedure New_Evil (Game   : in out Game_State;
                       X, Y   :        Pixels;
                       Speed  :        Integer;
                       Object :    out not null Object_Access);

   procedure New_Slime (Game   : in out Game_State;
                        X, Y   :        Pixels;
                        Speed  :        Integer;
                        Object :    out not null Object_Access);

   procedure New_Chain_Head (Game     : in out Game_State;
                             X, Y     :        Pixels;
                             Image    :        Sprite_Index;
                             Target_X :        Integer;
                             Object   :    out not null Object_Access);

   procedure New_Chain_Link (Game   : in out Game_State;
                             X, Y   :        Pixels;
                             Image  :        Sprite_Index;
                             Target :        Object_Id;
                             Object :    out not null Object_Access);

   overriding
   procedure Before_Objects (Game : in out Game_State);

   type Map_Type is range 0 .. 4;
   procedure Load_Level (Game : in out Game_State;
                         Map  :        Map_Type);


   procedure Player_Handler (Object : in out Game_Object;
                             Event  :        PIG_Event);

   procedure Powerup_Handler (Object : in out Game_Object;
                              Event  :        PIG_Event);

   procedure Star_Handler (Object : in out Game_Object;
                           Event  :        PIG_Event);

   procedure Evil_Handler (Object : in out Game_Object;
                           Event  :        PIG_Event);

   procedure Slime_Handler (Object : in out Game_Object;
                            Event  :        PIG_Event);

   procedure Chain_Head_Handler (Object : in out Game_Object;
                                 Event  :        PIG_Event);

   procedure Dashboard (Game : in out Game_State);
   --  Render the dashboard

   procedure Chain_Link_Handler (Object : in out Game_Object;
                                 Event  :        PIG_Event);

   procedure Start_Game (Game : in out Game_State);

   procedure Handle_Input (Game  : in out Game_State;
                           Event : in out SDL.Events.Events.Events);

   procedure Handle_Keys (Game : in out Game_State);

   procedure Play_Game (Double_Buffer : Boolean;
                        Full_Screen   : Boolean;
                        BPP           : Positive);

end Games;
