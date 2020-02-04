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
with Ada.Unchecked_Conversion;

with Engines;

package Games
  with Elaborate_Body
is

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

   type Game_State is record

      --  I/O
      Engine         : Engines.PIG_Engine_Access;
      Keys           : Key_Array;
      Nice           : Boolean;
      Refresh_Screen : Integer;
      Jump           : Integer;

      --  Sprites
      Lifepig   : Integer;
      Scorefont : Integer;
      Glassfont : Integer;
      Icons     : Integer;
      Stars     : Integer;
      Pigframes : Integer;
      Evil      : Integer;
      Slime     : Integer;

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
      Player            : Engines.PIG_Object_Access;

      --  Statistics
      Logic_Frames      : Integer;
      Rendered_Frames   : Integer;

      Start_Time        : Ada.Real_Time.Time;
   end record;
   type Game_State_Access is access all Game_State;

   Clean_Game : constant Game_State :=
     (Engine            => null,
      Keys              => (others => False),
      Nice              => False,
      Refresh_Screen    => 0,
      Jump              => 0,
      Running           => False,
      Lives_Wobble      => 0.0,
      Lives_Wobble_Time => 0.0,
      Score_Wobble      => 0.0,
      Score_Wobble_Time => 0.0,
      Dashboard_Time    => 0.0,
      Player            => null,
      Start_Time        => Ada.Real_Time.Time_First,
      others            => 0);


   procedure Inc_Score (Game : in out Game_State; V : Integer);

   procedure Inc_Score_Nobonus (Game : in out Game_State; V : Integer);

   procedure Message (Game : in out Game_State; Text : in String);

   procedure Add_Life (Game : in out Game_State);

   procedure Remove_Life (Game : in out Game_State);

   procedure New_Player (Game : in out Game_State);

   procedure New_Player (Game   : in out Game_State;
                         Object :    out Engines.PIG_Object_Access);

   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engines.Power_Ups;
                          Object :    out not null Engines.PIG_Object_Access);

   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engines.Power_Ups);

   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer);

   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer;
                       Object :    out not null Engines.PIG_Object_Access);

   procedure New_Evil (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Speed  : in     Integer;
                       Object :    out not null Engines.PIG_Object_Access);

   procedure New_Slime (Game   : in out Game_State;
                        X, Y   :        Integer;
                        Speed  : in     Integer;
                        Object :    out not null Engines.PIG_Object_Access);

   procedure New_Chain_Head (Game     : in out Game_State;
                             X, Y     : in     Integer;
                             Image    : in     Integer;
                             Target_X : in     Integer;
                             Object   :    out not null Engines.PIG_Object_Access);

   procedure New_Chain_Link (Game   : in out Game_State;
                             X, Y   : in     Integer;
                             Image  : in     Integer;
                             Target : in     Integer;
                             Object :    out not null Engines.PIG_Object_Access);


   type Map_Type is range 0 .. 4;
   procedure Load_Level (Game : in out Game_State; Map : in Map_Type);

   procedure Player_Handler (Object : in out Engines.PIG_Object;
                             Event  : in     Engines.PIG_Event);

   procedure Powerup_Handler (Object : in out Engines.PIG_Object;
                              Event  : in     Engines.PIG_Event);

   procedure Star_Handler (Object : in out Engines.PIG_Object;
                           Event  : in     Engines.PIG_Event);

   procedure Evil_Handler (Object : in out Engines.PIG_Object;
                           Event  : in     Engines.PIG_Event);

   procedure Slime_Handler (Object : in out Engines.PIG_Object;
                            Event  : in     Engines.PIG_Event);

   procedure Chain_Head_Handler (Object : in out Engines.PIG_Object;
                                 Event  : in     Engines.PIG_Event);

   procedure Chain_Link_Handler (Object : in out Engines.PIG_Object;
                                 Event  : in     Engines.PIG_Event);

   function To_Game_State is
      new Ada.Unchecked_Conversion (Long_Integer, Game_State_Access);

   function From_Game_State is
      new Ada.Unchecked_Conversion (Game_State_Access, Long_Integer);

   procedure Before_Objects (Engine : in out Engines.PIG_Engine);

end Games;
