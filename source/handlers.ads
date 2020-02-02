--
--
--

with Ada.Real_Time;
with Ada.Unchecked_Conversion;

with Engine;

package Handlers is

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

   type Key_Used is (Up, Down, Left, Right);
   type Key_Array is array (Key_Used) of Boolean;
   type Game_State is record
      --  I/O
      Pe         : Engine.PIG_Engine_Access;
--        Uint8           *keys;
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
      Lives_Wobble_Time : Duration; -- Float;
      Score             : Integer;
      Score_Wobble      : Float;
      Score_Wobble_Time : Duration; -- Float;
      Dashboard_Time    : Duration; -- Float;
      Fun_Count         : Integer;
      Enemycount        : Integer;
      Messages          : Integer;

      --  Objects
      Player            : Engine.PIG_Object_Access;

      --  Statistics
      Logic_Frames      : Integer;
      Rendered_Frames   : Integer;

      Start_Time  : Ada.Real_Time.Time;
   end record;
   type Game_State_Access is access all Game_State;


   procedure Inc_Score (Gs : in out Game_State; V : Integer);
   procedure Inc_Score_Nobonus (Gs : in out Game_State; V : Integer);
   procedure Message (Game : in out Game_State; Text : in String);
   procedure Add_Life (Gs : in out Game_State);
   procedure Remove_Life (Gs : in out Game_State);
--   procedure Inc_Score (Gs : in out Game_State; V : Integer);
--   procedure Inc_Score_Nobonus (Gs : in out Game_State; V : Integer);
   --  static PIG_object *new_player(Game_State *gs);
   procedure New_Player (Game : in out Game_State);
   procedure New_Player (Game   : in out Game_State;
                         Object :    out Engine.PIG_Object_Access);

   --  static PIG_object *new_powerup(Game_State *gs,
   --                  int x, int y, int speed, POWERUPS type);
   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engine.Power_Ups;
                          Object :    out Engine.PIG_Object_Access);
   procedure New_Powerup (Game   : in out Game_State;
                          X, Y   : in     Integer;
                          Speed  : in     Integer;
                          Type_C : in     Engine.Power_Ups);
   --  static PIG_object *new_star(Game_State *gs, int x, int y, int vx, int vy);
   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer);
   procedure New_Star (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Vx, Vy : in     Integer;
                       Object :    out Engine.PIG_Object_Access);
   procedure New_Evil (Game   : in out Game_State;
                       X, Y   : in     Integer;
                       Speed  : in     Integer;
                       Object :    out Engine.PIG_Object_Access);
   --  static PIG_object *new_evil(Game_State *gs,
   --                  int x, int y, int speed);
   procedure New_Slime (Game   : in out Game_State;
                        X, Y   :        Integer;
                        Speed  : in     Integer;
                        Object :    out Engine.PIG_Object_Access);
   --  static PIG_object *new_slime(Game_State *gs,
   --                  int x, int y, int speed);
   procedure New_Chain_Head (Game     : in out Game_State;
                             X, Y     : in     Integer;
                             Image    : in     Integer;
                             Target_X : in     Integer;
                             Object   :    out Engine.PIG_Object_Access);
   procedure New_Chain_Link (Game   : in out Game_State;
                             X, Y   : in     Integer;
                             Image  : in     Integer;
                             Target : in     Integer;
                             Object :    out Engine.PIG_Object_Access);


   type Map_Type is range 0 .. 4;
   procedure Load_Level (Gs : in out Game_State; Map : in Map_Type);

   procedure Player_Handler (Po : in out Engine.PIG_Object;
                             Ev : in     Engine.PIG_Event);

   procedure Powerup_Handler (Po : in out Engine.PIG_Object;
                              Ev : in     Engine.PIG_Event);

   procedure Star_Handler (Po : in out Engine.PIG_Object;
                           Ev : in     Engine.PIG_Event);

   procedure Evil_Handler (Po : in out Engine.PIG_Object;
                           Ev : in     Engine.PIG_Event);

   procedure Slime_Handler (Po : in out Engine.PIG_Object;
                            Ev : in     Engine.PIG_Event);

   procedure Chain_Head_Handler (Object : in out Engine.PIG_Object;
                                 Event  : in     Engine.PIG_Event);

   procedure Chain_Link_Handler (Po : in out Engine.PIG_Object;
                                 Ev : in     Engine.PIG_Event);

   function To_Game_State is
      new Ada.Unchecked_Conversion (Long_Integer, Game_State_Access);

   function From_Game_State is
      new Ada.Unchecked_Conversion (Game_State_Access, Long_Integer);

   procedure Before_Objects (Pe : in out Engine.PIG_Engine);

end Handlers;
