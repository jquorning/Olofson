--------------------------------------------------------------
--  Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.Video.Windows;

with Dirty;

package Engines
  with Elaborate_Body
is
   ----------------------------------------------------------
   --        Game Engine
   ----------------------------------------------------------
   type Object_States is
     (Waiting,
      Walking,
      Falling,
      Knocked,
      Next_Level,
      Dead);
   --  Moved from pig.adb

   type Power_Ups is
     (Power_Life,
      Power_Bonus_1,
      Power_Bonus_2);
   --  Moved from pig.adb

   type Pixels is new Integer;
   type Tiles  is new Integer;


   type Game_Object;
   type Game_Engine;
   type Object_Access is access all Game_Object;
   type Engine_Access is access all Game_Engine;

   type    Sprite_Counts is new Natural;
   subtype Sprite_Index  is Sprite_Counts range 1 .. Sprite_Counts'Last;

   --
   --  Interpolated point
   --
   type PIG_Ipoint is record
      --  From the last logic frame:
      Ox, Oy : Float;         --  Position

      --  From the last/current rendered frame:
      Gimage : Sprite_Counts; --  Sprite frame index
      Gx, Gy : Float;         --  Interpolated position
   end record;

   --
   --  Game logic events
   --
   type PIG_Events is
     (
      PIG_PREFRAME,
      --  Occurs once per logic frame, before collision and
      --  off-screen detection, and before timer handlers.

      PIG_TIMER_1,
      PIG_TIMER_2,
      PIG_TIMER_3,
      --  Occurs whenever timer x expires. Timers are one-
      --  shot, but can be reloaded by the handler for
      --  periodic action. Timer events are handled before
      --  before collision and off-screen detection.

      PIG_HIT_TILE,
      --  Occurs when the hot-spot of an object hits a
      --  marked side of a tile, and the corresponding bit
      --  in 'tilemask' is set.

      PIG_HIT_OBJECT,
      --  Occurs when the collision circle of an object
      --  intersects the collision circle of another object,
      --  provided one or more bits in 'hitgroup' of the
      --  other object matches bits in 'hitmask'.

      PIG_OFFSCREEN,
      --  Occurs when an object is off-screen. This takes
      --  in account the hot-spot and bounding rectangle of
      --  the current sprite frame.

      PIG_POSTFRAME
        --  Occurs once per logic frame, after collision
        --  detection, off-screen detection and all other
        --  events.
   );

   type Pig_Sides is record
      Top    : Boolean;
      Bottom : Boolean;
      Left   : Boolean;
      Right  : Boolean;
   end record;
   PIG_Top  : constant Pig_Sides := (Top => True, others => False);
   PIG_None : constant Pig_Sides := (others => False);
   PIG_All  : constant Pig_Sides := (others => True);

   --  Magic values
   PIG_UNCHANGED : constant := -10000000;
   PIG_MIN       : constant := -10000001;
   PIG_CENTER    : constant := -10000002;
   PIG_MAX       : constant := -10000003;

   --  Collision info
   type PIG_Cinfo is record
      Ff    : Float;      --  Fractional frame
      X, Y  : Pixels;     --  Exact position
      Sides : Pig_Sides;  --  Side of tile hit
   end record;


   type PIG_Event is record
      Kind  : PIG_Events;

      Cinfo : PIG_Cinfo;      --  Detailed collision info
      --  For HIT_TILE, HIT_OBJECT and OFFSCREEN

      Obj   : Object_Access;  --  Which object?
      --  For HIT_OBJECT
   end record;

   --
   --  Logic object
   --
   type Timer_Id    is range 1 .. 3;
   type Timer_Array is array (Timer_Id) of Natural;

   type Handler_Access is not null access
     procedure (Object : in out Game_Object;
                Event  :        PIG_Event);

   procedure Null_Handler (Object : in out Game_Object;
                           Event  :        PIG_Event) is null;
   type Game_Engine_Class is access all Game_Engine'Class;
   type Object_Id    is new Natural;

   type Game_Object is record
      Owner : Game_Engine_Class;
--        PIG_object      *next, *prev;

      Id       : Object_Id;     -- Unique ID. 0 means "free".

      Ibase    : Sprite_Counts; -- Sprite frame base index
      Image    : Sprite_Counts; -- Sprite frame offset
      X, Y     : Float;         -- Position
      Vx, Vy   : Float;         -- Speed
      Ax, Ay   : Float;         -- Acceleration
      Ip       : PIG_Ipoint;
      Tilemask : Pig_Sides;     -- Sprite/tile mask [PIG_ALL]

      Hitmask  : Integer;       -- Sprite/sprite mask [0]
      Hitgroup : Integer;       -- Sprite/sprite group [0]

      Timer    : Timer_Array;   -- Down-counting timers
      Age      : Integer;       -- Age timer (logic frames)

      Score    : Natural;
      Power    : Integer;
      Target   : Integer;
      State    : Object_States;

      Handler  : Handler_Access;
   end record;

   subtype SDL_Surface   is SDL.Video.Surfaces.Surface;
   subtype SDL_Rectangle is SDL.Video.Rectangles.Rectangle;
   subtype SDL_Window    is SDL.Video.Windows.Window;

   --
   --  Level map
   --
   type Tile_Index is range 0 .. 255;
   type Map_Array  is array (Tiles range <>, Tiles range <>) of Tile_Index;
   type Hit_Array  is array (Tiles range <>, Tiles range <>) of Pig_Sides;
   type Hitinfo_Array is array (Tile_Index) of Pig_Sides;
   type Map_Array_Access is not null access Map_Array;
   type Hit_Array_Access is not null access Hit_Array;

   type PIG_Map is record
      Owner       : Game_Engine_Class;

      Width       : Tiles;             --  Size of map (tiles)
      Height      : Tiles;
      Map         : Map_Array_Access;  --  2D aray of tile indices
      Hit         : Hit_Array_Access;  --  2D aray of collision flags

      Tile_Width  : Pixels;            --  Size of one tile (pixels)
      Tile_Height : Pixels;
      Tile        : SDL_Surface;       --  Tile palette image
      Hitinfo     : Hitinfo_Array;     --  Collision info for the tiles
   end record;
   type PIG_Map_Access is access all PIG_Map;

   --  Sprite frame
   type PIG_Sprite is record
      Width, Height : Pixels;      --  Size of sprite (pixels)
      Hot_X, Hot_Y  : Pixels;      --  Hot-spot offset (pixels)
      Radius        : Pixels;      --  Collision zone radius (pixels)
      Surface       : SDL_Surface; --  Access
   end record;
   type PIG_Sprite_Access is access all PIG_Sprite;

   --
   --  Engine
   --
   package Object_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Object_Access);

   type Sprite_Array is array (Sprite_Index range <>) of PIG_Sprite_Access;
   type Dirty_Array  is array (0 .. 1) of Dirty.Table_Access;
   type Sprite_Array_Access is access Sprite_Array;

   type Game_Engine is
     new Ada.Finalization.Limited_Controlled
     with record
        Self    : Engine_Access;

        --  Video stuff
        Screen  : SDL_Surface;
        Buffer  : SDL_Surface;   --  For h/w surface displays
        Surface : SDL_Surface;   --  Where to render to
        Pages   : Integer;       --  # of display VRAM buffers
        View    : SDL_Rectangle; --  Viewport pos & size (pixels)

        --  Dirty
        Page      : Integer range 0 .. 1;      --  Current page (double buffer)
        Pagedirty : Dirty_Array;               --  One table for each page
        Workdirty : Dirty.Table_Access;        --  The work dirtytable

        --  "Live" switches
        Interpolation   : Boolean;
        Direct          : Boolean;    --  True: Render directly to screen
        Show_Dirtyrects : Boolean;

        --  Time
        Time  : Long_Float;           --  Logic time (frames)
        Frame : Integer;              --  Logic time; integer part

        --  Background graphics
        Map   : PIG_Map_Access;

        --  Objects
        Objects           : Object_Lists.List;
        Object_Id_Counter : Object_Id;

        --  Sprites
        Sprite_Last       : Sprite_Counts;
        Sprites           : Sprite_Array_Access;

        --  Space for user data
        --      Userdata : Long_Integer;
   end record;

   --
   --  Engine
   --
   overriding
   procedure Initialize (Engine : in out Game_Engine);

   overriding
   procedure Finalize (Engine : in out Game_Engine);

   procedure Before_Objects (Engine : in out Game_Engine) is null;
   procedure After_Objects  (Engine : in out Game_Engine) is null;
   --  Logic frame global handlers

   procedure Setup (Engine : in out Game_Engine;
                    Self   :        Engine_Access;
                    Screen :        SDL_Surface;
                    Pages  :        Positive);

   procedure Set_Viewport (Engine : in out Game_Engine'Class;
                           X, Y   :        Pixels;
                           Width  :        Pixels;
                           Height :        Pixels);
   --  Set viewport size and position

   procedure Pig_Start (Engine : in out Game_Engine;
                        Frame  :        Integer);
   --  Start engine at logic time 'frame'

   procedure Create_Sprites (Engine   : in out Game_Engine'Class;
                             Filename :        String;
                             Width    :        Pixels;
                             Height   :        Pixels;
                             Handle   :    out Sprite_Index);
   --  Load a sprite palette image. The image is chopped up into
   --  sprites, based on Width and Height, and added as new frames
   --  in the sprite bank. Default values:
   --       Hot-spot:               (Width / 2, Height / 2)
   --       Collision radius:       0.2 * (Width + Height)
   --
   --  Passing 0 for With and/or Height makes pig_sprites() take
   --  the respective value from the image width and/or height.
   --
   --  Returns the index of the first frame loaded.

   procedure Set_Hotspot (Engine : in out Game_Engine'Class;
                          Frame  :        Sprite_Index;
                          Hot_X  :        Pixels;
                          Hot_Y  :        Pixels);
   --  Set hot-spot of sprite 'frame' to (hotx, hoty)

   procedure Pig_Radius (Engine : in out Game_Engine;
                         Frame  :        Sprite_Index;
                         Radius :        Pixels);
   --  Set sprite/sprite collision zone radius of 'frame'

   procedure Pig_Animate (Engine : in out Game_Engine'Class;
                          Frames :        Float);
   --  Advance logic time by 'frames' logic frames

   procedure Pig_Dirty (Engine : in out Game_Engine;
                        Area   :        SDL_Rectangle);
   --  Manually add a dirtyrect for pig_refresh().
   --  Area can be outside the engine viewport.

   procedure Pig_Flip (Engine : in out Game_Engine;
                       Window : in out SDL_Window);
   --  Do what's needed to deal with the dirtyrects
   --  and then make the new frame visible.

   procedure Pig_Refresh (Engine : in out Game_Engine);
   --  Refresh the viewport and any additional dirtyrects.
   --
   --  Note that this does not refresh the entire viewport;
   --  only the areas that have actually changed!

   procedure Pig_Refresh_All (Engine : in out Game_Engine);
   --  Refresh the whole viewport, including sprites.

   procedure Clean_Object_List (Engine : in out Game_Engine);
   --  Remove objects marked for deletion.

   procedure Pig_Draw_Sprite (Engine : in out Game_Engine;
                              Frame  :        Sprite_Index;
                              X, Y   :        Pixels);
   --  Render a sprite "manually", bypassing the engine

   function Pig_Test_Map (Engine : Game_Engine;
                          X, Y   : Pixels) return Pig_Sides;
   --  Get the collision flags for the tile at (x, y),
   --  where the unit of x and y is pixels. The return
   --  is the PIG_sides flags for the tile, or PIG_NONE
   --  if (x, y) is outside the map.

   type PIG_Cinfo_Access is access all PIG_Cinfo;
   function Pig_Test_Map_Vector (Engine : in out Game_Engine;
                                 X1, Y1 :        Pixels;
                                 X2, Y2 :        Pixels;
                                 Mask   :        Pig_Sides;
                                 Ci     :        PIG_Cinfo_Access)
                                return Pig_Sides;
   --  Find the first "collidable" tile side when going from
   --  (x1, y1) to (x2, y2). 'mask' determines which tile sides
   --  are considered for collisions.
   --
   --  Returns the side(s) hit, if any tile was hit. If the return
   --  is non-zero, the PIG_cinfo struct at 'ci' contains detailed
   --  information about the collision.


   --
   --  Map
   --
   function Pig_Map_Open (Engine : Game_Engine_Class;
                          Width  : Tiles;
                          Height : Tiles)
                         return not null PIG_Map_Access;

   procedure Pig_Map_Close (Map : in out PIG_Map);

   procedure Pig_Map_Tiles (Map      : in out PIG_Map;
                            Filename :        String;
                            Width    :        Pixels;
                            Height   :        Pixels;
                            Result   :    out Integer);
   --  Load a tile palette image

   procedure Pig_Map_Collisions (Map   : in out PIG_Map;
                                 First :        Natural;
                                 Count :        Natural;
                                 Sides :        Pig_Sides);
   --  Set tile collision info for 'count' tiles, starting at
   --  'first'. Each tile in the tile palette has a set of
   --  PIG_sides flags that determine which sides the tile are
   --  considered for sprite/map collisions.

   procedure Pig_Map_From_String (Map   : in out PIG_Map;
                                  Trans :        String;
                                  Data  :        String);
   --  Load a map from a string (one byte/tile). 'trans'
   --  is a string used for translating 'data' into integer
   --  tile indices. Each position in 'trans' corresponds
   --  to one tile in the tile palette.


   --
   --  Object
   --
   function Open_Object (Engine : in out Game_Engine'Class;
                         X, Y   :        Pixels;
                         Last   :        Boolean)
                        return not null Object_Access;
   --  Create an object with the initial position (X, Y). If
   --  Last, the object will end up last in the
   --  processing and rendering order, otherwise, first.
   --
   --  Note that relative processing order is very important
   --  to objects that chase each other and stuff like that!
   --  If they're placed in the "wrong" order, the tracking
   --  objects get an extra frame of reaction time, which is
   --  annoying if it's not what you intend.

   procedure Unlink_Object (Object : in out Game_Object);
   --  Unlink an object.
   --
   --  Note that objects are never actually deleted. Instead,
   --  they are placed in a free pool, where pig_object_open()
   --  looks for objects to recycle.
   --
   --  In fact, they are not even freed when you ask for it,
   --  but rather kept around until the next rendered frame,
   --  so they can be removed from the screen correctly.

   procedure Unlink_All_Objects (Engine : in out Game_Engine);
   --  Unlink all objects.

   function Find_Object (Start : in out Game_Object;
                         Id    :        Object_Id) return Object_Access;
   --  Find object by 'id', starting at object 'start'.
   --
   --  The search starts at 'start' and is done in both
   --  directions in parallel, assuming that the matching
   --  object is near 'start' in the list. (It usually is
   --  when dealing with linked objects.)
   --
   --  Returns NULL if the object was not found.

end Engines;
