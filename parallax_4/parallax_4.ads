--
--  "Parallax Scrolling IV - Overdraw Elimination"
--
--   Nghia             <nho@optushome.com.au>
--   Randi J. Relander <rjrelander@users.sourceforge.net>
--   David Olofson     <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact authors for permission if you want to use this
--  software, or work derived from it, under other terms.

with SDL.Video.Surfaces;
with SDL.Video.Rectangles;

package Parallax_4 is


   --  Definitions...

   --  foreground and background velocities in pixels/sec
   FOREGROUND_VEL_X : constant := 100.0;
   FOREGROUND_VEL_Y : constant :=  50.0;

   BACKGROUND_VEL   : constant := 100.0;

   --  Size of the screen in pixels
   SCREEN_W : constant := 320;
   SCREEN_H : constant := 240;

   --  Size of one background tile in pixels
   TILE_W : constant := 48;
   TILE_H : constant := 48;

   --  The maps are 16 by 16 squares, and hold one
   --  character per square. The characters determine
   --  which tiles are to be drawn in the corresponding
   --  squares on the screen. Space (" ") means that
   --  no tile will be drawn.
   MAP_W : constant := 16;
   MAP_H : constant := 16;

   type Map_X_Type is new Integer;
   type Map_Y_Type is new Integer;
   type Tile_Raw_Type is new Character;

   type Map_Data_Type
     is array (Map_Y_Type range 0 .. MAP_H - 1,
               Map_X_Type range 0 .. MAP_W - 1) of Tile_Raw_Type;

   type Map_Data_Access is access all Map_Data_Type;

   type Flag_Type is
      record
         Linked       : Boolean;
         Limit_Bounce : Boolean;
      end record;

   subtype Surface   is SDL.Video.Surfaces.Surface;
   subtype Rectangle is SDL.Video.Rectangles.Rectangle;

   type Layer_Type;
   type Layer_Access is access all Layer_Type;

   type Layer_Type is
      record
        --  Next layer in recursion order
        Next : Layer_Access;

        --  Position and speed
        Pos_X, Pos_Y : Float;
        Vel_X, Vel_Y : Float;

        --  Various flags
        Flags : Flag_Type;

        --  Map and tile data
        Map          : Map_Data_Access;
        Tiles        : Surface;
        Opaque_Tiles : Surface;

        --  Position link
        Link : Layer_Access;
        Ratio : Float;

        --  Statistics
        Calls      : Integer;
        Blits      : Integer;
        Recursions : Integer;
        Pixels     : Integer;
      end record;

   --  Flag definitions
   TL_LIMIT_BOUNCE : constant := 16#00000001#;
   TL_LINKED       : constant := 16#00000002#;

   procedure Layer_Init (Layer        : out Layer_Type;
                         Map          :     Map_Data_Access;
                         Tiles        :     Surface;
                         Opaque_Tiles :     Surface);
   --  Initialize layer; set up map and tile graphics data.

   procedure Layer_Next (Layer      : in out Layer_Type;
                         Next_Layer :        Layer_Access);
   --  Tell a layer which layer is next, or under this layer.

   procedure Layer_Pos (Layer : in out Layer_Type;
                        X, Y  :        Float);
   --  Set position.

   procedure Layer_Vel (Layer : in out Layer_Type;
                        X, Y  :        Float);
   --  Set velocity.

   procedure Layer_Animate (Layer : in out Layer_Type;
                            DT    :        Float);
   --  Update animation (apply the velocity, that is).

   procedure Layer_Limit_Bounce (Layer : in out Layer_Type);
   --  Bounce at map limits.

   procedure Layer_Link (Layer    : in out Layer_Type;
                         To_Layer :        Layer_Access;
                         Ratio    :        Float);
   --  Link the position of this layer to another layer, w/ scale ratio.

   procedure Layer_Render (Layer  : in out Layer_Type;
                           Screen : in out Surface;
                           Rect   :        Rectangle);
   --  Render layer to the specified surface,
   --  clipping to the specified rectangle.

   procedure Layer_Reset_Stats (Layer : in out Layer_Type);

   procedure Main;

   --  Command line options
   Verbose       : aliased Integer;
   No_Planets    : aliased Boolean;
   Num_Of_Layers : aliased Integer;
   Bounce_Around : aliased Boolean;
   Wrap          : aliased Boolean;
   Alpha         : aliased Integer;
   Double_Buffer : aliased Boolean;
   Full_Screen   : aliased Boolean;

end Parallax_4;
