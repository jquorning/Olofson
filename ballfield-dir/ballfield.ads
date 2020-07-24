--
--  "Ballfield"
--
--  (C) David Olofson <david@olofson.net>, 2002
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with SDL.Video.Surfaces;
with SDL.Video.Rectangles;

package Ballfield is

   Assets_Path : constant String := "ballfield-dir/assets/";

   procedure Main;

   --  Definitions
   type Ball_Index is mod 1_000;
   type Color_Type is (Blue, Red);

   type Base_Coord_Type   is new Integer;
   Coord_High   : constant := 16#20000#;
   Coord_Center : constant := Coord_High / 2;
   subtype Coord_Type     is Base_Coord_Type range 0 .. Coord_High - 1;
   subtype Rel_Coord_Type is Base_Coord_Type range -500 .. 500;

   type Point_Type is
      record
         X, Y, Z : Coord_Type;
         Color   : Color_Type;
      end record;

   --  Ballfield
   type Point_Array is array (Ball_Index) of Point_Type;

   type Surface_Array
   is array (Color_Type)
   of SDL.Video.Surfaces.Surface;

   type Frame_Index is new Natural;

   type Rectangle_Array
   is array (Frame_Index range <>)
   of SDL.Video.Rectangles.Rectangle;

   type Frame_Array is access all Rectangle_Array;

   type Ballfield_Type is
      record
        Points    : Point_Array;
        Frames    : Frame_Array;
        Gfx       : Surface_Array;
        Use_Alpha : Boolean;
      end record;

   --  Size of the screen in pixels
   SCREEN_W : constant := 800;
   SCREEN_H : constant := 600;

   --  Size of the biggest ball image in pixels
   --
   --  Balls are scaled down and *packed*, one pixel
   --  smaller for each frame down to 1x1. The actual
   --  image width is (obviously...) the same as the
   --  width of the first frame.
   BALL_W : constant := 32;
   BALL_H : constant := 32;

      --  Command line options
   Double_Buffer : aliased Boolean;
   Full_Screen   : aliased Boolean;
   Alpha         : aliased Integer;
   HW_Surface    : aliased Boolean;

end Ballfield;
