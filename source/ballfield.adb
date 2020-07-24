--
--  "Ballfield"
--
--  (C) David Olofson <david@olofson.net>, 2002, 2003
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Text_IO;

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;

with SDL.Video.Windows.Makers;

with SDL.Images.IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;

with SDL.Video.Palettes;

package body Ballfield is

   use SDL.Video.Surfaces;

--   procedure Clean_Alpha (Work : out Surface;
--                          S    :     Surface);
   --  Bump areas of low and high alpha to 0% or 100%
   --  respectively, just in case the graphics contains
   --  "alpha noise".

   procedure Load_Zoomed (Sprites   : out Surface;
                          File_Name :     String;
                          Alpha     :     Boolean);
   --  Load and convert an antialiazed, zoomed set of sprites.

   procedure Print_Number (Destin : in out Surface;
                           Font   :        Surface;
                           X, Y   :        Integer;
                           Value  :        String);
   --  Render Value to position (X, Y) on Destin surface with Font.
   --  Value elements must be in "0..9.,-" otherwise element is not output.

   procedure Ballfield_Init (Field : out Ballfield_Type);
   procedure Ballfield_Free (Field : in out Ballfield_Type);
   procedure Ballfield_Init_Frames (Field : in out Ballfield_Type);
   procedure Ballfield_Load_Gfx (Field     : in out Ballfield_Type;
                                 File_Name :        String;
                                 Color     :        Color_Type);
   procedure Ballfield_Move (Field      : in out Ballfield_Type;
                             Dx, Dy, Dz :        Rel_Coord_Type);
   procedure Ballfield_Render (Field  : in out Ballfield_Type;
                               Screen : in out Surface);

   procedure Tiled_Back (Back   :        Surface;
                         Screen : in out Surface;
                         Xo, Yo :        Integer);
   --  Draw tiled background image with offset.

   -----------------
   -- Clean_Alpha --
   -----------------

--     procedure Clean_Alpha (Work : out surface;
--                            S    :     Surface)
--     is
--        use SDL.Video.Pixel_Formats;
--        subtype Unsigned_32 is Interfaces.Unsigned_32;
--        use type SDL.Video.Pixel_Formats.C.int;
--        --    SDL_Surface *work;
--        Pixels : Natural; --    Uint32*
--        Pp     : Integer;
--        X, Y   : Integer;
--        Size : constant SDL.Sizes := S.Size;
--     begin
--        SDL.Video.Surfaces.Makers.Create (Work,
--                                          -- SDL_SWSURFACE,
--                                          (S.Size.Width, S.Size.Height), 32,
--                                          16#FF_00_00_00#, 16#00_FF_00_00#,
--                                          16#00_00_FF_00#, 16#00_00_00_FF#);
--  --      Work := SDL_CreateRGBSurface
--  (SDL_SWSURFACE, S.Size.Width, S.Size.Height, 32,
--  --                                    16#Ff_00_00_00#, 16#00_FF_00_00#,
--  --                                    16#00_00_FF_00#, 16#00_00_00_FF#);

--        --    if(!work)
--  --            return NULL;

--        declare
--           use SDL.Video.Rectangles;
--           R  : Rectangle := (0, 0, S.Size.Width, S.Size.Height);
--           SA : Rectangle := (0,0,0,0);
--        begin
--           Work.Blit (R, S, SA);
--        end;
--  --              if(SDL_BlitSurface(s, &r, work, NULL) < 0)
--  --      {
--  --              SDL_FreeSurface(work);
--  --              return NULL;
--  --      end if;

--        Work.Lock;

--        Pixels := Work.Pixels;
--        Pp := Work.Pitch / (Unsigned_32'Size / 8);

--        for Y in 0 .. Work.Size.Height - 1 loop
--           for X in 0 .. Work.Size.Width - 1 loop

--              declare
--                 Pix : Unsigned_32 := Pixels (Y * pp + X);
--              begin
--                 case Pix mod 16#100# / 2**4 is
--                    when 0      =>  pix := 16#00000000#;
--                    when others =>  null;
--                    when 15     =>  Pix := Pix or 16#FF#;
--                 end case;

--                 Pixels (Y * pp + X) := Pix;
--              end;
--           end loop;
--        end loop;

--        Work.Unlock;

--        --return work;
--     end Clean_Alpha;

   -----------------
   -- Load_Zoomed --
   -----------------

   procedure Load_Zoomed (Sprites   : out Surface;
                          File_Name :     String;
                          Alpha     :     Boolean)
   is
--      Sprites : SDL.Video.Surfaces.Surface;
--      Temp    : SDL.Video.Surfaces.Surface;
   begin
--      SDL.Images.IO.Create (Temp, File_Name);
      SDL.Images.IO.Create (Sprites, File_Name);


--      Sprites := Temp;

      --  SDL.Video.Textures.Set_Alpha (Sprites, 200);  -- SDL_RLEACCEL, 255);
--      Clean_Alpha (Temp, Sprites);

      --      Sprites.Finalize; -- SDL_FreeSurface (Sprites);

--      if(!temp)
--      {
--              fprintf(stderr, "Could not clean alpha!\n");
--              return NULL;
--      }

      if Alpha then
         null;
         --  SDL.Video.Textures.Set_Alpha (Temp, 0);
         --  SDL_SRCALPHA or SDL_RLEACCEL, 0);
         --  Sprites := SDL_DisplayFormatAlpha (Temp);
      else
         null;
         --           SDL.Video.Surfaces.Set_Colour_Key
--             (Temp, -- SDL_SRCCOLORKEY or SDL_RLEACCEL,
--              Now    => SDL.Video.Pixel_Formats.To_Pixel
--  (Temp.Pixel_Format,0,0,0),
--              --SDL_MapRGB (Temp.format, 0, 0, 0),
--              Enable => True);
--           --Sprites := SDL_DisplayFormat (Temp);
      end if;
      --  SDL_FreeSurface(temp);

      --  return Sprites;
   end Load_Zoomed;

   ------------------
   -- Print_Number --
   ------------------

   procedure Print_Number (Destin : in out Surface;
                           Font   :        Surface;
                           X, Y   :        Integer;
                           Value  :        String)
   is
      use SDL.Video.Rectangles, SDL.C;
      Character_Width  : constant int := 7;
      Character_Height : constant int := 10;

      subtype Character_Index is Integer range 0 .. 11;
      Character_Zero   : constant Character_Index := 0;
      Character_Minus  : constant Character_Index := 10;
      Character_Point  : constant Character_Index := 11;
      Char             : Character_Index;
      Good_Character   : Boolean;

      From : Rectangle := (0, 0,
                           Width  => Character_Width,
                           Height => Character_Height);
      To   : Rectangle;
   begin
      for Pos in Value'Range loop

         Good_Character := True;
         case Value (Pos) is
            when '0' .. '9' => Char := (Character_Zero
                                          + Character'Pos (Value (Pos))
                                          - Character'Pos ('0'));
            when '-'        => Char := Character_Minus;
            when '.' | ','  => Char := Character_Point;
            when others     => Good_Character := False;
         end case;

         if Good_Character then

            From.X := int (Char) * Character_Width;

            To.X := int (X) + int (Pos - Value'First) * Character_Width;
            To.Y := int (Y);

            Destin.Blit (Source      => Font,
                         Source_Area => From,
                         Self_Area   => To);
         end if;
      end loop;

   end Print_Number;

   ----------------
   -- Initialize --
   ----------------

   procedure Ballfield_Init (Field : out Ballfield_Type) is

      package Random_Coords
      is new Ada.Numerics.Discrete_Random (Coord_Type);

      type Percentage is range 0 .. 99;
      package Random_Percentage
      is new Ada.Numerics.Discrete_Random (Percentage);

      Coord_Generator   : Random_Coords.Generator;
      Percent_Generator : Random_Percentage.Generator;
      function Random_Coord   return Coord_Type is (Random_Coords.Random (Coord_Generator));
      function Random_Percent return Percentage is (Random_Percentage.Random (Percent_Generator));
   begin
      Random_Coords.Reset (Coord_Generator);

      for I in Ball_Index loop

         Field.Points (I) :=
           (X     => Random_Coord,
            Y     => Random_Coord,
            Z     => Coord_Type'Last * Coord_Type (I) / Ball_Index'Modulus,
            Color => (if Random_Percent > 80 then Red else Blue));

      end loop;

   end Ballfield_Init;

   ----------
   -- Free --
   ----------

   procedure Ballfield_Free (Field : in out Ballfield_Type)
   is
      pragma Unreferenced (Field);
   begin
      for I in Color_Type loop
         null; --  Bf.Gfx (I).Finalize;
      end loop;
   end Ballfield_Free;

   -----------------------
   -- Initialize_Frames --
   -----------------------

   procedure Ballfield_Init_Frames (Field : in out Ballfield_Type)
   is
      use SDL.C;

      J     : int          := 0;
      Width : constant int := Field.Gfx (Blue).Size.Width;

      subtype Index_Range is Frame_Index
      range 0 .. Frame_Index (Width) - 1;

   begin
      --
      --  Set up source rects for all frames
      --
      Field.Frames := new Rectangle_Array'(Index_Range => <>);

      for I in Index_Range loop

         Field.Frames (I) := (X      => 0,
                           Y      => J,
                           Width  => Width - int (I),
                           Height => Width - int (I));

         J := J + Width - int (I);
      end loop;

   end Ballfield_Init_Frames;

   --------------
   -- Load_Gfx --
   --------------

   procedure Ballfield_Load_Gfx (Field     : in out Ballfield_Type;
                                 File_Name :        String;
                                 Color     :        Color_Type) is
   begin
      Load_Zoomed (Field.Gfx (Color),
                   File_Name => File_Name,
                   Alpha     => Field.Use_Alpha);
   end Ballfield_Load_Gfx;

   ----------
   -- Move --
   ----------

   procedure Ballfield_Move (Field      : in out Ballfield_Type;
                             Dx, Dy, Dz :        Rel_Coord_Type) is
   begin
      for Point of Field.Points loop
         Point := (X     => (Point.X + Dx) mod Coord_High,
                   Y     => (Point.Y + Dy) mod Coord_High,
                   Z     => (Point.Z + Dz) mod Coord_High,
                   Color => Point.Color);
      end loop;
   end Ballfield_Move;

   ------------
   -- Render --
   ------------

   procedure Ballfield_Render (Field     : in out Ballfield_Type;
                               Screen : in out Surface)
   is

      function Find_Z_Maximum return Ball_Index;
      --  Find the ball with the highest Z.

      function Find_Z_Maximum return Ball_Index is
         J_High : Ball_Index := 0;
         Z_High : Coord_Type := 0;
      begin
         for Index in Ball_Index loop
            if Field.Points (Index).Z > Z_High then
               J_High := Index;
               Z_High := Field.Points (Index).Z;
            end if;
         end loop;
         return J_High;
      end Find_Z_Maximum;

      J : Ball_Index := Find_Z_Maximum;
   begin

      --  Render all balls in back->front order.
      for Index in Ball_Index loop
         declare
            use SDL.C;

            Ball_Width : constant Integer := Integer (Field.Frames (0).Width);
            Frame : Frame_Index;
            FI    : Integer;
            Z     : Integer;
         begin
            Z := Integer (Field.Points (J).Z) + 50;

            FI := ((Ball_Width / 2**12) + 100000) / Z;
            FI := Ball_Width - FI;
            FI := Integer'Max (0, FI);
            FI := Integer'Min (FI, Ball_Width - 1);
            Frame := Frame_Index (FI);

            Z := Z / 2**7 + 1;

            declare
               use SDL.Video.Rectangles;

               X_Some : constant int := int (Field.Points (J).X - Coord_Center) / int (Z);
               Y_Some : constant int := int (Field.Points (J).Y - Coord_Center) / int (Z);
               X_Half : constant int := (Screen.Size.Width  - Field.Frames (Frame).Width) / 2;
               Y_Half : constant int := (Screen.Size.Height - Field.Frames (Frame).Height) / 2;

               Rect   : Rectangle    := (X      => X_Some + X_Half,
                                         Y      => Y_Some + Y_Half,
                                         others => 0);
            begin
               Screen.Blit (Source      => Field.Gfx (Field.Points (J).Color),
                            Source_Area => Field.Frames (Frame),
                            Self_Area   => Rect);
            end;

            J := J - 1;  -- Modulus type
         end;
      end loop;
   end Ballfield_Render;

   -------------------------------
   -- Other rendering functions --
   -------------------------------

   ---------------
   -- Tile_Back --
   ---------------

   procedure Tiled_Back (Back   :        Surface;
                         Screen : in out Surface;
                         Xo, Yo :        Integer)
   is
      use SDL.C;
      Width  : constant Natural := Natural (Back.Size.Width);
      Height : constant Natural := Natural (Back.Size.Height);

      Xoc    : constant Natural :=
        (Xo + Width  * ((-Xo) / Width + 1))  mod Width;

      Yoc    : constant Natural :=
        (Yo + Height * ((-Yo) / Height + 1)) mod Height;

      X, Y : Integer;
   begin
      Y  := -Yoc;
      while Y < Integer (Screen.Size.Height) loop
         X := -Xoc;
         while X < Integer (Screen.Size.Width) loop
            declare
               R  : SDL.Video.Rectangles.Rectangle := (int (X), int (Y), 0, 0);
               SA : SDL.Video.Rectangles.Rectangle := (0, 0, 0, 0);
            begin
               Screen.Blit (Source      => Back,
                            Source_Area => SA,
                            Self_Area   => R);
            end;
            X := X + Width;
         end loop;
         Y := Y + Height;
      end loop;

   end Tiled_Back;

   ----------
   -- Main --
   ----------

   procedure Main is

      use SDL.Video.Windows;

      Balls  : Ballfield_Type;
      Window : SDL.Video.Windows.Window;

      Screen : Surface;
      Back   : Surface;
      Logo   : Surface;
      Font   : Surface;
--      Temp   : Surface;

      Flags     : constant Window_Flags := (if Full_Screen
                                              then SDL.Video.Windows.Full_Screen
                                              else 0);

--      flags  : Integer := SDL_DOUBLEBUF or SDL_SWSURFACE;
      Alpha  : constant Boolean := True;
   begin
      --  Look like Audio has to be enabled otherwise crash at shutdown
      if not SDL.Initialise (SDL.Enable_Audio) then
         raise Program_Error with "Could not initialise SDL";
      end if;

      SDL.Video.Windows.Makers.Create (Window,
                                       Title    => "Ballfield",
                                       Position => (100, 100),
                                       Size     => (SCREEN_W, SCREEN_H),
                                       Flags    => Flags);
      Screen := Window.Get_Surface;

--      if(flags & SDL_FULLSCREEN)
--              SDL_ShowCursor(0);

      Ballfield_Init (Balls);

      --  Load and prepare balls...
      Balls.Use_Alpha := Alpha;

      Ballfield_Load_Gfx (Balls, Assets_Path & "blueball.png", Blue);
      Ballfield_Load_Gfx (Balls, Assets_Path & "redball.png",  Red);
      Ballfield_Init_Frames (Balls);

      --  Load background image
      SDL.Images.IO.Create (Back, Assets_Path & "redbluestars.png");
--      Back := SDL_DisplayFormat(temp_image);

      --  Load logo
      SDL.Images.IO.Create (Logo, Assets_Path & "logo.bmp");
      SDL.Video.Surfaces.Set_Colour_Key
        (Self => Logo,
         Now  => SDL.Video.Palettes.Colour'(Red => 255, Green => <>, Blue => 255, Alpha => <>));
      --  Logo := SDL.Video.Surfaces.Helpers.Convert (Temp, Format => Screen.Pixel_Format);

      --  Load font
      SDL.Images.IO.Create (Font, Assets_Path & "font7x10.bmp");
      SDL.Video.Surfaces.Set_Colour_Key
        (Self => Font,
         Now  => SDL.Video.Palettes.Colour'(Red => 255, Green => <>, Blue => 255, Alpha => <>));
--      font := SDL_DisplayFormat (temp_image);

      Dynamic :
      declare
         package FPS_IO is new Ada.Text_IO.Float_IO (Num => Float);
         Number : String (1 .. 5);

         X_Offs : Integer := 0;
         Y_Offs : Integer := 0;

         Tick      : Ada.Real_Time.Time;
         Last_Tick : Ada.Real_Time.Time := Ada.Real_Time.Clock;

         Time       : Long_Float := 0.000;
         Delta_Time : Duration;

         FPS_Count : Integer := 0;
         FPS_Start : Ada.Real_Time.Time := Ada.Real_Time.Clock;
         FPS       : Float := 0.0;
      begin
         loop

            --  Events
            declare
               use SDL.Events;
               use type SDL.Events.Keyboards.Key_Codes;

               Event : SDL.Events.Events.Events;
            begin
               if Events.Poll (Event) then
                  exit when Event.Common.Event_Type = Quit;
                  exit when Event.Common.Event_Type = Keyboards.Key_Down and
                    Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape;
               end if;
            end;

            --  Timing
            declare
               use Ada.Real_Time;
            begin
               Tick       := Ada.Real_Time.Clock;
               Delta_Time := To_Duration (Tick - Last_Tick);
               Last_Tick  := Tick;
            end;

            --  Background image
            Tiled_Back (Back, Screen, X_Offs / 2**11, Y_Offs / 2**11);

            --  Ballfield
            Ballfield_Render (Balls, Screen);

            --  Logo
            declare
               use SDL.Video.Rectangles;
               Destin_Area : Rectangle := (2, 2, 0, 0);
               Source_Area : Rectangle := (0, 0, 0, 0);
            begin
               Screen.Blit (Source      => Logo,
                            Source_Area => Source_Area,
                            Self_Area   => Destin_Area);
            end;

            --  FPS counter
            declare
               use Ada.Real_Time;
            begin
               if Tick > FPS_Start + Milliseconds (500) then
                  FPS       := (Float (FPS_Count)
                                  / Float (To_Duration (Tick - FPS_Start)));
                  FPS_Count := 0;
                  FPS_Start := Tick;
               end if;
               FPS_IO.Put (Number, FPS, Exp => 0, Aft => 1);
               Print_Number (Screen, Font,
                             X     => Integer (Screen.Size.Width) - 37,
                             Y     => Integer (Screen.Size.Height) - 12,
                             Value => Number);

               FPS_Count := FPS_Count + 1;
            end;

            --  Update
            Window.Update_Surface;

            --  Animate
            declare
               use Ada.Numerics.Elementary_Functions;

               FT      : constant Float := Float (Time);
               X_Speed : constant Float := 500.0 * Sin (FT * 0.37);
               Y_Speed : constant Float := 500.0 * Sin (FT * 0.53);
               Z_Speed : constant Float := 400.0 * Sin (FT * 0.21);
            begin
               Ballfield_Move (Balls,
                               Rel_Coord_Type (X_Speed),
                               Rel_Coord_Type (Y_Speed),
                               Rel_Coord_Type (Z_Speed));

               X_Offs := X_Offs - Integer (X_Speed);
               Y_Offs := Y_Offs - Integer (Y_Speed);
            end;

            Time := Time + Long_Float (Delta_Time);
         end loop;
      end Dynamic;

      Ballfield_Free (Balls);

      SDL.Finalise;
   end Main;

end Ballfield;
