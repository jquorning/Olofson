
with GNAT.Command_Line;

with Ballfield;

procedure Main is
   use GNAT.Command_Line;
   use Ballfield;

   Config : Command_Line_Configuration;
begin
   Set_Usage (Config, "[options]", "Ballfield SDL2 demo");

   Define_Switch (Config, Alpha'Access,         "-a:", Help => "Alpha (0..255)");
   Define_Switch (Config, Double_Buffer'Access, "-d",  Help => "Double buffer");
   Define_Switch (Config, Full_Screen'Access,   "-f",  Help => "Full screen");
   Define_Switch (Config, HW_Surface'Access,    "-s",  Help => "HW surface");

   Getopt (Config);

   if Alpha in 0 .. 255 then
      Ballfield.Main;
   else
      Display_Help (Config);
   end if;

exception
   when
     Invalid_Parameter |
     Invalid_Switch    |
     Exit_From_Command_Line
     =>
      Display_Help (Config);
end Main;
