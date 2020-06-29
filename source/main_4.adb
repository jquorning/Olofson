
with GNAT.Command_Line;

with Parallax_4;

procedure Main_4 is
   use GNAT.Command_Line;
   use Parallax_4;

   Config : Command_Line_Configuration;
begin

   Set_Usage (Config, "[options]", "Parallax demo");

   Define_Switch (Config, Verbose'Access,       "-v:", Help => "Verbose (0..3)");
   Define_Switch (Config, Alpha'Access,         "-a:", Help => "Alpha (0..255)");
   Define_Switch (Config, Bounce_Around'Access, "-b",  Help => "Bounce around");
   Define_Switch (Config, Wrap'Access,          "-w",  Help => "Wrap");
   Define_Switch (Config, Num_Of_Layers'Access, "-l:", Help => "Number of layers (1..X)",
                  Initial => 7);
   Define_Switch (Config, No_Planets'Access,    "-n",  Help => "No planets");
   Define_Switch (Config, Double_Buffer'Access, "-d",  Help => "Double buffer");
   Define_Switch (Config, Full_Screen'Access,   "-f",  Help => "Full screen");

   Getopt (Config);

   if
     Num_Of_Layers >= 1 and
     Verbose in 0 .. 3  and
     Alpha   in 0 .. 255
   then
      Parallax_4.Main;
   else
      Display_Help (Config);
   end if;

exception
   when Invalid_Parameter =>
      Display_Help (Config);
end Main_4;
