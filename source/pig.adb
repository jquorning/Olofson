--------------------------------------------------------------
--  Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2004 David Olofson <david@olofson.net>
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Command_Line;
with Ada.Text_IO;

with Games;

procedure Pig is
   use Games;

   procedure Put_Usage;


   procedure Put_Usage is
      use Ada.Text_IO;
   begin
      Put_Line ("The Fixed Rate Pig Game 0.1.0");
      New_Line;
      Put_Line ("Usage: pig [-s] [-f] [BPP] [--help|--version]");
      Put_Line ("       -s  : Double buffer        ");
      Put_Line ("       -f  : Full screen          ");
      Put_Line ("       BPP : Bit per pixel (ex 16)");
   end Put_Usage;

   BPP           : Positive := 1;
   Double_Buffer : Boolean  := False;
   Full_Screen   : Boolean  := False;
begin

   Process_Command_Line :
   declare
      use Ada.Command_Line;
   begin
      for Index in 1 .. Argument_Count loop
         declare
            Switch : String renames Argument (Index);
         begin
            if    Switch = "-s" then  Double_Buffer := False;
            elsif Switch = "-f" then  Full_Screen   := True;
            elsif Switch = "--help"
              or  Switch = "--version"
            then
               Put_Usage;
               return;
            else
               BPP := Integer'Value (Switch);
            end if;
         end;
      end loop;
   exception
      when Constraint_Error =>  Put_Usage;  return;
   end Process_Command_Line;

   Play_Game (Double_Buffer, Full_Screen, BPP);

end Pig;
