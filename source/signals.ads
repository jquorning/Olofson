--------------------------------------------------------------
--        Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  Copyright (C) 2020 Jesper Quorning
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Interrupts.Names;

package Signals is
   pragma Unreserve_All_Interrupts;

   protected Process_Control is

      function Is_Running return Boolean;

      procedure Handle_Signal;

      pragma Attach_Handler (Handle_Signal, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler (Handle_Signal, Ada.Interrupts.Names.SIGHUP);
      pragma Attach_Handler (Handle_Signal, Ada.Interrupts.Names.SIGTERM);
      --  Statically attach handlers to the SIGINT, SIGHUP and SIGTERM
      --  interrupts.
   private
      Running : Boolean := True;
   end Process_Control;

end Signals;
