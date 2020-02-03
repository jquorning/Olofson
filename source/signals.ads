--
--
--

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
