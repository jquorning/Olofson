--------------------------------------------------------------
--  Fixed Rate Pig - a fixed logic frame rate demo
--------------------------------------------------------------
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

package body Signals is

   protected body Process_Control is


      function Is_Running return Boolean
      is (Running);


      procedure Handle_Signal is
      begin
         Running := False;
      end Handle_Signal;


   end Process_Control;

end Signals;
