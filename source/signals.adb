--
--
--

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
