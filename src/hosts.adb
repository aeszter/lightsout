package body Hosts is

   ----------
   -- Init --
   ----------

   procedure Init
     (What     : out Host;
      Name     : String;
      Maintain : Maintenance;
      Bug      : Natural)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Init unimplemented");
      raise Program_Error;
   end Init;

   overriding procedure Poweron (What : Host) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Poweron unimplemented");
      raise Program_Error;
   end Poweron;

   overriding procedure Poweroff (What : Host) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Poweroff unimplemented");
      raise Program_Error;
   end Poweroff;

   overriding procedure Enable (What : Host) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Enable unimplemented");
      raise Program_Error;
   end Enable;

   overriding procedure Disable (What : Host) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Enable unimplemented");
      raise Program_Error;
   end Disable;

end Hosts;
