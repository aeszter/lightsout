package body Twins is

   ----------
   -- Init --
   ----------

   procedure Init (What : in out Twin) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Init unimplemented");
      raise Program_Error;
   end Init;

   -------------
   -- Set_PDU --
   -------------

   procedure Set_PDU (Where : in out Twin; Str : String) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Set_PDU unimplemented");
      raise Program_Error;
   end Set_PDU;

   --------------
   -- Add_Host --
   --------------

   procedure Add_Host (Where : in out Twin; Host : String) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Add_Host unimplemented");
      raise Program_Error;
   end Add_Host;

   overriding procedure Poweron (What : Twin) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Poweron unimplemented");
      raise Program_Error;
   end Poweron;

   overriding procedure Poweroff (What : Twin) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Poweroff unimplemented");
      raise Program_Error;
   end Poweroff;

   overriding procedure Enable (What : Twin) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Enable unimplemented");
      raise Program_Error;
   end Enable;

   overriding procedure Disable (What : Twin) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Disable unimplemented");
      raise Program_Error;
   end Disable;

end Twins;
