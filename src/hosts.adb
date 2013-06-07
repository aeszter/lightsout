with Actions;

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
      Node (What).Set_Name (Name);
      Node (What).Set_Maintenance (Maintain);
      Node (What).Set_Bug (Bug);
   end Init;

   overriding procedure Poweron (What : Host) is
   begin
      Actions.Poweron (Node (What));
   end Poweron;

   overriding procedure Poweroff (What : Host) is
   begin
      Actions.Poweroff (Node (What));
   end Poweroff;

   overriding procedure Enable (What : Host) is
   begin
      Actions.Enable (Node (What));
   end Enable;

   overriding procedure Disable (What : Host) is
   begin
      Actions.Disable (Node (What));
   end Disable;

end Hosts;
