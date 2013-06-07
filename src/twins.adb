with Actions;
with Hosts;

package body Twins is

   ----------
   -- Init --
   ----------

   procedure Init (What : in out Twin) is
   begin
      What.Sub_Nodes.Clear;
      What.PDU := PDU_Strings.To_Bounded_String ("undefined");
   end Init;

   -------------
   -- Set_PDU --
   -------------

   procedure Set_PDU (Where : in out Twin; Str : String) is
   begin
      Where.PDU := PDU_Strings.To_Bounded_String (Str);
   end Set_PDU;

   --------------
   -- Add_Host --
   --------------

   procedure Add_Host (Where : in out Twin; Host : String) is
      Sub_Node : Hosts.Host;
   begin
      Sub_Node.Init (Name     => Host,
                     Maintain => none,
                     Bug      => 0);
      Where.Sub_Nodes.Append (+Sub_Node);
   end Add_Host;

   overriding procedure Poweron (What : Twin) is
   begin
      Actions.Poweron (What.PDU);
   end Poweron;

   overriding procedure Poweroff (What : Twin) is
   begin
      Actions.Poweroff (What.PDU);
   end Poweroff;

   overriding procedure Enable (What : Twin) is
   begin
      What.Sub_Nodes.Iterate (Nodes.Enable'Access);
   end Enable;

   overriding procedure Disable (What : Twin) is
   begin
      What.Sub_Nodes.Iterate (Nodes.Disable'Access);
   end Disable;

end Twins;
