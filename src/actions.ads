with Nodes;
with Twins;

package Actions is
   procedure Enable (What : Nodes.Node);
   procedure Disable (What : Nodes.Node);
   procedure Poweron (What : Nodes.Node);
   procedure Poweron (PDU : Twins.PDU_String);
   procedure Poweroff (What : Nodes.Node);
   procedure Poweroff (PDU : Twins.PDU_String);
   procedure Powercycle (What : Nodes.Node);

   Subcommand_Error : exception;
private

end Actions;
