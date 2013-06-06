with Nodes;

package Actions is
   procedure Enable (What : Nodes.Node);
   procedure Disable (What : Nodes.Node);
   procedure Poweron (What : Nodes.Node);
   procedure Poweroff (What : Nodes.Node);
   procedure Powercycle (What : Nodes.Node);

   Subcommand_Error : exception;
private

end Actions;
