with Nodes;

package Actions is
   procedure Enable (What : Nodes.Node);
   procedure Disable (What : Nodes.Node);
   procedure Poweron (What : Nodes.Node);
   procedure Poweroff (What : Nodes.Node);
   procedure Powercycle (What : Nodes.Node);
   procedure Try_To_Poweroff (The_Node : Nodes.Node; Succeeded : out Boolean);
   -- Given an online and idle node, disable it, check for idleness (again),
   -- then switch it off (if still idle), otherwise just enable it again
   -- and notify caller of failure

   Subcommand_Error : exception;
private

end Actions;
