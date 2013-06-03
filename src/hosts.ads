with Nodes; use Nodes;

package Hosts is
   type Host is new Node with private;

   procedure Init (What : out Host;
                   Name    : String;
                   Maintain: Maintenance;
                   Bug : Natural);
private
type Host is new Node with null record;
end Hosts;
