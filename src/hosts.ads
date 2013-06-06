with Nodes; use Nodes;

package Hosts is
   type Host is new Node with private;

   procedure Init (What     : out Host;
                   Name     : String;
                   Maintain : Maintenance;
                   Bug      : Natural);

   overriding procedure Poweron (What : Host);
   overriding procedure Poweroff (What : Host);
   overriding procedure Enable (What : Host);
   overriding procedure Disable (What : Host);

private
   type Host is new Node with null record;
end Hosts;
