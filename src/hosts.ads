with Nodes; use Nodes;

package Hosts is
   type Host is new Node with private;

   procedure Init (What     : out Host;
                   Name     : String;
                   Maintain : Maintenance;
                   Bug      : Natural;
                   Sequence : Natural);

   overriding procedure Poweron (What : Host);
   overriding procedure Poweroff (What : Host);
   overriding procedure Enable (What : Host);
   overriding procedure Disable (What : Host);

   overriding procedure Query_Node (What     : Host;
                                    Disabled,
                                    Online,
                                    Idle     : out Boolean);

private
   type Host is new Node with null record;
end Hosts;
