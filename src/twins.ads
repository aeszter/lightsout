with Ada.Strings.Bounded;
with Nodes;
use Nodes;

package Twins is

   Twin_Error : exception;

   package PDU_Strings is
     new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 10);
   subtype PDU_String is PDU_Strings.Bounded_String;

   type Twin is new Node with private;

   procedure Init (What : in out Twin);
   procedure Set_PDU (Where : in out Twin; Str : String);
   procedure Add_Host (Where : in out Twin; Host : String);

   overriding procedure Poweron (What : Twin);
   overriding procedure Poweroff (What : Twin);
   overriding procedure Enable (What : Twin);
   overriding procedure Disable (What : Twin);

   overriding procedure Query_Node (What     : Twin;
                                    Disabled, Online,
                                    Idle     : out Boolean);

private
   type Twin is new Node with record
      PDU : PDU_String;
      Sub_Nodes : Nodes.List;
   end record;

end Twins;
