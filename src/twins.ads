with Ada.Strings.Bounded;
with Nodes;
use Nodes;

package Twins is
   package PDU_Strings is
     new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 10);
   type PDU_String is new PDU_Strings.Bounded_String;

   type Twin is new Node with private;

   procedure Init (What : in out Twin);
   procedure Set_PDU (Where : in out Twin; Str : String);
   procedure Add_Host (Where : in out Twin; Host : String);

private
   type Twin is new Node with record
      PDU : PDU_String;
      Sub_Nodes : Nodes.List;
   end record;
end Twins;
