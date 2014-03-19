with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Nodes; use Nodes;
with Twins; use Twins;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Node_Groups is
   type Group is tagged
      record
         Group_Name    : Unbounded_String;
         Hosts         : Nodes.List;
         Min_Online    : Natural := 1;
         Max_Online    : Natural := 5;
         Online_Target : Natural := 4;
      end record;

   function Get_Name (What : Group) return String;
   procedure Add_Host (Where : in out Group;
                       Name  : String;
                       Mode  : String;
                       Bug   : Natural;
                       Sequence : Natural);
   procedure Add_Twin (Where : in out Group;
                       What  : Twin;
                       Mode  : String;
                       Bug   : Natural);


   package Lists is new Doubly_Linked_Lists (Element_Type => Group);
   subtype List is Lists.List;
   procedure Manage (What : Lists.Cursor);
   procedure Sort (What : in out List);

private
   procedure Bring_Nodes_Online (How_Many : Integer; Hosts : in out Nodes.List);
   -- switch on How_Many nodes from the Hosts list
   procedure Put_Nodes_Offline (How_Many : Integer; Hosts : in out Nodes.List);
   -- switch off How_Many nodes from the Hosts list

end Node_Groups;
