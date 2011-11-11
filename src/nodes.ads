with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Nodes is

   type Maintenance is (none, ignore, disable, off);
   type Node is tagged record
      Name     : Unbounded_String;
      Maintain : Maintenance;
   end record;

   function Get_Name (What : Node) return String;
   procedure Query_Node (What : Node; Disabled, Online, Idle : out Boolean);
   function Is_Online_And_Idle (What : Node) return Boolean;
   function Is_Idle (What : Node) return Boolean;
   function Is_Online (What : Node) return Boolean;

   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (
      Element_Type => Node);
   subtype List is Node_Lists.List;
   subtype Cursor is Node_Lists.Cursor;

   procedure Check_Node (What       : Cursor;
                         Idle_Count : in out Integer);
end Nodes;
