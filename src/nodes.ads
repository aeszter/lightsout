with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Nodes is

   type Maintenance is (none, ignore, disable, off);
   type Node is record
      Name     : Unbounded_String;
      Maintain : Maintenance;
   end record;

   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (
      Element_Type => Node);
   subtype List is Node_Lists.List;
   subtype Cursor is Node_Lists.Cursor;
end Nodes;
