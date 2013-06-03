with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;

package Nodes is

   type Maintenance is (none, ignore, disable, off);
   type Node is abstract tagged private;
   type Node_Access is access all Node'Class;
   type Node_Safe_Pointer is tagged private;

   function "+" (N : Node'Class) return Node_Safe_Pointer;

   function Get_Name (What : Node) return String;
   procedure Query_Node (What : Node; Disabled, Online, Idle : out Boolean);
   function Is_Online_And_Idle (What : Node) return Boolean;
   function Is_Idle (What : Node) return Boolean;
   function Is_Online (What : Node) return Boolean;
   procedure Handle_Disabled_Node (The_Node : Node);
   -- call only for nodes that are disabled for unknown reasons (i.e. not by
   -- us during this run, nor in maintenance)
   procedure Set_Maintenance (Where : in out Node'Class; Maint : Maintenance);
   procedure Set_Bug (Where : in out Node'Class; Bug_ID : Natural);

   type List is tagged private;

   function Is_Empty (What : List) return Boolean;
   procedure Append (Where : List; What: Node_Safe_Pointer'Class);

private
   type Node is abstract tagged record
      Name     : Unbounded_String;
      Maintain : Maintenance;
      Bug      : Natural := 0;
   end record;

   type Node_Safe_Pointer is new Ada.Finalization.Controlled with record
      N : Node_Access;
   end record;

   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (
      Element_Type => Node_Safe_Pointer);
   type List is new Node_Lists.List with null record;
   subtype Cursor is Node_Lists.Cursor;

   procedure Check_Node (What       : Cursor;
                         Idle_Count : in out Integer);

end Nodes;
