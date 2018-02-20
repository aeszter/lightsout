with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Containers.Doubly_Linked_Lists;

package Nodes is

   type Maintenance is (none, ignore, disable, off);
   type Node is abstract tagged private;
   type Node_Access is access all Node'Class;
   type Node_Safe_Pointer is tagged private;

   function "+" (N : Node'Class) return Node_Safe_Pointer;
   function "-" (P : Node_Safe_Pointer) return Node'Class;

   --------------
   -- Get/Set --
   --------------

   function Get_Name (What : Node) return String;
   procedure Set_Name (Where : in out Node; Name : String);
   procedure Set_Maintenance (Where : in out Node'Class; Maint : Maintenance);
   procedure Set_Bug (Where : in out Node'Class; Bug_ID : Natural);
   procedure Set_Sequence (Where : in out Node'Class; Sequence_No : Natural);
   function In_Maintenance (What : Node) return Boolean;
   function Get_Maintenance (What : Node) return String;
   function Has_Active_Sequence (What : Node'Class) return Boolean;
   function Get_Sequence (What : Node'Class) return Natural;


   -----------
   -- Check --
   -----------

   procedure Query_Node (What : Node; Disabled, Online, Idle : out Boolean) is abstract;
   function Is_Online_And_Idle (What : Node'Class) return Boolean;
   function Is_Idle (What : Node'Class) return Boolean;
   function Is_Online (What : Node'Class) return Boolean;



   -------------
   -- Actions --
   -------------

   procedure Check_Node (What       : Node_Safe_Pointer;
                         Idle_Count : in out Integer);

   procedure Handle_Disabled_Node (The_Node : Node'Class);
   -- call only for nodes that are disabled for unknown reasons (i.e. not by
   -- us during this run, nor in maintenance)

   procedure Poweron (What : Node) is abstract;
   procedure Poweroff (What : Node) is abstract;
   procedure Enable (What : Node) is abstract;
   procedure Disable (What : Node) is abstract;

   procedure Enable (What : Node_Safe_Pointer'Class);
   procedure Disable (What : Node_Safe_Pointer'Class);

   procedure Try_To_Poweroff (The_Node : Node'Class; Succeeded : out Boolean);
   -- Given an online and idle node, disable it, check for idleness (again),
   -- then switch it off (if still idle), otherwise just enable it again
   -- and notify caller of failure


   type List is tagged private;

   -------------------
   -- List handling --
   -------------------

   function Is_Empty (What : List) return Boolean;
   procedure Rewind (What : in out List);
   procedure Next_Node (Where : in out List);
   function Current (From : List) return Node_Safe_Pointer'Class;
   function At_End (What : List) return Boolean;
   procedure Append (Where : in out List; What : Node_Safe_Pointer'Class);
   procedure Iterate (Over    : List;
                      Process : not null access procedure (Element : Node_Safe_Pointer'Class));
   procedure Query_Nodes (From : List; Disabled, Online, Idle : out Natural);
   procedure Clear (What : in out List);
   function Length (From : List) return Natural;
   procedure Sort (What : in out List);
   procedure Reverse_Sort (What : in out List);
   function Copy (Source : List) return List;

private
   type Node is abstract tagged record
      Name     : Unbounded_String;
      Maintain : Maintenance;
      Bug      : Natural := 0;
      Sequence : Natural := 1;
   end record;

   type Node_Safe_Pointer is new Ada.Finalization.Controlled with record
      N : Node_Access;
   end record;

   function Precedes_By_Sequence (Left, Right : Node_Safe_Pointer) return Boolean;
   function Succedes_By_Sequence (Left, Right : Node_Safe_Pointer) return Boolean;

   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Node_Safe_Pointer);

   type List is new Node_Lists.List with record
      Current : Node_Lists.Cursor := Node_Lists.No_Element;
   end record;


   package Sorting is new Node_Lists.Generic_Sorting
     ("<" => Precedes_By_Sequence);

   package Reverse_Sorting is new Node_Lists.Generic_Sorting
     ("<" => Succedes_By_Sequence);

end Nodes;
