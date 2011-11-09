with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
with Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Node_Groups is
   type Group is tagged
      record
         Group_Name    : Unbounded_String;
         Host_Names    : Utils.String_List;
         Min_Online    : Natural := 1;
         Max_Online    : Natural := 5;
         Online_Target : Natural := 4;
      end record;

   function Get_Name (What : Group) return String;

   package Lists is new Doubly_Linked_Lists (Element_Type => Group);
   subtype List is Lists.List;
   procedure Manage (What : Lists.Cursor);
   procedure Check_Node (What       : Utils.String_Lists.Cursor;
                         Idle_Count : in out Integer);
   procedure Query_Node (Node : String; Disabled, Online, Idle : out Boolean);
   function Is_Online_And_Idle (Node : String) return Boolean;
   function Is_Idle (Node : String) return Boolean;
   function Is_Online (Node : String) return Boolean;

private
      procedure Bring_Nodes_Online (How_Many : Integer; Hosts : Utils.String_List);
      -- switch on How_Many nodes from the Hosts list
      procedure Put_Nodes_Offline (How_Many : Integer; Hosts : Utils.String_List);
      -- switch off How_Many nodes from the Hosts list

end Node_Groups;
