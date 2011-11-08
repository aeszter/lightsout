with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
with Utils;

package Node_Groups is
   type Group is
      record
         Host_Names    : Utils.String_List;
         Min_Online    : Natural := 1;
         Max_Online    : Natural := 5;
         Online_Target : Natural := 4;
      end record;

   package Lists is new Doubly_Linked_Lists (Element_Type => Group);
   subtype List is Lists.List;
   procedure Manage (What : Lists.Cursor);
   procedure Check_Node (What       : Utils.String_Lists.Cursor;
                         Idle_Count : in out Integer);
   procedure Enable (Node : String);
   procedure Disable (Node : String);
   procedure Poweroff (Node : String);
   procedure Poweron (Node : String);
   procedure Query_Node (Node : String; Disabled, Online, Idle : out Boolean);
   function Is_Online_And_Idle (Node : String) return Boolean;
   function Is_Idle (Node : String) return Boolean;
   function Is_Online (Node : String) return Boolean;

end Node_Groups;
