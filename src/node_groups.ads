with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
with Utils;

package Node_Groups is
   type Group is
      record
         Host_Names             : Utils.String_List;
         Number_To_Keep_Online  : Natural := 1;
         Seconds_To_Keep_Online : Natural := 3_600;
      end record;

   package Lists is new Doubly_Linked_Lists (Element_Type => Group);
   subtype List is Lists.List;
   procedure Manage (What : Lists.Cursor);
   procedure Check_Node (What       : Utils.String_Lists.Cursor;
                         Idle_Count_Above_Threshold : in out Integer);
   procedure Enable (Node : String);
   procedure Disable (Node : String);
   function Is_Disabled (Node : String) return Boolean;
   procedure Poweroff (Node : String);
   procedure Poweron (Node : String);
   function Is_Online (Node : String) return Boolean;
   function Is_Idle (Node : String) return Boolean;

end Node_Groups;
