with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Utils is
   package String_Lists is new Doubly_Linked_Lists (Element_Type => Unbounded_String);
   subtype String_List is String_Lists.List;

   procedure Debug (Message : String);
   procedure Enable_Debug;
   procedure Check_Options;
   function Dry_Run (Message : String) return Boolean;
   function Terminate_After_Config return Boolean;

private
   Debug_Enabled : Boolean := False;
   Action        : Boolean := True;
   Config_Only   : Boolean := False;
end Utils;
