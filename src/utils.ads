with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Utils is
   package String_Lists is new Doubly_Linked_Lists (Element_Type => Unbounded_String);
   subtype String_List is String_Lists.List;
end Utils;
