with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Numerics.Float_Random;

package Utils is
   Version : String := "v1.5";
   package String_Lists is new Doubly_Linked_Lists (Element_Type => Unbounded_String);
   subtype String_List is String_Lists.List;

   procedure Debug (Message : String);
   procedure Verbose_Message (Message : String);
   procedure Enable_Debug;
   procedure Check_Options;
   function Dry_Run (Message         : String;
                     Show_Anyway : Boolean := True) return Boolean;
   -- return whether Action is false
   -- if so, print Message
   -- also, print Message if both Verbose and Show_On_Verbose are true

   function Terminate_After_Config return Boolean;
   function Stats_Enabled return Boolean;

   function Random return Ada.Numerics.Float_Random.Uniformly_Distributed;
   procedure Init_Random;
private
   Debug_Enabled : Boolean := False;
   Action        : Boolean := True;
   Config_Only   : Boolean := False;
   Verbose       : Boolean := False;
   Stats         : Boolean := False;
   Random_Generator : Ada.Numerics.Float_Random.Generator;
end Utils;
