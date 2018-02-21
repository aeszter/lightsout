with Node_Groups;
with Ada.Strings.Unbounded;

package Config is
   function Read return Node_Groups.List;
   function Get_Bugzilla return String;
   function Get_Mailto return String;
   function Bugzilla_Writable return Boolean;
   Config_Error : exception;
private
   Bugzilla_Address : Ada.Strings.Unbounded.Unbounded_String;
   Mailto_Address   : Ada.Strings.Unbounded.Unbounded_String;
   Bugzilla_Is_Writable : Boolean := True;
end Config;
