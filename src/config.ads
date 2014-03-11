with Node_Groups;
with Ada.Strings.Unbounded;

package Config is
   function Read return Node_Groups.List;
   function Get_Bugzilla return String;
   Config_Error : exception;
private
   Bugzilla_Address : Ada.Strings.Unbounded.Unbounded_String;
end Config;
