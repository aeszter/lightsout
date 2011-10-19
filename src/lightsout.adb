with Node_Groups;
with Config;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;


procedure Lightsout is
   Exit_Unknown_Error : constant Exit_Status := 1;
   Exit_Config_Error  : constant Exit_Status := 2;

   All_Nodes : Node_Groups.List;
begin
   All_Nodes := Config.Read;
   All_Nodes.Iterate (Node_Groups.Manage'Access);
exception
   when E : Config.Config_Error =>
      Put_Line (File => Standard_Error,
                  Item => "Unable to read config: " & Exception_Message (E));
      Set_Exit_Status (Code => Exit_Config_Error);
   when E : others =>
      Put_Line (File => Standard_Error,
                Item => "Unexpected error: " & Exception_Message (E));
      Set_Exit_Status (Code => Exit_Unknown_Error);
end Lightsout;
