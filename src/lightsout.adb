with Node_Groups;
with Config;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with Utils; use Utils;
with Statistics;


procedure Lightsout is
   Exit_Unknown_Error : constant Exit_Status := 1;
   Exit_Config_Error  : constant Exit_Status := 2;

   All_Nodes : Node_Groups.List;
begin
   Utils.Verbose_Message ("Lightsout " & Utils.Version & " by aeszter@mpibpc.mpg.de");
   Utils.Check_Options;
   Debug ("Debugging enabled");
   All_Nodes := Config.Read;
   if not Utils.Terminate_After_Config then
      All_Nodes.Iterate (Node_Groups.Manage'Access);
      if Utils.Stats_Enabled then
         Statistics.Print;
      end if;
   end if;
exception
   when E : Config.Config_Error =>
      Put_Line (File => Standard_Error,
                  Item => "Unable to read config: " & Exception_Message (E));
      Set_Exit_Status (Code => Exit_Config_Error);
   when E : others =>
      Put_Line (File => Standard_Error,
                Item => "Unexpected error (" & Exception_Name (E) & "): " & Exception_Message (E));
      Put (File => Standard_Error,
           Item => Exception_Information (E));
      Set_Exit_Status (Code => Exit_Unknown_Error);
end Lightsout;
