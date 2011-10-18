with Node_Groups;
with Config;

procedure Lightsout is
   All_Nodes : constant Node_Groups.List := Config.Read;
begin
   All_Nodes.Iterate (Node_Groups.Manage'Access);
end Lightsout;
