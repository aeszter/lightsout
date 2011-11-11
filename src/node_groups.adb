with Utils; use Utils;
with Actions; use Actions;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Nodes; use Nodes.Node_Lists;

package body Node_Groups is

   procedure Add_Host (Where : in out Group; Name : String; Mode : String) is
      New_Node : Nodes.Node;
   begin
      New_Node.Name := To_Unbounded_String (Name);
      New_Node.Maintain := Maintenance'Value (Mode);
      Where.Hosts.Append (New_Node);
   exception
      when E : Constraint_Error =>
         Ada.Text_IO.Put_Line ("Unable to add host """ & Name
                               & """, possibly because of illegal mode """
                               & Mode & """");
         Ada.Text_IO.Put_Line (Exception_Message (E));
   end Add_Host;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (What : Group) return String is
   begin
      return To_String (What.Group_Name);
   end Get_Name;

   ------------------------
   -- Bring_Nodes_Online --
   ------------------------

   procedure Bring_Nodes_Online (How_Many : Integer; Hosts : Nodes.List)
   is
      Nodes_To_Switch_On : Natural := How_Many;
      Index    : Nodes.Cursor := Hosts.First;
   begin
      while Index /= No_Element loop
         declare
            The_Node : constant Node := Element (Index);
         begin
            if The_Node.Maintain = none then
               if Nodes_To_Switch_On > 0 and then
                 not Is_Online (What => The_Node) then
                  Poweron (What => The_Node);
                  Enable (What => The_Node);
                  Nodes_To_Switch_On := Nodes_To_Switch_On - 1;
               end if;
               if Nodes_To_Switch_On = 0 then
                  Debug ("Not switching on nodes after " & Get_Name (The_Node) &
                         " because the required number of"
                         & How_Many'Img & " has been reached");
                  return;
               end if;
            else -- Maintain /= none
               Debug ("Not considering switching on" & Get_Name (The_Node) &
                      "because of maintenance """ & The_Node.Maintain'Img & """");
            end if;
            Next (Index);
         end;
      end loop;
      if Nodes_To_Switch_On > 0 then
         Debug ("Not switching on" & Nodes_To_Switch_On'Img
                & " more nodes because none left");
      end if;
   end Bring_Nodes_Online;

   -----------------------
   -- Put_Nodes_Offline --
   -----------------------

   procedure Put_Nodes_Offline (How_Many : Integer; Hosts : Nodes.List)
   is
      Nodes_To_Switch_Off : Natural := How_Many;
      Index               : Nodes.Cursor := Hosts.First;
      Success             : Boolean;
   begin
      while Index /= No_Element loop
         declare
            The_Node : constant Node := Element (Index);
         begin
            if The_Node.Maintain = none then
               if Nodes_To_Switch_Off > 0 and then
                 Is_Online_And_Idle (What => The_Node) then
                  Try_To_Poweroff (The_Node => The_Node, Succeeded => Success);
                  if Success then
                     Nodes_To_Switch_Off := Nodes_To_Switch_Off - 1;
                  end if;
               elsif Nodes_To_Switch_Off = 0 then
                  Debug (Message => "Not switching off nodes after "
                         & Get_Name (The_Node) & " because the required number of"
                         & How_Many'Img & " has been reached.");
                  return;
               end if;
            end if;
            Next (Index);
         end;
      end loop;
   end Put_Nodes_Offline;

   ------------
   -- Manage --
   ------------

   procedure Manage (What : Lists.Cursor) is
      Idle_Counter : Integer := 0;
      The_Group    : constant Group := Lists.Element (What);
      Index        : Nodes.Cursor :=  The_Group.Hosts.First;
   begin
      while Index /= No_Element loop
         Check_Node (What       => Index,
                     Idle_Count => Idle_Counter);
         Next (Index);
      end loop;

      if Idle_Counter < The_Group.Min_Online then
         Debug (The_Group.Get_Name & ":" & Idle_Counter'Img
                & " Nodes idle when" & The_Group.Min_Online'Img
                & " is the minimum");
         Bring_Nodes_Online (How_Many => The_Group.Online_Target - Idle_Counter,
                             Hosts    => The_Group.Hosts);
      elsif Idle_Counter > The_Group.Max_Online then
         Debug (The_Group.Get_Name & ":" & Idle_Counter'Img
                & " Nodes idle when" & The_Group.Max_Online'Img
                & " is the maximum");
         Put_Nodes_Offline (How_Many => Idle_Counter - The_Group.Online_Target,
                            Hosts => The_Group.Hosts);
      end if;
   exception
      when E : Subcommand_Error =>
         Ada.Text_IO.Put_Line (Exception_Message (E)
                               & " -- skipping remainder of group "
                               & The_Group.Get_Name);
   end Manage;


end Node_Groups;
