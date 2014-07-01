with Utils; use Utils;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Statistics;
with Hosts; use Hosts;
with Actions;

package body Node_Groups is

   procedure Inner_Sort (What : in out Group);

   procedure Add_Host (Where : in out Group;
                       Name  : String;
                       Mode  : String;
                       Bug   : Natural;
                       Sequence : Natural) is
      New_Host : Host;
   begin
      New_Host.Init (Name => Name,
                     Maintain => Maintenance'Value (Mode),
                     Bug      => Bug,
                     Sequence => Sequence);
      Where.Hosts.Append (+New_Host);
   exception
      when E : Constraint_Error =>
         Ada.Text_IO.Put_Line ("Unable to add host """ & Name
                               & """, possibly because of illegal mode """
                               & Mode & """");
         Ada.Text_IO.Put_Line (Exception_Message (E));
   end Add_Host;

   procedure Add_Twin (Where : in out Group;
                       What  : Twin;
                       Mode  : String;
                       Bug   : Natural;
                       Sequence : Natural) is
      New_Node : Twin := What;
   begin
      New_Node.Set_Maintenance (Maintenance'Value (Mode));
      New_Node.Set_Bug (Bug);
      New_Node.Set_Sequence (Sequence);
      Where.Hosts.Append (+New_Node);
   end Add_Twin;


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

   procedure Bring_Nodes_Online (How_Many : Integer; Hosts : in out Nodes.List)
   is
      Nodes_To_Switch_On : Natural := How_Many;
   begin
      Hosts.Reverse_Sort;
      Hosts.Rewind;
      while not At_End (Hosts) loop
         declare
            The_Node : constant Node_Safe_Pointer'Class := Hosts.Current;
         begin
            if not In_Maintenance (-The_Node) then
               if Nodes_To_Switch_On > 0 and then
                 not Is_Online (What => -The_Node) then
                  Poweron (What => -The_Node);
                  Enable (What => -The_Node);
                  Nodes_To_Switch_On := Nodes_To_Switch_On - 1;
               end if;
               if Nodes_To_Switch_On = 0 then
                  Debug ("Not switching on nodes after " & Get_Name (-The_Node) &
                         " because the required number of"
                         & How_Many'Img & " has been reached");
                  return;
               end if;
            else -- Maintain /= none
               Debug ("Not considering switching on " & Get_Name (-The_Node) &
                      " because of maintenance """ & Get_Maintenance (-The_Node) & """");
            end if;
            Hosts.Next_Node;
         exception
            when E : Actions.Subcommand_Error =>
               Verbose_Message ("While switching on " & Get_Name (-The_Node) & ":");
               Verbose_Message (Exception_Message (E));
               Hosts.Next_Node;
         end;
      end loop;
      if Nodes_To_Switch_On > 0 then
         Debug ("Not switching on " & Nodes_To_Switch_On'Img
                & " more nodes because none left");
         Statistics.Too_Few_Nodes (Number => Nodes_To_Switch_On);
      end if;
   end Bring_Nodes_Online;

   -----------------------
   -- Put_Nodes_Offline --
   -----------------------

   procedure Put_Nodes_Offline (How_Many : Integer; Hosts : in out Nodes.List)
   is
      Nodes_To_Switch_Off : Natural := How_Many;
      Success             : Boolean;
   begin
      Hosts.Sort;
      Hosts.Rewind;
      while not At_End (Hosts) loop
         declare
            The_Node : constant Node_Safe_Pointer'Class := Current (Hosts);
         begin
            if not In_Maintenance (-The_Node)
               and then Has_Active_Sequence (-The_Node) then
               if Nodes_To_Switch_Off > 0 and then
                 Is_Online_And_Idle (What => -The_Node) then
                  Try_To_Poweroff (The_Node => -The_Node, Succeeded => Success);
                  if Success then
                     Nodes_To_Switch_Off := Nodes_To_Switch_Off - 1;
                  end if;
               elsif Nodes_To_Switch_Off = 0 then
                  Debug (Message => "Not switching off nodes after "
                         & Get_Name (-The_Node) & " because the required number of"
                         & How_Many'Img & " has been reached.");
                  return;
               end if;
            end if;
            Hosts.Next_Node;
         exception
            when E : Actions.Subcommand_Error =>
               Verbose_Message ("While switching off " & Get_Name (-The_Node) & ":");
               Verbose_Message (Exception_Message (E));
               Hosts.Next_Node;
         end;
      end loop;
   end Put_Nodes_Offline;

   ------------
   -- Manage --
   ------------

   procedure Manage (What : Lists.Cursor) is
      Idle_Counter : Integer := 0;
      The_Group    : Group := Lists.Element (What);
   begin
      The_Group.Hosts.Rewind;
      while not At_End (The_Group.Hosts) loop
         Check_Node (What       => The_Group.Hosts.Current,
                     Idle_Count => Idle_Counter);
         Statistics.Node_Seen;
         The_Group.Hosts.Next_Node;
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
                            Hosts    => The_Group.Hosts);
      else
         Debug (The_Group.Get_Name & ":" & Idle_Counter'Img
                & " Nodes idle, limits: " & The_Group.Min_Online'Img & The_Group.Max_Online'Img);
      end if;
   exception
      when E : Actions.Subcommand_Error =>
         Ada.Text_IO.Put_Line (Exception_Message (E)
                               & " -- skipping remainder of group "
                               & The_Group.Get_Name);
   end Manage;

   procedure Sort (What : in out List) is
      use Lists;

      Position : Lists.Cursor := What.First;
   begin
      while Position /= Lists.No_Element loop
         What.Update_Element (Position, Inner_Sort'Access);
         Next (Position);
      end loop;
   end Sort;

   procedure Inner_Sort (What : in out Group) is
   begin
      What.Hosts.Sort;
   end Inner_Sort;

end Node_Groups;
