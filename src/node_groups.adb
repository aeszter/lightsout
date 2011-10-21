with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Utils; use Utils; use Utils.String_Lists;
with Parser;
with DOM.Core; with DOM.Core.Nodes; with DOM.Core.Attrs;

package body Node_Groups is

   ------------
   -- Manage --
   ------------

   procedure Manage (What : Lists.Cursor) is
      The_Group  : constant Group := Lists.Element (What);
      Idle_Counter : Integer := -The_Group.Number_To_Keep_Online;
      Index      : Utils.String_Lists.Cursor :=  The_Group.Host_Names.First;
      Nodes_To_Switch_On : Natural;
   begin
      while Index /= No_Element loop
         Check_Node (What                       => Index,
                     Idle_Count_Above_Threshold => Idle_Counter);
         Next (Index);
      end loop;

      if Idle_Counter < 0 then
         Nodes_To_Switch_On := -Idle_Counter;
         Index := The_Group.Host_Names.First;
         Switch_On :
         while Index /= No_Element loop
            declare
               The_Node : constant String := To_String (Utils.String_Lists.Element (Index));
            begin
               if Nodes_To_Switch_On > 0 and then
                not Is_Online (Node => The_Node) then
                  Poweron (Node => The_Node);
                  Enable (Node => The_Node);
                  Nodes_To_Switch_On := Nodes_To_Switch_On - 1;
               end if;
               if Nodes_To_Switch_On = 0 then
                  Debug ("Not switching on nodes after " & The_Node &
                         " because the threshold has been reached");
                  exit Switch_On;
               end if;
               Next (Index);
            end;
         end loop Switch_On;
      end if;
   end Manage;

   procedure Check_Node (What       : Utils.String_Lists.Cursor;
                         Idle_Count_Above_Threshold : in out Integer) is
      The_Node : constant String := To_String (Element (What));
      Was_Disabled : Boolean := Is_Disabled (Node => The_Node);
   begin
      if not Is_Online (Node => The_Node) or else
        not Is_Idle (Node => The_Node) then
         Debug ("Not switching off " & The_Node & " because not online or not idle");
         return;
      end if;
      -- check idle time against TTKO
      Idle_Count_Above_Threshold := Idle_Count_Above_Threshold + 1;
      if Idle_Count_Above_Threshold <= 0 then
         Debug ("Not switching off " & The_Node
                   & " because" & Integer'Image (-Idle_Count_Above_Threshold)
                     & " less nodes are idle than required");
         return; -- keep node online
      end if;
      Was_Disabled := Is_Disabled (Node => The_Node);
      Disable (Node => The_Node);
      if not Is_Idle (Node => The_Node) then
         -- the scheduler has been faster
         if not Was_Disabled then
            Enable (Node => The_Node);
         end if;
         return;
      end if;
      Poweroff (Node => The_Node);
   end Check_Node;

   procedure Enable (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Enabling " & Node);
   end Enable;

   procedure Disable (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Disabling " & Node);
   end Disable;

   procedure Poweron (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Switching on " & Node);

   end Poweron;

   procedure Poweroff (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Switching off " & Node);
   end Poweroff;

   function Is_Disabled (Node : String) return Boolean is
      SGE_Out : Parser.Tree;
      Nodes   : DOM.Core.Node_List;
      State_Node : DOM.Core.Node;
   begin
      SGE_Out := Parser.Setup (Selector => "-f -q \*@" & Node);
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "state");
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         State_Node := DOM.Core.Nodes.Item (Nodes, I);
         if Ada.Strings.Fixed.Count (Source => DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (State_Node)),
           Pattern => "d") > 0 then
            return True;
         end if;
      end loop;
      return False;
   end Is_Disabled;

   function Is_Online (Node : String) return Boolean is
      SGE_Out : Parser.Tree;
      Nodes   : DOM.Core.Node_List;
      State_Node : DOM.Core.Node;
   begin
      SGE_Out := Parser.Setup (Selector => "-f -q \*@" & Node);
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "state");
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         State_Node := DOM.Core.Nodes.Item (Nodes, I);
         if Ada.Strings.Fixed.Count (Source => DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (State_Node)),
           Pattern => "u") > 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Online;

   function Is_Idle (Node : String) return Boolean is
      SGE_Out : Parser.Tree;
      Nodes   : DOM.Core.Node_List;
      Slots_Node : DOM.Core.Node;
      Count : Natural;
   begin
      -- call qstat -f -q *@Node -xml
      SGE_Out := Parser.Setup (Selector => "-f -q \*@" & Node);
      -- extract slots_used and slots_resv
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "slots_used");
      -- if any of these is >0, return false
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         Slots_Node := DOM.Core.Nodes.Item (Nodes, I);
         Count := Integer'Value (DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (Slots_Node)));
         if Count > 0 then
            return False;
         end if;
      end loop;

      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "slots_resv");
      -- if any of these is >0, return false
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         Slots_Node := DOM.Core.Nodes.Item (Nodes, I);
         Count := Integer'Value (DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (Slots_Node)));
         if Count > 0 then
            return False;
         end if;
      end loop;

      -- otherwise, return true
      return True;
   end Is_Idle;


end Node_Groups;
