with Ada.Strings.Fixed;
with Utils; use Utils; use Utils.String_Lists;
with Parser;
with DOM.Core; with DOM.Core.Nodes; with DOM.Core.Attrs;
with Actions; use Actions;

package body Node_Groups is

   --------------
   -- Get_Name --
   --------------

   function Get_Name (What : Group) return String is
   begin
      return To_String (What.Group_Name);
   end Get_Name;

   ------------
   -- Manage --
   ------------

   procedure Manage (What : Lists.Cursor) is
      The_Group  : constant Group := Lists.Element (What);
      Idle_Counter : Integer := 0;
      Index      : Utils.String_Lists.Cursor :=  The_Group.Host_Names.First;
      Nodes_To_Switch_On : Natural;
      Nodes_To_Switch_Off : Natural;
   begin
      while Index /= No_Element loop
         Check_Node (What       => Index,
                     Idle_Count => Idle_Counter);
         Next (Index);
      end loop;

      if Idle_Counter < The_Group.Min_Online then
         Debug (Idle_Counter'Img & " Nodes idle when " & The_Group.Min_Online'Img
                  & " is the minimum");
         Nodes_To_Switch_On := The_Group.Online_Target - Idle_Counter;
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
                         " because the threshold of"
                         & The_Group.Online_Target'Img & " has been reached");
                  exit Switch_On;
               end if;
               Next (Index);
            end;
         end loop Switch_On;
      elsif Idle_Counter > The_Group.Max_Online then
         Debug (Idle_Counter'Img & " Nodes idle when " & The_Group.Max_Online'Img
                  & " is the maximum");
         Nodes_To_Switch_Off := Idle_Counter - The_Group.Online_Target;
         Index := The_Group.Host_Names.First;
         Switch_Off :
         while Index /= No_Element loop
            declare
               The_Node : constant String := To_String (Utils.String_Lists.Element (Index));
            begin
               if Nodes_To_Switch_Off > 0 and then
                 Is_Online_And_Idle (Node => The_Node) then
                  Disable (Node => The_Node);
                  if not Is_Idle (Node => The_Node) then
         -- the scheduler has been faster
                     Enable (Node => The_Node);
                     -- it is OK to enable The_Node without checking whether
                     -- it has been disabled by an admin:
                     -- if we are here, the node has been idle, but is no longer
                     -- therefore, it can only have been disabled after we checked
                     -- for Is_Idle. An admin is unlikely to have hit this short
                     -- interval.
                  else
                     Poweroff (Node => The_Node);
                     Nodes_To_Switch_Off := Nodes_To_Switch_Off - 1;
                  end if;
               elsif Nodes_To_Switch_Off = 0 then
                  Debug (Message => "Not switching off nodes after "
                         & The_Node & " because the threshold of"
                         & The_Group.Online_Target'Img & " has been reached.");
                  exit Switch_Off;
               end if;
               Next (Index);
            end;
         end loop Switch_Off;
      end if;
   end Manage;

   procedure Check_Node (What       : Utils.String_Lists.Cursor;
                         Idle_Count : in out Integer) is
      The_Node : constant String := To_String (Element (What));
   begin
      if Is_Online_And_Idle (Node => The_Node) then
         Idle_Count := Idle_Count + 1;
      end if;
   end Check_Node;

   procedure Query_Node (Node : String; Disabled, Online, Idle : out Boolean) is
      SGE_Out : Parser.Tree;
      Nodes   : DOM.Core.Node_List;
      State_Node : DOM.Core.Node;
      Slots_Node : DOM.Core.Node;
      Count : Natural;
   begin
      SGE_Out := Parser.Setup (Selector => "-f -q \*@" & Node);
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "state");
      Disabled := False;
      Online := True;
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         State_Node := DOM.Core.Nodes.Item (Nodes, I);
         if Ada.Strings.Fixed.Count (Source => DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (State_Node)),
           Pattern => "d") > 0 then
            Disabled := True;
         end if;
         if Ada.Strings.Fixed.Count (Source => DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (State_Node)),
           Pattern => "u") > 0 then
            Online := False;
         end if;
      end loop;

      Idle := True;
      -- extract slots_used and slots_resv
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "slots_used");
      -- if any of these is >0, return false
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         Slots_Node := DOM.Core.Nodes.Item (Nodes, I);
         Count := Integer'Value (DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (Slots_Node)));
         if Count > 0 then
            Idle := False;
         end if;
      end loop;

      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "slots_resv");
      -- if any of these is >0, return false
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         Slots_Node := DOM.Core.Nodes.Item (Nodes, I);
         Count := Integer'Value (DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (Slots_Node)));
         if Count > 0 then
            Idle := False;
         end if;
      end loop;
   end Query_Node;

   function Is_Online_And_Idle (Node : String) return Boolean is
      Online, Idle, Disabled : Boolean;
   begin
      Query_Node (Node     => Node,
                  Disabled => Disabled,
                  Online   => Online,
                  Idle     => Idle);
      return Online and then Idle;
   end Is_Online_And_Idle;

   function Is_Online (Node : String) return Boolean is
      Online, Idle, Disabled : Boolean;
   begin
      Query_Node (Node     => Node,
                  Disabled => Disabled,
                  Online   => Online,
                  Idle     => Idle);
      return Online;
   end Is_Online;

   function Is_Idle (Node : String) return Boolean is
      Online, Idle, Disabled : Boolean;
   begin
      Query_Node (Node     => Node,
                  Disabled => Disabled,
                  Online   => Online,
                  Idle     => Idle);
      return Idle;
   end Is_Idle;

end Node_Groups;
