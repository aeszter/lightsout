with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Attrs;
with Actions;
with Parser;

package body Hosts is

   ----------
   -- Init --
   ----------

   procedure Init
     (What     : out Host;
      Name     : String;
      Maintain : Maintenance;
      Bug      : Natural;
      Sequence : Natural)
   is
   begin
      Node (What).Set_Name (Name);
      Node (What).Set_Maintenance (Maintain);
      Node (What).Set_Bug (Bug);
      Node (What).Set_Sequence (Sequence);
   end Init;

   overriding procedure Poweron (What : Host) is
   begin
      Actions.Poweron (Node (What));
   end Poweron;

   overriding procedure Poweroff (What : Host) is
   begin
      Actions.Poweroff (Node (What));
   end Poweroff;

   overriding procedure Enable (What : Host) is
   begin
      Actions.Enable (Node (What));
   end Enable;

   overriding procedure Disable (What : Host) is
   begin
      Actions.Disable (Node (What));
   end Disable;

   overriding procedure Query_Node (What                   : Host;
                                    Disabled, Online, Idle : out Boolean) is
      SGE_Out : Parser.Tree;
      Nodes   : DOM.Core.Node_List;
      State_Node : DOM.Core.Node;
      Slots_Node : DOM.Core.Node;
      Count : Natural;
   begin
      SGE_Out := Parser.Setup (Selector => "-f -q \*@" & Get_Name (What));
      Nodes := Parser.Get_Elements_By_Tag_Name (SGE_Out, "state");
      Disabled := False;
      Online := True;
      for I in 0 .. DOM.Core.Nodes.Length (Nodes) - 1 loop
         State_Node := DOM.Core.Nodes.Item (Nodes, I);
         if Ada.Strings.Fixed.Count (Source => DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (State_Node)),
                                     Pattern => "d") > 0
         then
            Disabled := True;
         end if;
         if Ada.Strings.Fixed.Count (Source => DOM.Core.Attrs.Value (DOM.Core.Nodes.First_Child (State_Node)),
                                     Pattern => "u") > 0
         then
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
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Failed to query node " & Get_Name (What) & ": "
                               & Exception_Message (E));
         Idle := False; -- This is a safe state, since busy nodes will not be shut off
   end Query_Node;

end Hosts;
