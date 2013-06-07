with Ada.Text_IO;
with Ada.Strings.Fixed;
with Parser;
with Utils; use Utils;
with Nodes;
with DOM.Core.Nodes;
with DOM.Core.Attrs;
with Ada.Exceptions; use Ada.Exceptions;
with Bugzilla;
with Statistics;

package body Nodes is

   use Ada;
   use Node_Lists;

   procedure Enable (What : Node_Safe_Pointer'Class) is
   begin
      Enable (-What);
   end Enable;

   procedure Disable (What : Node_Safe_Pointer'Class) is
   begin
      Disable (-What);
   end Disable;

   ----------------
   -- Check_Node --
   ----------------

   procedure Check_Node (What       : Node_Safe_Pointer;
                         Idle_Count : in out Integer) is
      The_Node : constant Node'Class := -What;
      Success  : Boolean;
      Disabled, Online, Idle : Boolean;
   begin
      case The_Node.Maintain is
         when none =>
            Query_Node (What     => The_Node,
                        Disabled => Disabled,
                        Online   => Online,
                        Idle     => Idle);
            if Online and Idle then
               Idle_Count := Idle_Count + 1;
            end if;
            if Online and Disabled then
               Handle_Disabled_Node (The_Node);
            end if;
         when ignore =>
            Verbose_Message ("Maintenance: ignoring " & Get_Name (The_Node));
            return;
         when disable =>
            Verbose_Message ("Maintenance: disabling " & Get_Name (The_Node));
            Disable (The_Node);
         when off =>
            Query_Node (What     => The_Node,
                        Disabled => Disabled,
                        Online   => Online,
                        Idle     => Idle);
            if Online and Idle then
               Try_To_Poweroff (The_Node  => The_Node,
                                Succeeded => Success);
               if Success then
                  Ada.Text_IO.Put_Line ("Powered off " & Get_Name (The_Node)
                                        & " for maintenance");
                  if The_Node.Bug > 0  then
                     Bugzilla.Add_Comment (Bug_ID  => The_Node.Bug,
                                        Comment => "Powered off " & Get_Name (The_Node)
                                        & " for maintenance");
                     Ada.Text_IO.Put_Line ("Added bugzilla comment");
                  end if;
               else
                  Disable (The_Node);
                  Debug ("Could not (yet) power off " & Get_Name (The_Node)
                         & " for maintenance");
               end if;
            elsif not Idle and not Disabled then
               Verbose_Message ("Maintenance: disabling " & Get_Name (The_Node)
                               & " (poweroff pending)");
               Disable (The_Node);
               if The_Node.Bug > 0  then
                  Bugzilla.Add_Comment (Bug_ID  => The_Node.Bug,
                                        Comment => "Disabling " & Get_Name (The_Node)
                               & " (poweroff pending)");
                  Verbose_Message ("Added bugzilla comment");
               end if;
            elsif Online and Disabled then
               Debug ("Waiting for disabled " & Get_Name (The_Node)
                      & " to become idle");
            elsif not Online then
               Debug ("Will not power off " & Get_Name (The_Node)
                      & " because it is already off");
            else
               raise Program_Error with "This code should be unreachable";
            end if;
      end case;
   end Check_Node;

   ----------------
   -- Query_Node --
   ----------------

   procedure Query_Node (What : Node; Disabled, Online, Idle : out Boolean) is
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
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Failed to query node " & Get_Name (What) & ": "
                               & Exception_Message (E));
         Idle := False; -- This is a safe state, since busy nodes will not be shut off
   end Query_Node;

   ------------------------
   -- Is_Online_And_Idle --
   ------------------------

   function Is_Online_And_Idle (What : Node) return Boolean is
      Online, Idle, Disabled : Boolean;
   begin
      Query_Node (What     => What,
                  Disabled => Disabled,
                  Online   => Online,
                  Idle     => Idle);
      return Online and then Idle;
   end Is_Online_And_Idle;

   ---------------
   -- Is_Online --
   ---------------

   function Is_Online (What : Node) return Boolean is
      Online, Idle, Disabled : Boolean;
   begin
      Query_Node (What => What,
                  Disabled => Disabled,
                  Online   => Online,
                  Idle     => Idle);
      return Online;
   end Is_Online;

   -------------
   -- Is_Idle --
   -------------

   function Is_Idle (What : Node) return Boolean is
      Online, Idle, Disabled : Boolean;
   begin
      Query_Node (What => What,
                  Disabled => Disabled,
                  Online   => Online,
                  Idle     => Idle);
      return Idle;
   end Is_Idle;

   function Get_Name (What : Node) return String is
   begin
      return To_String (What.Name);
   end Get_Name;

   --------------------------
   -- Handle_Disabled_Node --
   --------------------------

   procedure Handle_Disabled_Node (The_Node : Node'Class) is
   begin
      if Utils.Random > 0.1 then
         return;
      end if;
      Ada.Text_IO.Put_Line ("Enabling suspect " & To_String (The_Node.Name));
      Enable (The_Node);
   end Handle_Disabled_Node;

   function "+" (N : Node'Class) return Node_Safe_Pointer is
   begin
      return (Ada.Finalization.Controlled with new Node'Class'(N));
   end "+";

   function "-" (P : Node_Safe_Pointer) return Node'Class is
   begin
      return P.N.all;
   end "-";


   --------------
   -- Get/Set --
   --------------

   procedure Set_Maintenance (Where : in out Node'Class; Maint : Maintenance) is
   begin
      Where.Maintain := Maint;
   end Set_Maintenance;

   procedure Set_Bug (Where : in out Node'Class; Bug_ID : Natural) is
   begin
      Where.Bug := Bug_ID;
   end Set_Bug;

   procedure Set_Name (Where : in out Node; Name : String) is
   begin
      Where.Name := To_Unbounded_String (Name);
   end Set_Name;

   function In_Maintenance (What : Node) return Boolean is
   begin
      return What.Maintain /= none;
   end In_Maintenance;

   function Get_Maintenance (What : Node) return String is
   begin
      return What.Maintain'Img;
   end Get_Maintenance;


   ---------------------
   -- Try_To_Poweroff --
   ---------------------

   procedure Try_To_Poweroff (The_Node : Node'Class; Succeeded : out Boolean) is
   begin
      Disable (What => The_Node);
      if not Nodes.Is_Idle (What => The_Node) then
         -- the scheduler has been faster
         Enable (What => The_Node);
         -- it is OK to enable The_Node without checking whether
         -- it has been disabled by an admin:
         -- if we are here, the node has been idle, but is no longer
         -- therefore, it can only have been disabled after we checked
         -- for Is_Idle. An admin is unlikely to have hit this short
         -- interval.
         Statistics.Race;
         Succeeded := False;
      else
         Poweroff (What => The_Node);
         Succeeded := True;
      end if;
   end Try_To_Poweroff;

   overriding function Is_Empty (What : List) return Boolean is
   begin
      return Is_Empty (Node_Lists.List (What));
   end Is_Empty;

   procedure Rewind (What : in out List) is
   begin
      What.Current := What.First;
   end Rewind;

   procedure Next_Node (Where : in out List) is
   begin
      Node_Lists.Next (Where.Current);
   end Next_Node;

   function Current (From : List) return Node_Safe_Pointer'Class is
   begin
      return Node_Lists.Element (From.Current);
   end Current;

   function At_End (What : List) return Boolean is
   begin
      return What.Current = Node_Lists.No_Element;
   end At_End;

   procedure Append (Where : in out List; What : Node_Safe_Pointer'Class) is
   begin
      Node_Lists.Append (Node_Lists.List (Where), Node_Safe_Pointer (What));
   end Append;

   procedure Iterate (Over    : List;
                      Process : not null access procedure (Element : Node_Safe_Pointer'Class)) is
      Pos : Node_Lists.Cursor := Over.First;
   begin
      while Pos /= No_Element loop
         Process (Element (Pos));
         Next (Pos);
      end loop;
   end Iterate;

   overriding procedure Clear (What : in out List) is
   begin
      Clear (Node_Lists.List (What));
   end Clear;

end Nodes;
