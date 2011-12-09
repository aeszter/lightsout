with Ada.Text_IO;
with Actions; use Actions;
with Parser;
with Utils; use Utils;
with Ada.Strings.Fixed;
with Nodes; use Nodes.Node_Lists;
with DOM.Core.Nodes;
with DOM.Core.Attrs;
with Ada.Exceptions; use Ada.Exceptions;

package body Nodes is

   ----------------
   -- Check_Node --
   ----------------

   procedure Check_Node (What       : Cursor;
                         Idle_Count : in out Integer) is
      The_Node : constant Node := Element (What);
      Success  : Boolean;
   begin
      case The_Node.Maintain is
         when none =>
            if Is_Online_And_Idle (What => The_Node) then
               Idle_Count := Idle_Count + 1;
            end if;
         when ignore =>
            Verbose_Message ("Maintenance: ignoring " & Get_Name (The_Node));
            return;
         when disable =>
            Verbose_Message ("Maintenance: disabling " & Get_Name (The_Node));
            Disable (The_Node);
         when off =>
            if Is_Online_And_Idle (The_Node) then
               Try_To_Poweroff (The_Node  => The_Node,
                                Succeeded => Success);
               if Success then
                  Ada.Text_IO.Put_Line ("Powered off " & Get_Name (The_Node)
                                        & " for maintenance");
               else
                  Debug ("Could not (yet) power off " & Get_Name (The_Node)
                         & " for maintenance");
               end if;
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
end Nodes;
