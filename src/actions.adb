with POSIX; use POSIX;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX.IO;
with Utils; use Utils;
with Nodes; use Nodes;
with Statistics;
with Ada.Exceptions; use Ada.Exceptions;
with CM.Power;
with CM.Taint; use CM.Taint;

package body Actions is

   procedure Disable_Or_Enable (The_Node : Nodes.Node; Enable : Boolean);



   procedure Disable_Or_Enable (The_Node : Nodes.Node; Enable : Boolean) is

      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
      Node_Name     : constant String := Get_Name (What => The_Node);
   begin
      if Enable then
         if Utils.Dry_Run (Message => "enabling " & Node_Name,
                           Show_Anyway => False)
         then
            return;
         end if;
      else
         if Utils.Dry_Run (Message => "disabling " & Node_Name,
                           Show_Anyway => False)
         then
            return;
         end if;
      end if;
      Append (Args, "qmod");
      if Enable then
         Append (Args, "-e");
         Debug ("enabling " & Node_Name);
      else
         Append (Args, "-d");
         Debug ("disabling " & Node_Name);
      end if;
      Append (Args, To_POSIX_String ("*@" & Node_Name));
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => POSIX.IO.Standard_Output);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => "/cm/shared/apps/sge/current/bin/linux-x64/qmod",
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create qmod process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in qmod";
         when others => raise Subcommand_Error with "qmod exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   exception
      when E : POSIX_Error =>
         if Enable then
            raise Subcommand_Error with "qmod raised error when called with ""-e *@" & Node_Name &
            """:" & Exception_Message (E);
         else
            raise Subcommand_Error with "qmod raised error when called with ""-d *@" & Node_Name &
            """:" & Exception_Message (E);
         end if;
   end Disable_Or_Enable;


   procedure Enable (What : Nodes.Node) is
   begin
      Disable_Or_Enable (The_Node => What,
                            Enable   => True);
   end Enable;


   procedure Disable (What : Nodes.Node) is
   begin
      Disable_Or_Enable (The_Node => What, Enable => False);
   end Disable;

   procedure Poweron (What : Nodes.Node) is
      The_Node     : constant Trusted_String := Sanitise (Get_Name (What));
   begin
      Statistics.Node_Switched_On;
      if Utils.Dry_Run ("switching on " & Value (The_Node)) then
         return;
      end if;
      CM.Power.Poweron (The_Node);
   end Poweron;

   procedure Poweron (PDU : Twins.PDU_String) is
      use type Twins.PDU_String;
   begin
      if Utils.Dry_Run ("switching on PDU " & Twins.PDU_Strings.To_String (PDU)) then
         return;
      end if;
      CM.Power.Poweron (Sanitise (Twins.PDU_Strings.To_String (PDU)), PDU => True);
   end Poweron;

   procedure Poweroff (What : Nodes.Node) is
      The_Node : constant Trusted_String := Sanitise (Get_Name (What));
   begin
      Statistics.Node_Switched_Off;
      if Utils.Dry_Run ("switching off " & Value (The_Node)) then
         return;
      end if;
      CM.Power.Poweroff (What => The_Node,
                   PDU  => False);
   end Poweroff;

   procedure Poweroff (PDU : Twins.PDU_String) is
      use type Twins.PDU_String;
   begin
      if Utils.Dry_Run ("switching off PDU " & Twins.PDU_Strings.To_String (PDU)) then
         return;
      end if;
      CM.Power.Poweroff (Sanitise (Twins.PDU_Strings.To_String (PDU)), PDU => True);
   end Poweroff;


   procedure Powercycle (What : Nodes.Node) is
      The_Node : constant Trusted_String := Sanitise (Get_Name (What));
   begin
      if Utils.Dry_Run ("powercycling " & Value (The_Node)) then
         return;
      end if;
      CM.Power.Powercycle (The_Node);
   end Powercycle;

end Actions;
