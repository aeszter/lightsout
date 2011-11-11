with POSIX; use POSIX;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with Utils; use Utils;
with Nodes; use Nodes;
with Statistics;

package body Actions is

   procedure Enable (What : Nodes.Node) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
      The_Node     : constant String := Get_Name (What);
   begin
      if Utils.Dry_Run ("enabling " & The_Node) then
         return;
      end if;
      Append (Args, "qmod");
      Append (Args, "-e");
      Append (Args, To_POSIX_String ("*@" & The_Node));
      Debug ("enabling " & The_Node);
      Open_Template (Template);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => "/cm/shared/apps/sge/current/bin/lx26-amd64/qmod",
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create qmod process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in qmod";
         when others => raise Subcommand_Error with "qmod exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Enable;

   procedure Disable (What : Nodes.Node) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
      The_Node     : constant String := Get_Name (What);
   begin
      if Utils.Dry_Run ("disabling " & The_Node) then
         return;
      end if;
      Append (Args, "qmod");
      Append (Args, "-d");
      Append (Args, To_POSIX_String ("*@" & The_Node));
      Debug ("disabling " & The_Node);
      Open_Template (Template);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => "/cm/shared/apps/sge/current/bin/lx26-amd64/qmod",
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create qmod process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in qmod";
         when others => raise Subcommand_Error with "qmod exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Disable;

   procedure Poweron (What : Nodes.Node) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
      The_Node     : constant String := Get_Name (What);
   begin
      Statistics.Node_Switched_On;
      if Utils.Dry_Run ("switching on " & The_Node) then
         return;
      end if;
      Append (Args, "cmsh");
      Append (Args, "-c");
      Append (Args, To_POSIX_String ("device power -n " & The_Node & " on"));
      Debug ("switching on " & The_Node);
      Open_Template (Template);
      Start_Process_Search (Child    => PID,
                            Template => Template,
                            Filename => "cmsh",
                            Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create cmsh process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in cmsh";
         when others => raise Subcommand_Error with "cmsh exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Poweron;

   procedure Poweroff (What : Nodes.Node) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
      The_Node     : constant String := Get_Name (What);
   begin
      Statistics.Node_Switched_Off;
      if Utils.Dry_Run ("switching off " & The_Node) then
         return;
      end if;
      Append (Args, "cmsh");
      Append (Args, "-c");
      Append (Args, To_POSIX_String ("device power -n " & The_Node & " off"));
      Debug ("switching off " & The_Node);
      Open_Template (Template);
      Start_Process_Search (Child    => PID,
                            Template => Template,
                            Filename => "cmsh",
                            Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create cmsh process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in cmsh";
         when others => raise Subcommand_Error with "cmsh exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Poweroff;

   ---------------------
   -- Try_To_Poweroff --
   ---------------------

   procedure Try_To_Poweroff (The_Node : Nodes.Node; Succeeded : out Boolean) is
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

end Actions;
