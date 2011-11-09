with POSIX; use POSIX;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with Utils; use Utils;

package body Actions is

   procedure Enable (Node : String) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
   begin
      Append (Args, "qmod");
      Append (Args, "-e");
      Append (Args, To_POSIX_String ("*@" & Node));
      Debug ("enabling " & Node);
      Open_Template (Template);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => "/cm/shared/apps/sge/current/bin/lx26-amd64/qmod",
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create child process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in child process";
         when others => raise Subcommand_Error with "Child exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Enable;

   procedure Disable (Node : String) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
   begin
      Append (Args, "qmod");
      Append (Args, "-d");
      Append (Args, To_POSIX_String ("*@" & Node));
      Debug ("disabling " & Node);
      Open_Template (Template);
      Start_Process (Child    => PID,
                     Template => Template,
                     Pathname => "/cm/shared/apps/sge/current/bin/lx26-amd64/qmod",
                     Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create child process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in child process";
         when others => raise Subcommand_Error with "Child exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Disable;

   procedure Poweron (Node : String) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
   begin
      Append (Args, "cmsh");
      Append (Args, "-c");
      Append (Args, To_POSIX_String ("device power -n " & Node & " on"));
      Debug ("switching on " & Node);
      Open_Template (Template);
      Start_Process_Search (Child    => PID,
                            Template => Template,
                            Filename => "cmsh",
                            Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create child process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in child process";
         when others => raise Subcommand_Error with "Child exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Poweron;

   procedure Poweroff (Node : String) is
      PID          : Process_ID;
      Return_Value : Termination_Status;
      Args         : POSIX.POSIX_String_List;
      Template     : Process_Template;
   begin
      Append (Args, "cmsh");
      Append (Args, "-c");
      Append (Args, To_POSIX_String ("device power -n " & Node & "off"));
      Debug ("switching off " & Node);
      Open_Template (Template);
      Start_Process_Search (Child    => PID,
                            Template => Template,
                            Filename => "cmsh",
                            Arg_List => Args);
      Wait_For_Child_Process (Status => Return_Value, Child => PID);
      case Exit_Status_Of (Return_Value) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Subcommand_Error with "Failed to create child process";
         when Unhandled_Exception_Exit => raise Subcommand_Error with "Unhandled exception in child process";
         when others => raise Subcommand_Error with "Child exited with status" & Exit_Status_Of (Return_Value)'Img;
      end case;
   end Poweroff;

end Actions;
