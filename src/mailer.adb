with POSIX; use POSIX;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX.IO;
with Ada.Exceptions; use Ada.Exceptions;

with Config;
with Utils;
with CM.Taint; use CM.Taint;

package body Mailer is

   Mailer_Error : exception;




   procedure Send (Node_Name : CM.Taint.Trusted_String; Message : String) is
      Args           : POSIX.POSIX_String_List;
      Template       : Process_Template;
      PID            : Process_ID;
      Return_Value   : Termination_Status;
      Pipe,
      From_Lightsout : POSIX.IO.File_Descriptor;
      Buffer         : POSIX.IO.IO_Buffer (1 .. Message'Length + 1);
      Last           : IO_Count;

   begin
      if Config.Bugzilla_Writable then
         return;
      end if;
      begin
         Append (Args, "/usr/bin/mail");
         Append (Args, "-s");
         Append (Args, To_POSIX_String (Value (Node_Name)));
         Append (Args, To_POSIX_String (Config.Get_Mailto));
         POSIX.IO.Create_Pipe (Read_End  => From_Lightsout,
                               Write_End => Pipe);
         Open_Template (Template);
         Set_File_Action_To_Close (Template => Template,
                                   File     => Pipe);
         Set_File_Action_To_Duplicate (Template  => Template,
                                       File      => POSIX.IO.Standard_Input,
                                       From_File => From_Lightsout);

      exception
         when others =>
            Utils.Debug ("in mail setup:");
            raise;
      end;
      begin
         Start_Process (Child    => PID,
                        Template => Template,
                        Pathname => "/usr/bin/mail",
                        Arg_List => Args);
         POSIX.IO.Close (From_Lightsout);
      exception
         when others =>
            Utils.Debug ("in mail start:");
            raise;
      end;
      begin
         Buffer (1 .. Message'Last) := To_POSIX_String (Message);
         Buffer (Message'Last + 1) := LF;
         POSIX.IO.Write (File   => Pipe,
                         Buffer => Buffer,
                         Last   => Last);
         POSIX.IO.Close (Pipe);
         Wait_For_Child_Process (Status => Return_Value, Child => PID);
      exception
         when others =>
            Utils.Debug ("in mail wait:");
            raise;
      end;
      case Termination_Cause_Of (Return_Value) is
         when Exited =>
            case Exit_Status_Of (Return_Value) is
               when Normal_Exit => return;
               when Failed_Creation_Exit => raise Mailer_Error with "Failed to create mail process";
               when Unhandled_Exception_Exit => raise Mailer_Error with "Unhandled exception in mail";
               when others => raise Mailer_Error with "mail exited with status" & Exit_Status_Of (Return_Value)'Img;
            end case;
         when Terminated_By_Signal =>
            Utils.Debug ("mail terminated by signal " & Termination_Signal_Of (Return_Value)'Img);
         when Stopped_By_Signal =>
            Utils.Debug ("mail stopped");
      end case;
   exception
      when E : POSIX_Error =>
         raise Mailer_Error with "mail raised error: "
            & Exception_Information (E);
   end Send;



end Mailer;
