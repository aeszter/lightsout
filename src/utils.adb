with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with POSIX.Process_Primitives;

package body Utils is

   -----------
   -- Debug --
   -----------

   procedure Debug (Message : String) is
   begin
      if Debug_Enabled then
         Put_Line (Standard_Error, Message);
      end if;
   end Debug;

   ---------------------
   -- Verbose_Message --
   ---------------------

   procedure Verbose_Message (Message : String) is
   begin
      if Verbose then
         Put_Line (Message);
      end if;
   end Verbose_Message;

   ------------------
   -- Enable_Debug --
   ------------------

   procedure Enable_Debug is
   begin
      Debug_Enabled := True;
   end Enable_Debug;


   -------------------
   -- Check_Options --
   -------------------

   procedure Check_Options is
   begin
      for Arg in 1 .. Argument_Count loop
         if Argument (Arg) = "-d" or else
           Argument (Arg) = "--debug" then
            Debug_Enabled := True;
         elsif Argument (Arg) = "-n" or else
           Argument (Arg) = "--no-action" then
            Action := False;
         elsif Argument (Arg) = "-c" or else
           Argument (Arg) = "--check-config" then
            Config_Only := True;
         elsif Argument (Arg) = "-v" or else
           Argument (Arg) = "--verbose" then
            Verbose := True;
         elsif Argument (Arg) = "-h" or else
           Argument (Arg) = "--help" then
            Ada.Text_IO.Put_Line ("Options may be given in full or with a single hyphen "
                                  & "and the first letter only");
            Ada.Text_IO.Put_Line ("--debug gives debugging output");
            Ada.Text_IO.Put_Line ("--verbose states which nodes are switched on/off");
            Ada.Text_IO.Put_Line ("--no-action only goes through the motions "
                                  & "without actually calling qmod or cmsh; "
                                  & " implies --verbose");
            Ada.Text_IO.Put_Line ("--check-config only checks the config file, "
                                  & "then terminates the program");
            Ada.Text_IO.Put_Line ("--help shows this message, then terminates");
            POSIX.Process_Primitives.Exit_Process;
         else
            raise Program_Error with "unknown option: " & Argument (Arg);
         end if;
      end loop;
   end Check_Options;

   -------------
   -- Dry_Run --
   -------------

   function Dry_Run (Message         : String;
                     Show_On_Verbose : Boolean := True) return Boolean is
   begin
      if not Action or else (Verbose and Show_On_Verbose) then
         Ada.Text_IO.Put_Line (Message);
      end if;
      return not Action;
   end Dry_Run;

   function Terminate_After_Config return Boolean is
   begin
      return Config_Only;
   end Terminate_After_Config;

end Utils;
