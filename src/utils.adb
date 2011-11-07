with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

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

   ------------------
   -- Enable_Debug --
   ------------------

   procedure Enable_Debug is
   begin
      Debug_Enabled := True;
   end Enable_Debug;

   ----------------------
   -- Check_Debug_Flag --
   ----------------------

   procedure Check_Debug_Flag is
   begin
      for Arg in 1 .. Argument_Count loop
         if Argument (Arg) = "-d" or else
          Argument (Arg) = "--debug" then
               Debug_Enabled := True;
               return;
         end if;
      end loop;
   end Check_Debug_Flag;

end Utils;
