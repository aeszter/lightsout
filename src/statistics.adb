with Ada.Text_IO;
package body Statistics is

   ---------------
   -- Node_Seen --
   ---------------

   procedure Node_Seen is
   begin
      Global_Stats.Seen := Global_Stats.Seen + 1;
   end Node_Seen;

   ----------------------
   -- Node_Switched_On --
   ----------------------

   procedure Node_Switched_On is
   begin
      Global_Stats.Switched_On := Global_Stats.Switched_On + 1;
   end Node_Switched_On;

   -----------------------
   -- Node_Switched_Off --
   -----------------------

   procedure Node_Switched_Off is
   begin
      Global_Stats.Switched_Off := Global_Stats.Switched_Off + 1;
   end Node_Switched_Off;

   -------------------
   -- Too_Few_Nodes --
   -------------------

   procedure Too_Few_Nodes (Number : Natural) is
   begin
      Global_Stats.Too_Few := Global_Stats.Too_Few + Number;
   end Too_Few_Nodes;

   ----------
   -- Race --
   ----------

   procedure Race is
   begin
      Global_Stats.Races := Global_Stats.Races + 1;
   end Race;

   -----------
   -- Print --
   -----------

   procedure Print is
   begin
      Ada.Text_IO.Put_Line ("Statistics:");
      Ada.Text_IO.Put_Line (Global_Stats.Seen'Img & " Nodes seen");
      if Global_Stats.Switched_Off > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.Switched_Off'Img & " Nodes switched off");
      end if;
      if Global_Stats.Switched_On > 0 or else
         Global_Stats.Too_Few > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.Switched_On'Img & " Nodes switched on");
         Ada.Text_IO.Put_Line (Global_Stats.Too_Few'Img
                               & " Nodes not switched on because there were none");
      end if;
      if Global_Stats.Races > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.Races'Img & " races with the scheduler lost");
      end if;
   end Print;

end Statistics;
