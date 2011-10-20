with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils.String_Lists;
with Ada.Text_IO; use Ada.Text_IO;

package body Node_Groups is

   ------------
   -- Manage --
   ------------

   procedure Manage (What : Lists.Cursor) is
      The_Group  : constant Group := Lists.Element (What);
      Idle_Counter : Integer := -The_Group.Number_To_Keep_Online;
      Index      : Utils.String_Lists.Cursor :=  The_Group.Host_Names.First;
      Nodes_To_Switch_On : Natural;
   begin
      while Index /= No_Element loop
         Check_Node (What                       => Index,
                     Idle_Count_Above_Threshold => Idle_Counter);
         Next (Index);
      end loop;

      if Idle_Counter < 0 then
         Nodes_To_Switch_On := -Idle_Counter;
         Index := Lists.Element (What).Host_Names.First;
         Switch_On :
         while Index /= No_Element loop
            declare
               The_Node : constant String := To_String (Element (Index));
               -- crashes here
               pragma Compile_Time_Warning (True, "crashes");
            begin
               if Nodes_To_Switch_On > 0 and then
                not Is_Online (Node => The_Node) then
                  Poweron (Node => The_Node);
                  Enable (Node => The_Node);
                  Nodes_To_Switch_On := Nodes_To_Switch_On - 1;
               end if;
               if Nodes_To_Switch_On = 0 then
                  exit Switch_On;
               end if;
               Next (Index);
            end;
         end loop Switch_On;
      end if;
   end Manage;

   procedure Check_Node (What       : Utils.String_Lists.Cursor;
                         Idle_Count_Above_Threshold : in out Integer) is
      The_Node : constant String := To_String (Element (What));
      Was_Disabled : Boolean := Is_Disabled (Node => The_Node);
   begin
      if not Is_Online (Node => The_Node) or else
       not Is_Idle (Node => The_Node) then
         return;
      end if;
      -- check idle time against TTKO
      Idle_Count_Above_Threshold := Idle_Count_Above_Threshold + 1;
      if Idle_Count_Above_Threshold <= 0 then
         return; -- keep node online
      end if;
      Was_Disabled := Is_Disabled (Node => The_Node);
      Disable (Node => The_Node);
      if not Is_Idle (Node => The_Node) then
         -- the scheduler has been faster
         if not Was_Disabled then
            Enable (Node => The_Node);
         end if;
         return;
      end if;
      Poweroff (Node => The_Node);
   end Check_Node;

   procedure Enable (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Enabling " & Node);
   end Enable;

   procedure Disable (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Disabling " & Node);
   end Disable;

   procedure Poweron (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Switching on " & Node);

   end Poweron;

   procedure Poweroff (Node : String) is
   begin
      Put_Line (File => Standard_Error,
                Item => "Switching off " & Node);
   end Poweroff;

   function Is_Disabled (Node : String) return Boolean is
   begin
      Put_Line (File => Standard_Error,
                Item => "Assume enabled " & Node);
      return False;
   end Is_Disabled;

   function Is_Online (Node : String) return Boolean is
   begin
      Put_Line (File => Standard_Error,
                Item => "Assume online " & Node);
      return True;
   end Is_Online;

   function Is_Idle (Node : String) return Boolean is
   begin
      -- well, this one will have to be real
      -- call qstat -f -q *@Node -xml
      -- extract slots_used and slots_resv
      -- if any of these is >0, return false
      -- otherwise, return true
      -- for now:
      return False;
   end Is_Idle;


end Node_Groups;
