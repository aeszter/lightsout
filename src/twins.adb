with Actions;
with Hosts;
with Utils;

package body Twins is

   ----------
   -- Init --
   ----------

   procedure Init (What : in out Twin) is
   begin
      What.Sub_Nodes.Clear;
      What.PDU := PDU_Strings.To_Bounded_String ("undefined");
   end Init;

   -------------
   -- Set_PDU --
   -------------

   procedure Set_PDU (Where : in out Twin; Str : String) is
   begin
      Where.PDU := PDU_Strings.To_Bounded_String (Str);
   end Set_PDU;

   --------------
   -- Add_Host --
   --------------

   procedure Add_Host (Where : in out Twin; Host : String) is
      Sub_Node : Hosts.Host;
   begin
      Sub_Node.Init (Name     => Host,
                     Maintain => none,
                     Bug      => 0);
      Where.Sub_Nodes.Append (+Sub_Node);
      Utils.Debug ("Added """ & Host & """ to """ & Where.Get_Name & """");
   end Add_Host;

   overriding procedure Poweron (What : Twin) is
   begin
      Actions.Poweron (What.PDU);
   end Poweron;

   overriding procedure Poweroff (What : Twin) is
   begin
      Actions.Poweroff (What.PDU);
   end Poweroff;

   overriding procedure Enable (What : Twin) is
   begin
      What.Sub_Nodes.Iterate (Nodes.Enable'Access);
   end Enable;

   overriding procedure Disable (What : Twin) is
   begin
      What.Sub_Nodes.Iterate (Nodes.Disable'Access);
   end Disable;

   overriding procedure Query_Node (What     : Twin;
                                    Disabled, Online,
                                    Idle     : out Boolean) is
      D, O, I : Natural;
   begin
      if What.Sub_Nodes.Length = 0 then
         raise Twin_Error with "Query_Nodes found no sub-nodes";
      end if;
      -- use "unsafe" states here: the first sub_node that
      -- disagrees will change the attribute to the safe state
      What.Sub_Nodes.Query_Nodes (Disabled => D,
                                  Online   => O,
                                  Idle     => I);
      if I = What.Sub_Nodes.Length then
         Idle := True;
      else
         Idle := False;
      end if;
      if D = What.Sub_Nodes.Length then
         Disabled := True;
      else
         Disabled := False;
      end if;
      if O > 0 then
         Online := True;
         -- Boolean state does not fully represent the twin here
         -- but: if at least one sub_node is online, the PDU cannot be off
      else
         Online := False;
      end if;
      Utils.Debug ("Twin " & What.Get_Name & " has D =>" & D'Img
                   & Disabled'Img & ", O =>" & O'Img & Online'Img &
                   ", I =>" & I'Img & Idle'Img);
   end Query_Node;

end Twins;
