with XMLrpc.Client;
with XMLrpc.Message.Payload;
with XMLrpc.Message.Response;
with XMLrpc.Parameters; use XMLrpc.Parameters;
with XMLrpc.Types; use XMLrpc;
use XMLrpc.Types;
with Utils;
with Config;

package body Bugzilla is

   -----------------
   -- Add_Comment --
   -----------------

   procedure Add_Comment (Bug_ID : Positive; Comment : String) is
      Set : constant Types.Object_Set := (
                 +S (V    => "lightsout@owl-master.mpibpc.intern",
                     Name => "Bugzilla_login"),
                 +S (V    => "ejg5RofkgNhskwcDl",
                     Name => "Bugzilla_password"),
                 +I (V    => Bug_ID,
                     Name => "id"),
                 +S (V    => Comment,
                     Name => "comment"));
      Params : constant Parameters.List := +R (V => Set,
                                   Name => "Params");
      Payload : constant XMLrpc.Message.Payload.Object
        := XMLrpc.Message.Payload.Build ("Bug.add_comment", Params);
   begin
      if Utils.Dry_Run ("Talking to Bugzilla") then
         return;
      end if;
      declare
         Response : constant XMLrpc.Message.Response.Object'Class
             := XMLrpc.Client.Call (Config.Get_Bugzilla & "/xmlrpc.cgi", Payload);

         Replied  : constant Parameters.List := XMLrpc.Message.Parameters (Response);
      begin
         if XMLrpc.Parameters.Exist (Replied, "faultCode") then
            raise Error with XMLrpc.Parameters.Get (Replied, "faultString");
         end if;
      end;
   end Add_Comment;

end Bugzilla;
