with AWS.Client;
with AWS.Response; use AWS.Response;
with SOAP.Message.Payload;
with SOAP.Client;
with Ada.Text_IO; use Ada.Text_IO;
with SOAP.Message.Response;
with SOAP.Parameters;
with SOAP.Types;

package body Bugzilla is

   ----------
   -- Test --
   ----------

   procedure Test is
      Params : constant SOAP.Parameters.List := SOAP.Parameters."+" (SOAP.Types.N);
      Payload : constant SOAP.Message.Payload.Object := SOAP.Message.Payload.Build ("Bugzilla.version", Params);
      Response : constant SOAP.Message.Response.Object'Class := SOAP.Client.Call ("http://ram.mpibpc.intern/bugzilla/xmlrpc.cgi", Payload);
      Replied  : constant SOAP.Parameters.List := SOAP.Message.Parameters (Response);
   begin
      Put_Line (AWS.Response.Message_Body (AWS.Client.Get (URL => "http://ram.mpibpc.intern/bugzilla")));
      for I in 1 .. SOAP.Parameters.Argument_Count (Replied) loop
         Put_Line (SOAP.Types.Image (SOAP.Parameters.Argument (P => Replied, N => I)));
      end loop;
   end Test;

end Bugzilla;
