with AWS.Client;
with AWS.Response;
with Ada.Text_IO; use Ada.Text_IO;

package body Bugzilla is

   ----------
   -- Test --
   ----------

   procedure Test is
   begin
      Put_Line (AWS.Response.Message_Body (AWS.Client.Get (URL => "http://ram.mpibpc.intern/bugzilla")));
   end Test;

end Bugzilla;
