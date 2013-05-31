package Bugzilla is
   Error : exception;
   procedure Add_Comment (Bug_ID : Positive; Comment : String);
end Bugzilla;
