package Actions is
   procedure Enable (Node : String);
   procedure Disable (Node : String);
   procedure Poweron (Node : String);
   procedure Poweroff (Node : String);

   Subcommand_Error : exception;
private

end Actions;
