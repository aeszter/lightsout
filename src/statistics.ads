package Statistics is
   procedure Node_Seen;
   procedure Node_Switched_On;
   procedure Node_Switched_Off;
   procedure Too_Few_Nodes (Number : Natural);
   procedure Race;

   procedure Print;

private
   type Data is record
      Seen, Switched_On, Switched_Off, Too_Few, Races : Natural := 0;
   end record;

   Global_Stats : Data;
end Statistics;
