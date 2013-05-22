with DOM.Core; use DOM.Core;
with DOM.Readers;
with Sax.Readers;
with Pipe_Streams; use Pipe_Streams;
with Pipe_Commands; use Pipe_Commands;

package body Parser is

   -----------
   -- Setup --
   -----------
   function Setup (Command  : String := "qstat";
                          Selector : String) return Document is
      Reader      : DOM.Readers.Tree_Reader;
      SGE_Command : Pipe_Stream;
      Command_String : constant String := sgeroot
        & "/bin/linux-x64/" & Command & " " & Selector & " -xml";
   begin
      SGE_Command.Set_Public_Id ("qstat");
      SGE_Command.execute ("SGE_ROOT=" & sgeroot & " " &
                           Command_String
                           & ASCII.NUL,
                           Pipe_Commands.read_file);
      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (SGE_Command);
      SGE_Command.Close;
      Wait_For_Children;
      return Reader.Get_Tree;


   end Setup;
end Parser;
