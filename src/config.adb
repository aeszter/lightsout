with DOM; use DOM;
with DOM.Core; use DOM.Core;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Attrs; use DOM.Core.Attrs;
with DOM.Readers;
with Sax.Readers; use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Config is

   ----------
   -- Read --
   ----------

   function Read return Node_Groups.List is
      Reader : DOM.Readers.Tree_Reader;
      XML_Doc : Document;
      All_Nodes : Node_List;
      Config_Node, One_Node  : Node;
      File                   : File_Input;
      Group_List : Node_Groups.List;
   begin
      Open (Filename => "/etc/lights-out.xml",
            Input => File);

      Reader.Set_Feature (Sax.Readers.Validation_Feature, False);
      Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      Reader.Parse (File);
      Close (Input => File);
      XML_Doc := Reader.Get_Tree;
      Config_Node := First_Child (XML_Doc);
      All_Nodes := Child_Nodes (Config_Node);

      if Name (Config_Node) /= "config" then
         raise Config_Error with "Found unexpected """ & Name (Config_Node)
           & """ at top level in config file";
      end if;

      for I in 0 .. Length (All_Nodes) - 1 loop
         One_Node := Item (All_Nodes, I);
         if Node_Name (One_Node) = "nodegroup" then
            declare
               New_Group : Node_Groups.Group;
               Group_Nodes : Node_List;
               Group_Node : Node;
            begin
               Group_Nodes := Child_Nodes (One_Node);
               for J in 0 .. Length (Group_Nodes) - 1 loop
                  Group_Node := Item (Group_Nodes, J);
                  if Name (Group_Node) = "delay" then
                     New_Group.Seconds_To_Keep_Online := Integer'Value (Value (First_Child (Group_Node)));
                  elsif Name (Group_Node) = "residents" then
                     New_Group.Number_To_Keep_Online := Integer'Value (Value (First_Child (Group_Node)));
                  elsif Name (Group_Node) = "nodename" then
                     New_Group.Host_Names.Append (To_Unbounded_String (Value (First_Child (Group_Node))));
                  elsif Name (Group_Node) = "#text" or else
                    Name (Group_Node) = "#comment" then
                     null; -- ignore
                  else
                     raise Config_Error with "Found unexpected """
                       & Name (Group_Node) & """ in <nodegroup>";
                  end if;
               end loop;
               if not New_Group.Host_Names.Is_Empty then
                  Group_List.Append (New_Group);
               else
                  Put_Line (Standard_Error, "Warning: empty node group ignored");
               end if;
            end;
         elsif Node_Name (One_Node) = "#text" or else
           Node_Name (One_Node) = "#comment" then
            null; -- ignore
         else
            raise Config_Error with "Found unexpected """
              & Node_Name (One_Node) & """ in <config> while reading config file";
         end if;
      end loop;

      return Group_List;
   end Read;

end Config;
