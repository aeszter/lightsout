with Node_Groups;

package Config is
   function Read return Node_Groups.List;
   Config_Error : exception;
end Config;
