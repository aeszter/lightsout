with DOM.Core; with DOM.Core.Documents;


package Parser is
   subtype Tree is DOM.Core.Document;

   sgeroot : constant String := "/cm/shared/apps/sge/current";


   function Setup (Command  : String := "qstat";
                   Selector : String) return DOM.Core.Document;
   function Get_Elements_By_Tag_Name
     (Doc : DOM.Core.Document; Tag_Name : DOM.Core.DOM_String := "*")
      return DOM.Core.Node_List
      renames DOM.Core.Documents.Get_Elements_By_Tag_Name;


end Parser;
