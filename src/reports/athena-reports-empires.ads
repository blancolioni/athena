with Athena.Handles.Empire;

package Athena.Reports.Empires is

   procedure Report
     (Writer : in out Writer_Interface'Class;
      Empire : Athena.Handles.Empire.Empire_Handle);

end Athena.Reports.Empires;
