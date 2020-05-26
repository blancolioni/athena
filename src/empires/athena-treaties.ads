with Athena.Handles.Empire;
with Athena.Handles.War;

package Athena.Treaties is

   function At_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
      return Boolean;

   function Get_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.War.War_Handle;

   procedure Declare_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle);

end Athena.Treaties;
