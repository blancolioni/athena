with Athena.Handles.Technology.Selections;

package body Athena.Technology is

   function Get_Tec
     (Tec_Tag : String)
      return Athena.Handles.Technology.Technology_Handle;

   function Cargo return Athena.Handles.Technology.Technology_Handle
   is (Get_Tec ("cargo"));

   function Drive return Athena.Handles.Technology.Technology_Handle
   is (Get_Tec ("drive"));

   function Shield return Athena.Handles.Technology.Technology_Handle
   is (Get_Tec ("shield"));

   function Weapon return Athena.Handles.Technology.Technology_Handle
   is (Get_Tec ("weapon"));

   -------------
   -- Get_Tec --
   -------------

   function Get_Tec
     (Tec_Tag : String)
      return Athena.Handles.Technology.Technology_Handle
   is
      use Athena.Handles.Technology.Selections;
   begin
      return First_Where (Tag = Tec_Tag);
   end Get_Tec;

end Athena.Technology;
