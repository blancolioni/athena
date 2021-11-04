package body Athena.Technology is

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Minerva.Technology.Technology_Class is
   begin
      return Minerva.Technology.Get_By_Tag (Tag);
   end Get;

end Athena.Technology;
