with Minerva.Technology;

package Athena.Technology is

   function Get (Tag : String) return Minerva.Technology.Technology_Class;

   function Drive return Minerva.Technology.Technology_Class
   is (Get ("drive"));

end Athena.Technology;
