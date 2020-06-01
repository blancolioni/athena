package Athena.Economy is

   type Economic_Sector_Type is
     (Agriculture,
      Commercial,
      Government,
      Industry,
      Mining,
      Service);

   type Economy_Type is private;

   function Sector_Size
     (Economy : Economy_Type;
      Sector  : Economic_Sector_Type)
      return Non_Negative_Real;

   function Sector_Employment
     (Economy : Economy_Type;
      Sector  : Economic_Sector_Type)
      return Non_Negative_Real;

private

   type Sector_Record is
      record
         Employed   : Non_Negative_Real;
         Production : Non_Negative_Real;
      end record;

   type Sector_Array is array (Economic_Sector_Type) of Sector_Record;

   type Economy_Type is
      record
         Sectors : Sector_Array;
      end record;

end Athena.Economy;
