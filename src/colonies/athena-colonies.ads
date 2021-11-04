with Athena.Real_Images;

with Minerva.Colony;
with Minerva.Empire;
with Minerva.Star;

with Minerva.Db;

package Athena.Colonies is

   subtype Colony_Class is Minerva.Colony.Colony_Class;

   function Get_Colony
     (Star : Minerva.Star.Star_Class)
      return Colony_Class;

   procedure Capture_Colony
     (Colony    : Colony_Class;
      New_Owner : Minerva.Empire.Empire_Class);

   function Best_Colony
     (Owned_By : Minerva.Empire.Empire_Class;
      Score    : not null access
        function (Colony : Colony_Class) return Real)
      return Colony_Class;

   function Find_Colony
     (Owned_By : Minerva.Empire.Empire_Class;
      Test     : not null access function
        (Colony : Minerva.Colony.Colony_Class) return Boolean)
      return Minerva.Colony.Colony_Class;

   procedure Remove_Cargo
     (Colony   : Colony_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : in out Non_Negative_Real)
     with Post => Quantity <= Quantity'Old;

   procedure Add_Cargo
     (Colony   : Colony_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : in out Non_Negative_Real)
     with Post => Quantity <= Quantity'Old;

   procedure New_Colony
     (Star        : Minerva.Star.Star_Class;
      Empire      : Minerva.Empire.Empire_Class;
      Initial_Pop : in out Non_Negative_Real)
     with Post => Initial_Pop <= Initial_Pop'Old;

   procedure Produce_Material
     (Colony   : Colony_Class;
      Quantity : Non_Negative_Real);

   procedure Request_Material
     (Colony   : Colony_Class;
      Material : in out Non_Negative_Real)
     with Post => Material <= Material'Old;

   procedure Request_Construct
     (Colony    : Colony_Class;
      Construct : in out Non_Negative_Real)
     with Post => Construct <= Construct'Old;

   function Can_Provide
     (Colony      : Colony_Class;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0)
      return Boolean;

   procedure Use_Assets
     (Colony      : Colony_Class;
      Description : String;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0);

   procedure Build_Industry
     (Colony   : Colony_Class;
      Priority : Integer;
      Quantity : Non_Negative_Real);

   procedure Produce_Material
     (Colony   : Colony_Class;
      Priority : Integer;
      Quantity : Non_Negative_Real)
   is null;

   procedure Log_Colony
     (Colony  : Colony_Class;
      Message : String);

private

   function Image (X : Real) return String
                      renames Athena.Real_Images.Approximate_Image;

end Athena.Colonies;
