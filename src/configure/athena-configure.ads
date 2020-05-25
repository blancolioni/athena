with WL.Random.Names;

package Athena.Configure is

   procedure Initialize_Database;

   procedure Create_Galaxy
     (Radius_X, Radius_Y : Non_Negative_Real;
      Star_Count         : Positive;
      Name_Generator     : WL.Random.Names.Name_Generator);

end Athena.Configure;
