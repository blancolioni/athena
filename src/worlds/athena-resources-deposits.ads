with Athena.Handles.Commodity;
with Athena.Handles.World;

package Athena.Resources.Deposits is

   function Score_Resource
     (Generator : Resource_Generator;
      World     : Athena.Handles.World.World_Handle)
      return Natural;

end Athena.Resources.Deposits;
