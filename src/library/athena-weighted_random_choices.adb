with WL.Random;

package body Athena.Weighted_Random_Choices is

   ------------
   -- Choose --
   ------------

   function Choose
     (Set : Weighted_Choice_Set'Class)
      return Element_Type
   is
      X : Positive := WL.Random.Random_Number (1, Set.Total_Score);
   begin
      for Choice of Set.Vector loop
         if X <= Choice.Score then
            return Choice.Element;
         else
            X := X - Choice.Score;
         end if;
      end loop;

      raise Program_Error;

   end Choose;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Set    : in out Weighted_Choice_Set'Class;
      Item   : Element_Type;
      Score  : Natural)
   is
   begin
      if Score > 0 then
         Set.Vector.Append ((Item, Score));
         Set.Total_Score := Set.Total_Score + Score;
      end if;
   end Insert;

end Athena.Weighted_Random_Choices;
