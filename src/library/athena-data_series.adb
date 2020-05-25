with Ada.Unchecked_Deallocation;

package body Athena.Data_Series is

   procedure Check_Properties (Container : Series);

   ---------------
   -- Add_Point --
   ---------------

   procedure Add_Point
     (Container : in out Series;
      X, Y      : Real)
   is
   begin
      Container.List.Append ((X, Y));
      Container.Props.Up_To_Date := False;
   end Add_Point;

   ----------------------
   -- Check_Properties --
   ----------------------

   procedure Check_Properties (Container : Series) is
      Sum_XY : Real := 0.0;
      Sum_X  : Real := 0.0;
      Sum_Y  : Real := 0.0;
      Sum_XX : Real := 0.0;
      N      : constant Non_Negative_Real :=
                 Non_Negative_Real (Container.List.Length);
   begin
      if Container.Props.Up_To_Date then
         return;
      end if;

      if not Container.List.Is_Empty then
         for P of Container.List loop
            Sum_XX := Sum_XX + P.X ** 2;
            Sum_X  := Sum_X + P.X;
            Sum_XY := Sum_XY + P.X * P.Y;
            Sum_Y  := Sum_Y + P.Y;
         end loop;
      end if;

      Container.Props.all := Series_Properties'
        (Variance   => Sum_XX - Sum_X ** 2 / N,
         Covariance => Sum_XY - Sum_X * Sum_Y / N,
         X_Mean     => Sum_X / N,
         Y_Mean     => Sum_Y / N,
         Up_To_Date => True);

   end Check_Properties;

   ----------------
   -- Covariance --
   ----------------

   function Covariance (Container : Series) return Real is
   begin
      Check_Properties (Container);
      return Container.Props.Covariance;
   end Covariance;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Series) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Series_Properties, Series_Properties_Access);
   begin
      Free (Item.Props);
   end Finalize;

   ---------------------
   -- Has_X_Intercept --
   ---------------------

   function Has_X_Intercept (Item : Regression) return Boolean is
   begin
      return Item.B /= 0.0;
   end Has_X_Intercept;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Item : in out Series) is
   begin
      Item.Props := new Series_Properties;
   end Initialize;

   ------------------------------
   -- Simple_Linear_Regression --
   ------------------------------

   function Simple_Linear_Regression
     (Container : Series)
      return Regression
   is
   begin
      return R : Regression do
         if Container.List.Is_Empty then
            R := (0.0, 0.0);
         else
            R.B := (if Variance (Container) = 0.0
                    then 0.0
                    else Covariance (Container) / Variance (Container));
            R.A := Y_Mean (Container) - R.B * X_Mean (Container);
         end if;
      end return;
   end Simple_Linear_Regression;

   --------------
   -- Variance --
   --------------

   function Variance (Container : Series) return Real is
   begin
      Check_Properties (Container);
      return Container.Props.Variance;
   end Variance;

   -----------------
   -- X_Intercept --
   -----------------

   function X_Intercept (Item : Regression) return Real is
   begin
      return -Item.A / Item.B;
   end X_Intercept;

   ------------
   -- X_Mean --
   ------------

   function X_Mean (Container : Series) return Real is
   begin
      Check_Properties (Container);
      return Container.Props.X_Mean;
   end X_Mean;

   ------------
   -- Y_Mean --
   ------------

   function Y_Mean (Container : Series) return Real is
   begin
      Check_Properties (Container);
      return Container.Props.Y_Mean;
   end Y_Mean;

end Athena.Data_Series;
