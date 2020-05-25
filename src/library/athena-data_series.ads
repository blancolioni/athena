private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Finalization;

package Athena.Data_Series is

   type Series is limited private;

   procedure Add_Point
     (Container : in out Series;
      X, Y      : Real);

   function Variance (Container : Series) return Real;
   function Covariance (Container : Series) return Real;

   function X_Mean (Container : Series) return Real;
   function Y_Mean (Container : Series) return Real;

   type Regression is private;

   function Simple_Linear_Regression
     (Container : Series)
      return Regression;

   function Has_X_Intercept (Item : Regression) return Boolean;
   function X_Intercept (Item : Regression) return Real;
   function Gradient (Item : Regression) return Real;

private

   type Data_Point is
      record
         X : Real;
         Y : Real;
      end record;

   package Data_Series_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Data_Point);

   type Series_Properties is
      record
         Variance, Covariance : Real := 0.0;
         X_Mean, Y_Mean       : Real := 0.0;
         Up_To_Date           : Boolean := True;
      end record;

   type Series_Properties_Access is access Series_Properties;

   type Series is
   limited new Ada.Finalization.Limited_Controlled with
      record
         List  : Data_Series_Lists.List;
         Props : Series_Properties_Access;
      end record;

   overriding procedure Initialize (Item : in out Series);
   overriding procedure Finalize (Item : in out Series);

   type Regression is
      record
         A, B : Real;
      end record;

   function Gradient (Item : Regression) return Real
   is (Item.B);

end Athena.Data_Series;
