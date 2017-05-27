with Ada.Text_IO;
with Petrol_Pump_Unit; use Petrol_Pump_Unit;

procedure Petrol_User is
   package IO renames Ada.Text_IO;

   F : Float := 4.27* Float(210);
   Amount_Paid : Cent;

   procedure Assert_Pass is --This is a typical successful petrol pump transaction.
   begin
      lift(Pump95);
      replace(Pump95);
      lift(Pump95);
      Amount_Paid := Amount_Paid + 3733 ;
      Start(Pump95, Amount_Paid);
      Stop(Pump95);
      Replace(Pump95);
      IO.Put_Line(Cent'Image(Get_Outstanding_Amount(Pump95)));
      Pay(Pump95, Amount_Paid);
      IO.Put_Line(Cent'Image(Get_Outstanding_Amount(Pump95)));
      IO.Put_Line(General_Cent'Image(Get_Total_Cash_Invariant));
   end;

   procedure Assert_Fail is
   begin
      lift(Pump95);
      Stop(Pump95); --Invalid Stop pre state.
   end;

   procedure Assert_Fail_2 is
   begin
      lift(Pump95);
      lift(Pump95); --Invalid Lift pre state.
   end;

   procedure Assert_Fail_3 is
   begin
      Start(Pump95, 10000); --Invalid Start pre state.
   end;

   procedure Assert_Fail_4 is
   begin
      lift(Pump95);
      Amount_Paid := Amount_Paid + 3733 ;
      Start(Pump95, Amount_Paid);
      Stop(Pump95);
      Replace(Pump95);
      Pay(Pump95, Amount_Paid -10); --Failed precondition: Paid less than outstanding amount.
   end;

begin
   Assert_Pass;
   Assert_Pass;
--     Assert_Fail;
--     Assert_Fail_2;
--     Assert_Fail_3;
--     Assert_Fail_4;

end;
