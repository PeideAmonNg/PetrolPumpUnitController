with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.discrete_Random;

package body Petrol_Pump_Unit with
SPARK_Mode,
Refined_State => (State => (Pumps), Internal_State => (Cash_Register, Total_Cash_Invariant))
is

   type Pumps_Array is array (Pump_Type'Range) of Pump;

   Pumps : Pumps_Array := (Pump91 => (Pump91, 206, 2_000 * 206, Base, 0),
                           Pump95 => (Pump95, 215, 2_000 * 215, Base, 0),
                           PumpDiesel => (PumpDiesel, 136, 2_000 * 136, Base, 0));

   Cash_Register: Cash_Register_Cent := 0;

   Total_Cash_Invariant : General_Cent := General_Cent(Pumps(Pump91).Reservoir) + General_Cent(Pumps(Pump95).Reservoir) + General_Cent(Pumps(PumpDiesel).Reservoir) +
     General_Cent(Cash_Register) +
     General_Cent(Pumps(Pump91).Outstanding_Amount) + General_Cent(Pumps(Pump95).Outstanding_Amount) +
       General_Cent(Pumps(PumpDiesel).Outstanding_Amount);



   function Get_Reservoir(P: Pump_Type) return Reservoir_Cent with
     Refined_Global => (Input => Pumps)
   is
   begin
      return Pumps(P).Reservoir;
   end;

   function Get_Total_Reservoir return General_Cent with
     Refined_Global => (Input => Pumps)
   is
   begin
      return General_Cent(Get_Reservoir(Pump91)) + General_Cent(Get_Reservoir(Pump95)) + General_Cent(Get_Reservoir(PumpDiesel));
   end;

   function Get_Outstanding_Amount(P: Pump_Type) return Cent with
     Refined_Global => (Input => Pumps)
   is
   begin
      return Pumps(P).Outstanding_Amount;
   end;

   function Get_Total_Outstanding_Amount return General_Cent with
     Refined_Global => (Input => Pumps)
   is
   begin
      return General_Cent(Get_Outstanding_Amount(Pump91)) + General_Cent(Get_Outstanding_Amount(Pump95)) +
        General_Cent(Get_Outstanding_Amount(PumpDiesel));
   end;

   function Get_Price(P: Pump_Type) return General_Cent is
   begin
      return Pumps(P).Price;
   end;

   function Get_State(P: Pump_Type) return Pump_State is
   begin
      return Pumps(P).State;
   end;

   procedure Print_State(P: Pump_Type) is
   begin
      Put_Line(Pump_State'Image(Pumps(p).State));
   end;

   function Get_Cash_Register return Cash_Register_Cent with
     Refined_Global => (Input => Cash_Register)
   is
   begin
      return Cash_Register;
   end;

   function Calculate_Total_Value return General_Cent
   is
   begin
      return General_Cent(Get_Total_Reservoir) + General_Cent(Get_Cash_Register) + General_Cent(Get_Total_Outstanding_Amount);
   end;

   function Get_Total_Cash_Invariant return General_Cent with
     Refined_Global => (Input => Total_Cash_Invariant)
   is
   begin
      return Total_Cash_Invariant;
   end;

   function Test_Cash_Invariant return Boolean with
     Refined_Global => (Input => (Pumps, Cash_Register, Total_Cash_Invariant))
   is
   begin
      return Calculate_Total_Value = General_Cent(Total_Cash_Invariant);
   end;



   function All_Pumps_Paid return Boolean is
      Amount_Owing : General_Cent := 0;
   begin
      for P in Pumps'Range loop
         Amount_Owing := Amount_Owing + General_Cent(Pumps(P).Outstanding_Amount);
      end loop;

      return Amount_Owing = 0;
   end;








   procedure Lift(P: Pump_Type)
   is
   begin
      Pumps(p).State := Ready;
      Print_State(P);
   end lift;

   procedure Start(p: Pump_Type; Amount_Paid: Cent) is
   begin
      Pumps(p).state := Pumping;
      Pumps(P).Reservoir := Get_Reservoir(P) - Reservoir_Cent(Amount_Paid);
--        Pumps(P).Outstanding_amount := Outstanding_Amount_Cent(Amount_Paid);
      Pumps(P).Outstanding_amount := Amount_Paid;
      Print_State(P);
--        Put_Line("Amount_Of_Petrol: " & Litre'Image(Amount_Of_Petrol));
--        Put_Line("Pumps(P).Reservoir: " & Litre'Image(Pumps(P).Reservoir));
--        Put_Line("Pumps(P).Reservoir: " & Litre'Image(Get_Reservoir(P)));
   end start;


   procedure Stop(p: Pump_Type) is
   begin
      Pumps(p).State := Ready;
      Print_State(P);
   end stop;

   procedure Replace(p: Pump_Type) is
   begin
      Pumps(p).State := Waiting;
      Print_State(P);
   end replace;

   procedure Pay(P: Pump_Type; Amount_Paid: Cent) is
--        Amount_Of_Petrol_Paid : Litre;
   begin
--        Amount_Of_Petrol_Paid := Calculate_Petrol_Paid(P, Amount_Paid);
      Pumps(P).State := Base;
      Pumps(P).Outstanding_Amount := Pumps(P).Outstanding_Amount - Cent(Amount_Paid);
      Put_Line("Amount_Paid: " & Cent'Image(Amount_Paid));
      Cash_Register := Cash_Register + Cash_Register_Cent(Amount_Paid);
      Print_State(P);
   end pay;


end;
