package Petrol_Pump_Unit with
SPARK_Mode,
Abstract_State => (State, Internal_State)
is
   pragma Assertion_Policy (Pre => Check, Post => Check);

   type Pump is tagged private;

   type Pump_Type is (Pump91, Pump95, PumpDiesel);
   type Pump_State is (Base, Waiting, Ready, Pumping);
   type General_Cent is range 0 .. Integer'Last; --General type for calculation
   type Cent is range 0 .. 1_000_00; --$1000 limit per pump transaction and per pump outstading amount.
   type Reservoir_Cent is range 0 .. 500_000_00; --100_000L fuel storage capacity, at $5/L
   type Cash_Register_Cent is range 0 .. 10_000_000_00; --$10m money limit in register.



   --Helper subprograms

   function Get_Reservoir(P: Pump_Type) return Reservoir_Cent with
     Global => (Input => State);

   function Get_Total_Reservoir return General_Cent with
     Global => (Input => State),
     Post => Get_Total_Reservoir'Result <= General_Cent(Reservoir_Cent'Last * 3);

   function Get_Outstanding_Amount(P: Pump_Type) return Cent with
     Global => (Input => State);

   function Get_Total_Outstanding_Amount return General_Cent with
     Global => (Input => State),
     Post => Get_Total_Outstanding_Amount'Result <= General_Cent(Cent'Last * 3);

   function Get_Price(P: Pump_Type) return General_Cent;

   function Get_State(P: Pump_Type) return Pump_State;

   procedure Print_State(P: Pump_Type);

   function Get_Cash_Register return Cash_Register_Cent with
     Global => (Input => Internal_State);

   function Test_Cash_Invariant return Boolean with
     Global => (Input => (State, Internal_State));

   function Get_Total_Cash_Invariant return General_Cent with
     Global => (Input => Internal_State);

   function Calculate_Total_Value return General_Cent with
     Global => (Input => (State, Internal_State));

   function All_Pumps_Paid return Boolean;


   --FSM actions
   procedure Lift(P: Pump_Type) with
     Global => (In_Out => State, Proof_In => Internal_State),
     Pre => Get_State(P) = Base or Get_State(P) = Waiting,
     Post => Get_State(P) = Ready and Test_Cash_Invariant;

   procedure Start(P: Pump_Type; Amount_Paid: Cent) with
     Global => (In_Out => State, Proof_In => Internal_State),
     Pre => Get_State(P) = Ready and General_Cent(Amount_Paid) <= General_Cent(Get_Reservoir(P))
     and All_Pumps_Paid,
     Post => Get_State(P) = Pumping and Get_Reservoir(P) >= 0 and Test_Cash_Invariant;

   procedure Stop(P: Pump_Type) with
     Global => (In_Out => State, Proof_In => Internal_State),
     Pre => Get_State(P) = Pumping,
     Post => Get_State(P) = Ready and Test_Cash_Invariant;

   procedure Replace(P: Pump_Type) with
     Global => (In_Out => State, Proof_In => Internal_State),
     Pre => Get_State(P) = Ready,
     Post => Get_State(P) = Waiting and Test_Cash_Invariant;

   procedure Pay(P: Pump_Type; Amount_Paid: Cent) with
     Global => (In_Out => (State, Internal_State)),
     Pre => Get_State(P) = Waiting and Amount_Paid = Get_Outstanding_Amount(P) and Get_Price(P) /= 0,
     Post => Get_State(P) = Base and Get_Outstanding_Amount(P) = 0 and Test_Cash_Invariant;

private

   type Pump is tagged record
      PumpType: Pump_Type;
      Price: General_Cent;
      Reservoir: Reservoir_Cent;
      State: Pump_State;
      Outstanding_Amount: Cent;
   end record;
end;
