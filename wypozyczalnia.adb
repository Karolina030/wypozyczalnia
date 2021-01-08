with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random, Ada.Numerics.Discrete_Random;

procedure Wypozyczalnia is

   type YearValue is range 2_000 .. 2_020;
   type MaxSpeedValue is range 100 .. 200;
   type FuelUsageValue is range 4 .. 8;
   type BrandName is ('A', 'B', 'C', 'D', 'E');
   type PriceValue is range 50 .. 200;

   package RandomYear is new Ada.Numerics.Discrete_Random (YearValue);
   package RandomSpeed is new Ada.Numerics.Discrete_Random (MaxSpeedValue);
   package RandomFuelUsage is new Ada.Numerics.Discrete_Random
     (FuelUsageValue);
   package RandomBrand is new Ada.Numerics.Discrete_Random (BrandName);
   package RandomPrice is new Ada.Numerics.Discrete_Random (PriceValue);

   type Car is record
      Id        : Integer;
      Brand     : BrandName;
      Year      : YearValue;
      MaxSpeed  : MaxSpeedValue;
      FuelUsage : FuelUsageValue;
      Price     : PriceValue;
   end record;

   type Cars is array (Integer range <>) of Car;
   type BoolVector is array (Integer range <>) of Boolean;

   procedure PrintCar (RentalsCar : in Car) is
   begin
      Put_Line
        ("Id:" & RentalsCar.Id'Img & " Brand: " & RentalsCar.Brand'Img &
         " Year: " & RentalsCar.Year'Img & " Max speed: " &
         RentalsCar.MaxSpeed'Img & "km/h" & " Fuel usage: " &
         RentalsCar.FuelUsage'Img & "l/100km" & " Price: " &
         RentalsCar.Price'Img & "zł/day");
   end PrintCar;

   function CreateCars (NumberOfCars : Integer) return Cars is
      use RandomYear, RandomSpeed, RandomFuelUsage, RandomBrand, RandomPrice;
      RandomYearGen      : RandomYear.Generator;
      RandomSpeedGen     : RandomSpeed.Generator;
      RandomFuelUsageGen : RandomFuelUsage.Generator;
      RandomBrandGen     : RandomBrand.Generator;
      RandomPriceGen     : RandomPrice.Generator;
      RentalsCars        : Cars (0 .. NumberOfCars);
   begin
      Reset (RandomYearGen);
      Reset (RandomSpeedGen);
      Reset (RandomFuelUsageGen);
      Reset (RandomBrandGen);
      Reset (RandomPriceGen);
      for I in RentalsCars'Range loop
         RentalsCars (I) :=
           (I, Random (RandomBrandGen), Random (RandomYearGen),
            Random (RandomSpeedGen), Random (RandomFuelUsageGen),
            Random (RandomPriceGen));
      end loop;
      return RentalsCars;
   end CreateCars;

   protected type RentalTask (NumberOfCars : Integer) is
      procedure Offer (RentalsOffer : out Cars);
      procedure RentCar
        (CarId : in Integer; RentedCar : out Car; IsCarPicked : out Boolean);
      procedure RetrieveCar (CarId : in Integer);
   private
      RentalsCars  : Cars (0 .. NumberOfCars) := CreateCars (NumberOfCars);
      IsCarAviable : BoolVector (0 .. NumberOfCars) :=
        (0 .. NumberOfCars => True);
   end RentalTask;

   protected body RentalTask is
      procedure Offer (RentalsOffer : out Cars) is
      begin
         RentalsOffer := RentalsCars;
      end Offer;
      procedure RentCar
        (CarId : in Integer; RentedCar : out Car; IsCarPicked : out Boolean)
      is
      begin
         if IsCarAviable (CarId) then
            RentedCar            := RentalsCars (CarId);
            IsCarAviable (CarId) := False;
            IsCarPicked          := True;
         else
            IsCarPicked := False;
         end if;
      end RentCar;
      procedure RetrieveCar (CarId : in Integer) is
      begin
         IsCarAviable (CarId) := True;
      end RetrieveCar;
   end RentalTask;

   type RentalTaskPtr is access RentalTask;

   task type CustomerServiceTask
     (CustomerServiceId : Integer; NumberOfCars : Integer;
      Rental            : RentalTaskPtr)
   is
      entry Offer (ClientsId : in Integer; RentalsOffer : out Cars);
      entry RentCar
        (ClientsId   : in     Integer; CarId : in Integer; RentedCar : out Car;
         IsCarPicked : in out Boolean);
      entry RetrieveCar (ClientsId : in Integer; CarId : in Integer);
      entry StartTask;
      entry EndTask;
   end CustomerServiceTask;

   task body CustomerServiceTask is
      CarsToRent : Cars (0 .. NumberOfCars);
   begin
      accept StartTask;
      Rental.Offer (CarsToRent);
      loop
         select
            accept Offer (ClientsId : in Integer; RentalsOffer : out Cars) do
               delay 1.0;
               New_Line;
               Put_Line
                 ("Witaj w wypozyczalni kliencie " & ClientsId'Img &
                  ", pracownik numer " & CustomerServiceId'Img &
                  " do twoich usług");
               Put_Line ("Oto wszystkie samochody w naszej ofercie:");
               RentalsOffer := CarsToRent;
            end Offer;
         or
            accept RentCar
              (ClientsId : in Integer; CarId : in Integer; RentedCar : out Car;
               IsCarPicked : in out Boolean)
            do
               delay 1.0;
               New_Line;
               Put_Line ("Witaj w wypozyczalni kliencie " & ClientsId'Img);
               Put_Line ("Rozpoczynam proces wynajmu auta...");
               Rental.RentCar (CarId, RentedCar, IsCarPicked);
               Put_Line ("Dziękujemy za wynajęcie auta");
               if IsCarPicked = False then
                  Put_Line
                    ("Podane auto jest wynajęte, proszę wybrać inne auto");
               end if;

            end RentCar;
         or
            accept RetrieveCar (ClientsId : in Integer; CarId : in Integer) do
               delay 1.0;
               New_Line;
               Put_Line ("Witaj w wypozyczalni kliencie " & ClientsId'Img);
               Put_Line ("Rozpoczynam proces zwrotu auta...");
               Rental.RetrieveCar (CarId);
               Put_Line ("Dziękujemy za wynajęcie auta");
            end RetrieveCar;
         or
            accept EndTask;
            exit;
         end select;
      end loop;
   end CustomerServiceTask;

   type CustomerServiceTaskPtr is access CustomerServiceTask;
   type CustomerServiceTaskArray is
     array (Integer range <>) of CustomerServiceTaskPtr;
   type CustomerServiceTaskArrayPtr is access CustomerServiceTaskArray;

   task type ClientTask
     (ClientsId    : Integer; CustomerService : CustomerServiceTaskArrayPtr;
      NumberOfCars : Integer)
   ;

   task body ClientTask is
      type CustomerServiceRange is
        new Integer range 0 .. CustomerService'Length;
      type CarRange is new Integer range 0 .. NumberOfCars;
      package RandomCustomerService is new Ada.Numerics.Discrete_Random
        (CustomerServiceRange);
      package RandomCar is new Ada.Numerics.Discrete_Random (CarRange);
      use Ada.Numerics.Float_Random, RandomCustomerService, RandomCar;
      FloatGen                 : Ada.Numerics.Float_Random.Generator;
      RandomCustomerServiceGen : RandomCustomerService.Generator;
      RandomCarGen             : RandomCar.Generator;
      ClientsCarId             : Integer;
      ClientsCar               : Car;
      RentalsOffer             : Cars (0 .. NumberOfCars);
      CustomerServiceId        : Integer;
      IsCarPicked              : Boolean := False;
   begin
      Reset (FloatGen);
      Reset (RandomCustomerServiceGen);
      Reset (RandomCarGen);
      CustomerServiceId := Integer (Random (RandomCustomerServiceGen));
      delay Duration (Random (FloatGen) * 4.0);
      Put_Line
        ("Klient " & ClientsId'Img &
         " chce skorzystać z usług wypozyczalni");
      CustomerService (CustomerServiceId).Offer (ClientsId, RentalsOffer);
      Put_Line
        ("Klient " & ClientsId'Img & " przegląda ofertę wypożyczalni");
      for I in RentalsOffer'Range loop
         PrintCar (RentalsOffer (I));
      end loop;

      while not IsCarPicked loop
         begin
            ClientsCarId := Integer (Random (RandomCarGen));
            Put_Line
              ("Klient " & ClientsId'Img &
               " decyduje się na samochód numer: " & ClientsCarId'Img);
            CustomerService (CustomerServiceId).RentCar
              (ClientsId, ClientsCarId, ClientsCar, IsCarPicked);
         end;
      end loop;
      PrintCar (ClientsCar);
      delay Duration (Random (FloatGen) * 20.0);
      CustomerService (CustomerServiceId).RetrieveCar
        (ClientsId, ClientsCarId);
      New_Line;
      Put_Line ("Klient " & ClientsId'Img & " żegna się");
      New_Line;
   end ClientTask;

   type ClientTaskPtr is access ClientTask;
   type ClientTaskArray is array (Integer range <>) of ClientTaskPtr;
   type ClientTaskArrayPtr is access ClientTaskArray;

   --variables
   CarsCount       : Integer;
   WorkersCount    : Integer;
   ClientsCount    : Integer;
   Rental          : RentalTaskPtr;
   CustomerService : CustomerServiceTaskArrayPtr;
   Clients         : ClientTaskArrayPtr;

begin
   Ada.Text_IO.Put ("Podaj liczbę samochodów w wypożyczalni: ");
   Ada.Integer_Text_IO.Get (CarsCount);
   Ada.Text_IO.Put ("Podaj liczbę pracowników wypożyczalni: ");
   Ada.Integer_Text_IO.Get (WorkersCount);
   Ada.Text_IO.Put ("Podaj liczbę klientów wypożyczalni: ");
   Ada.Integer_Text_IO.Get (ClientsCount);
   CarsCount       := CarsCount - 1;
   WorkersCount    := WorkersCount - 1;
   ClientsCount    := ClientsCount - 1;
   Rental          := new RentalTask (CarsCount);
   CustomerService := new CustomerServiceTaskArray (0 .. WorkersCount);
   Clients         := new ClientTaskArray (0 .. ClientsCount);
   begin
      for I in CustomerService'Range loop
         CustomerService (I) := new CustomerServiceTask (I, CarsCount, Rental);
         CustomerService (I).StartTask;
      end loop;
      for I in Clients'Range loop
         Clients (I) := new ClientTask (I, CustomerService, CarsCount);
      end loop;
   end;
   loop
      if (for all RentalClient of Clients.all => RentalClient'Terminated) then
         exit;
      end if;
   end loop;
   for I in CustomerService'Range loop
      CustomerService (I).EndTask;
   end loop;
end Wypozyczalnia;
