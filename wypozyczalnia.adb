with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO, Ada.Numerics.Float_Random, Ada.Numerics.Discrete_Random;
use Ada.Text_IO, Ada.Numerics.Float_Random;
with Ada.Exceptions; use Ada.Exceptions;

procedure Wypozyczalnia is

   type YearValue is range 2_000 .. 2_020;
   type MaxSpeedValue is range 100 .. 200;
   type FuelUsageValue is range 4 .. 8;
   type BrandName is ('A', 'B', 'C', 'D', 'E');
   type PriceValue is range 50 .. 200;

   package RandomYear is new Ada.Numerics.Discrete_Random (YearValue);
   package RandomSpeed is new Ada.Numerics.Discrete_Random (MaxSpeedValue);
   package RandomFuelUsage is new Ada.Numerics.Discrete_Random(FuelUsageValue);
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

   protected type Rental (NumberOfCars : Integer) is
      entry Offer (RentalsOffer : out Cars);
      entry RentCar (CarId : in Integer;RentedCar : out Car);
      entry RetrieveCar (CarId : in Integer);
   private
      RentalsCars : Cars (0 .. NumberOfCars) := CreateCars (NumberOfCars);
      IsCarAviable : BoolVector ( 0 .. NumberOfCars)  := (0 .. NumberOfCars => True);
   end Rental;

   CarNotAvaiableException : exception;

   protected body Rental is
      entry Offer (RentalsOffer : out Cars) when True is
      begin
         RentalsOffer:= RentalsCars;
      end Offer;
      entry RentCar (CarId : in Integer; RentedCar : out Car) when True is
      begin
         if IsCarAviable(CarId) then
         RentedCar := RentalsCars(CarId);
            IsCarAviable(CarId) := False;
         else
            raise CarNotAvaiableException with "Car not avaiable";
            end if;
      end RentCar;
      entry RetrieveCar (CarId : in Integer) when True is
      begin
         IsCarAviable(CarId) := True;
      end RetrieveCar;
   end Rental;

   FirstRental : Rental(5);

   task type CustomerService(CustomerServiceId : Integer; NumberOfCars: Integer) is
      entry Offer(ClientsId: in Integer;RentalsOffer : out Cars);
      entry RentCar(ClientsId: in Integer;CarId : in Integer;RentedCar : out Car);
      entry RetrieveCar(ClientsId: in Integer;CarId : in Integer);
      entry StartTask;
      entry EndTask;
    end CustomerService;

   task body CustomerService is
            CarsToRent : Cars (0 .. NumberOfCars);
    begin
      accept StartTask;
      FirstRental.Offer(CarsToRent);
      loop
        select   -- sprawdzanie  czy są komunikaty  od zadań
          accept Offer(ClientsId: in  Integer;RentalsOffer : out  Cars) do
            delay 1.0;
            New_Line;
            Put_Line("Witaj w wypozyczalni kliencie "& ClientsId'Img);
            Put_Line("Oto wszystkie samochody w naszej ofercie:");
            RentalsOffer:=CarsToRent;
            end Offer;
         or accept  RentCar(ClientsId: in Integer;CarId : in Integer;RentedCar : out Car) do
            delay 1.0;
            New_Line;
            Put_Line("Witaj w wypozyczalni kliencie "& ClientsId'Img);
            Put_Line("Rozpoczynam proces wynajmu auta...");
               FirstRental.RentCar(CarId,RentedCar);
               Put_Line("Dziękujemy za wynajęcie auta");
            end RentCar;
         or accept RetrieveCar(ClientsId: in Integer;CarId : in Integer) do
                           delay 1.0;
            New_Line;
            Put_Line("Witaj w wypozyczalni kliencie "& ClientsId'Img);
            Put_Line("Rozpoczynam proces zwrotu auta...");
            FirstRental.RetrieveCar(CarId);
            Put_Line("Dziękujemy za wynajęcie auta");
            end RetrieveCar;
        or
          accept EndTask;
          exit;
        end select;
        begin
          Put("");
        exception
          when Program_Error     => Put_Line("Program_Error");
            when Constraint_Error  => Put_Line("Constraint_Error");
          when CarNotAvaiableException => raise;
        end;
      end loop;
      Put_Line("Koniec symulacji");
      end CustomerService;
   CustomerService1 : CustomerService(1,5);

   task type Client(ClientsId: Integer); -- tworzony jest typ zadaniowy, reprezentujacy klientow

   task body Client is
      Gen : Ada.Numerics.Float_Random.Generator;
      ClientsCar : Car;
      RentalsOffer : Cars (0..5);
   begin
      reset(Gen);
      delay duration(random(Gen) * 4.0); -- klienci przychodzą w losowym czasie
      CustomerService1.Offer(ClientsId,RentalsOffer);
      for I in RentalsOffer'Range loop
         PrintCar(RentalsOffer(I));
      end loop;
      Put_Line("Klient: " & ClientsId'Img & " chce skorzystać z usług wypozyczalni");
      CustomerService1.RentCar(ClientsId,1,ClientsCar);
      PrintCar(ClientsCar);
      delay duration(random(Gen) * 20.0);   --  Client korzysta z samochodu
      CustomerService1.RetrieveCar(ClientsId,1);
      New_Line;
      Put_Line("Do widzenia");
      New_Line;
      CustomerService1.EndTask;
      begin
      Put("");
      exception
         when Program_Error     => Put_Line("Program_Error");
         when Constraint_Error  => Put_Line("Constraint_Error");
         when CarNotAvaiableException => raise;
        end;
   end Client;

   Client1 : Client(1);
   Client2 : Client(2);


begin
   CustomerService1.StartTask;
end Wypozyczalnia;
