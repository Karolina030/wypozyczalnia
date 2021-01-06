with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Text_IO, Ada.Numerics.Float_Random, Ada.Numerics.Discrete_Random;
use Ada.Text_IO, Ada.Numerics.Float_Random;
with Ada.Exceptions;  use Ada.Exceptions;

procedure Wypozyczalnia is

   type YearValue is range 2000..2020;
   type MaxSpeedValue is range 100..200;
   type FuelUsageValue is range 4..7;
   type BrandName is ('A','B','C','D','E');

   package RandomYear is new Ada.Numerics.Discrete_Random(YearValue);
   package RandomSpeed is new Ada.Numerics.Discrete_Random(MaxSpeedValue);
   package RandomFuelUsage is new Ada.Numerics.Discrete_Random(FuelUsageValue);
   package RandomBrand is new Ada.Numerics.Discrete_Random(BrandName);


   type Car is record
      Id : Integer;
      Brand : BrandName;
      Year : YearValue;
      MaxSpeed : MaxSpeedValue;
      FuelUsage : FuelUsageValue;
   end record;

   type RentalsCars is array(Integer range <>) of Car;

   procedure PrintCar(RentalCar : in Car) is
   begin
      Put_Line("Id:" & RentalCar.Id'Img & " Brand: " & RentalCar.Brand'Img & " Year: " & RentalCar.Year'Img & " Max speed: " & RentalCar.MaxSpeed'Img & "km/h" & " Fuel usage: " & RentalCar.FuelUsage'Img & "l/100km");
   end PrintCar;

   procedure CreateCars(Cars : in out RentalsCars) is
      use RandomYear, RandomSpeed, RandomFuelUsage, RandomBrand;
      RandomYearGen : RandomYear.Generator;
      RandomSpeedGen : RandomSpeed.Generator;
      RandomFuelUsageGen : RandomFuelUsage.Generator;
      RandomBrandGen: RandomBrand.Generator;
   begin
      Reset(RandomYearGen);
      Reset(RandomSpeedGen);
      Reset(RandomFuelUsageGen);
      Reset(RandomBrandGen);
      for I in Cars'Range loop
         Cars(I) := (I,Random(RandomBrandGen),Random(RandomYearGen), Random(RandomSpeedGen),Random(RandomFuelUsageGen));
         PrintCar(Cars(I));
      end loop;
   end CreateCars;

   task type Semaphore is
      entry Wait(ClientsCar : out Car);  --wejście Wait (opuszczenie semafora)
      entry Signal;  --wejście Signal (podniesienie semafora)
      entry Ilosc;
      entry Sprawdz(I: out Integer);
   end Semaphore;


    task body Semaphore is
      Rental : RentalsCars(1..5);
      MaxCount : Natural;   --ilosc dostepnych aut danego typu do wypozyczenia
      Count : Natural;  --początkowa wartość semafora
    begin
      CreateCars(Rental);
      MaxCount:=Rental'Length;
      Count:=Rental'Length;
      loop
          select
            when Count > 0 =>
              accept Wait(ClientsCar : out Car)
               do
                  ClientsCar := Rental(Count);
                  Count := Count - 1;
              end Wait;
          or
            when Count < MaxCount =>
              accept Signal
                do
                  Count := Count + 1;
              end Signal;
          or
            accept Ilosc do
                Put_Line("Ilosc dostepnych aut tego typu: " & Count'Img);
            end Ilosc;
          or
            accept Sprawdz(I: out Integer) do
              I := Count;
            end Sprawdz;
          or
            terminate;
          end select;
          begin
            Put("");
          exception
            when Program_Error     => Put_Line("Program_Error");
            when Constraint_Error  => Put_Line("Constraint_Error");
          end;
        end loop;
   end Semaphore;


S_Tanszy : Semaphore;
S_Drozszy : Semaphore;

    task Pracownik is
        entry Info(N: Integer);
        entry Start;
        entry Koniec;

    end Pracownik;

    task body Pracownik is
  --  N: Integer;
    begin

    accept Start;

    loop
        select   -- sprawdzanie  czy są komunikaty  od zadań
          accept Info(N: Integer) do
            delay 1.0;
            New_Line;
            Put_Line("Witaj w wypozyczalni kliencie "& N'Img);
            Put_Line("Info o wypozyczalni:");
            Put_Line("TANSZE AUTA:");
            S_Tanszy.Ilosc;  --klient czeka na dostęp do auta
            Put_Line("DROZSZE AUTA:");
            S_Drozszy.Ilosc;
          end Info;
        or
          accept Koniec;
          exit;
        end select;
        begin
          Put("");
        exception
          when Program_Error     => Put_Line("Program_Error");
          when Constraint_Error  => Put_Line("Constraint_Error");
        end;
    end loop;
    Put_Line("Koniec");
    end Pracownik;


   task type Klient(N: Integer); -- tworzony jest typ zadaniowy, reprezentujacy klientow

   task body Klient is
    Gen : Ada.Numerics.Float_Random.Generator;
    Licznik : Natural;  --początkowa wartość semafora
    ClientsCar : Car;

   begin
    loop
      --  accept Start;

        reset(Gen);
        delay duration(random(Gen) * 4.0); -- klienci przychodzą w losowym czasie

        Pracownik.Info(N);
        if (N mod 2 =0) then  -- klienci o parzystych numerach wypozyczaja auta tansze
            Put_Line("Klient: " & N'Img & " chce wypozyczyc tansze auto");
            S_Tanszy.Ilosc;  --klient czeka na dostęp do auta
            S_Tanszy.Sprawdz(Licznik);
            if Licznik=0 then  --brak dostępnych aut w wybranej półce cenowej
              Put_Line("UWAGA: Klient decyduje sie wypozyczyc drozsze auto");
              S_Drozszy.Wait(ClientsCar);  --klient czeka na dostęp do auta
              Put_Line("Auto wypozycza klient numer: " & N'Img); -- sekcja krytyczna
              Put_Line("Wypożyczone auto:");
              PrintCar(ClientsCar);
              delay duration(random(Gen) * 20.0);   --  klient korzysta z samochodu
              S_Drozszy.Signal;  -- klient zwraca auto
              New_Line;
              Put_Line("Auto oddaje klient numer: " & N'Img);
              Put_Line("Do widzenia");
              New_Line;

            else
              S_Tanszy.Wait(ClientsCar);  --klient czeka na dostęp do auta
              Put_Line("Auto wypozycza klient numer: " & N'Img); -- sekcja krytyczna
              Put_Line("Wypożyczone auto:");
              PrintCar(ClientsCar);
              delay duration(random(Gen) * 20.0);   --  klient korzysta z samochodu
              S_Tanszy.Signal;  -- klient zwraca auto
              New_Line;
              Put_Line("Auto oddaje klient numer: " & N'Img);
              Put_Line("Do widzenia");
              New_Line;

            end if;
        else -- klienci o nieparzystych numerach wypozyczaja auta drozsze
            Put_Line("Klient: " & N'Img & " chce wypozyczyc drozsze auto");
            S_Drozszy.Ilosc;  --klient czeka na dostęp do auta
            S_Drozszy.Sprawdz(Licznik);
            if Licznik=0 then  --brak dostępnych aut w wybranej półce cenowej
              Put_Line("UWAGA: Klient decyduje sie wypozyczyc tansze auto");
              S_Tanszy.Wait(ClientsCar);  --klient czeka na dostęp do auta
              Put_Line("Auto wypozycza klient numer: " & N'Img); -- sekcja krytyczna
              Put_Line("Wypożyczone auto:");
              PrintCar(ClientsCar);
              delay duration(random(Gen) * 20.0);   --  klient korzysta z samochodu
              S_Tanszy.Signal;  -- klient zwraca auto
              New_Line;
              Put_Line("Auto oddaje klient numer: " & N'Img);
              Put_Line("Do widzenia");
              New_Line;

            else
              S_Drozszy.Wait(ClientsCar);  --klient czeka na dostęp do auta
              Put_Line("Auto wypozycza klient numer: " & N'Img); -- sekcja krytyczna
              Put_Line("Wypożyczone auto:");
              PrintCar(ClientsCar);
              delay duration(random(Gen) * 20.0);   --  klient korzysta z samochodu
              S_Drozszy.Signal;  -- klient zwraca auto
              New_Line;
              Put_Line("Auto oddaje klient numer: " & N'Img);
              Put_Line("Do widzenia");
              New_Line;

            end if;
        end if;
        Pracownik.Koniec;
        begin
          Put("");
        exception
          when Program_Error     => Put_Line("Program_Error");
          when Constraint_Error  => Put_Line("Constraint_Error");
        end;
    end loop;
   end Klient;


--deklaracja 20 klientow o różnych numerach
K1 : Klient(1);
K2 : Klient(2);
K3 : Klient(3);
K4 : Klient(4);
K5 : Klient(5);
K6 : Klient(6);
K7 : Klient(7);
K8 : Klient(8);
K9 : Klient(9);
K10 : Klient(10);
K11 : Klient(12);
K12 : Klient(14);
K13 : Klient(16);
K14 : Klient(18);
K15 : Klient(20);
K16 : Klient(22);
K17 : Klient(24);
K18 : Klient(26);
K19 : Klient(28);
K20 : Klient(30);


begin
    Pracownik.Start;
end Wypozyczalnia;
