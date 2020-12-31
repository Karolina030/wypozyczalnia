
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Text_IO, Ada.Numerics.Float_Random, Ada.Numerics.Discrete_Random;
use Ada.Text_IO, Ada.Numerics.Float_Random;

procedure Semafor is

   task type Semaphore is
      entry Wait;  --wejście Wait (opuszczenie semafora)
      entry Signal;  --wejście Signal (podniesienie semafora)
      entry Ilosc;
   end Semaphore;


    task body Semaphore is

      Count : Natural := 5;  --początkowa wartość semafora
      MaxCount : Natural := 5;   --ilosc dostepnych aut do wypozyczenia
      begin
        loop
          select
            when Count > 0 =>
              accept Wait
                do 
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
            terminate;
          end select;
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
            Put_Line("witaj w wypozyczalni kliencie "& N'Img);
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
    end loop;
    Put_Line("Koniec");
    end Pracownik;


   task type Klient(N: Integer); -- tworzony jest typ zadaniowy, reprezentujacy klientow
   
   task body Klient is
    Gen : Ada.Numerics.Float_Random.Generator;
    Wart : Float;
   begin
    loop 
      --  accept Start;	

        reset(Gen);
        delay duration(random(Gen) * 4.0); -- klienci przychodzą w losowym czasie

        Pracownik.Info(N);
       -- Put_Line("witaj w wypozyczalni kliencie "& N'Img);
        if (N mod 2 =0) then
            Put_Line("Klient: " & N'Img & " chce wypozyczyc tansze auto");
            S_Tanszy.Ilosc;  --klient czeka na dostęp do auta
            S_Tanszy.Wait;  --klient czeka na dostęp do auta
            Put_Line("Auto wypozycza klient numer: " & N'Img); -- sekcja krytyczna
            delay duration(random(Gen) * 20.0);   --  klient korzysta z samochodu
            S_Tanszy.Signal;  -- klient zwraca auto
            Put_Line("Auto oddaje klient numer: " & N'Img);
            Put_Line("Do widzenia");
        else
            Put_Line("Klient: " & N'Img & " chce wypozyczyc drozsze auto");
            S_Drozszy.Ilosc;  --klient czeka na dostęp do auta
            S_Drozszy.Wait;  --klient czeka na dostęp do auta
            Put_Line("Auto wypozycza klient numer: " & N'Img); -- sekcja krytyczna
            delay duration(random(Gen) * 20.0);   --  klient korzysta z samochodu
            S_Drozszy.Signal;  -- klient zwraca auto
            Put_Line("Auto oddaje klient numer: " & N'Img);
            Put_Line("Do widzenia");
        end if;
        Pracownik.Koniec;

    end loop; 
   end Klient;


--deklaracja 10 klientow o różnych numerach
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

begin
    Pracownik.Start;
end Semafor;