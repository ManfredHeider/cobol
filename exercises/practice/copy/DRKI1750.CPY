      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1750.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI1750.CPv  $
      *
      *   Rev 2.9   Jan 14 2021 15:16:18   t13ae31
      *AKI01823: G50 Vertragsanzeige Tarife mehrfach Pgme
      *
      *   Rev 2.8   Sep 04 2020 11:55:00   t13ac20
      *AKI01799: Central: Aufnahme des EBE Beitrags in die Bestands
      *daten - Source
      *
      *   Rev 2.7   Mar 06 2018 10:14:10   t13ac13
      *AKI01568: Central: Bestandserweiterung um ein Kennz. für Anw
      *artschaft - Copys
      *
      *   Rev 2.6   Sep 12 2016 11:04:12   t13ex78
      *AKI01463: Programme, Copies ect.
      *
      *   Rev 2.5   Feb 23 2016 07:19:48   t13ae31
      *AKI01405: COC G50 CKV
      *
      *   Rev 2.4   Jun 13 2013 14:51:34   t13ae31
      *AKI01204: SEPA - DB-Online-CKV
      *
      *   Rev 2.3   Sep 20 2012 11:05:52   t13ae31
      *AKI01166: CKV Daten zu mitversicherten Personen
      *
      *   Rev 2.2   Jun 26 2012 17:04:34   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.1   Jul 27 2005 13:02:28   t13ae31
      *AKI00469: Anpassungen Central-DTA
      *
      *   Rev 2.0   Feb 28 2005 10:42:44   t13ae31
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV                          *
      *    -----------------------------------                         *
      *                                                                *
      *                                                                *
      *    Länge = 400 BYTES                                           *
      *    DAVON KEY = 029 BYTES                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                           !NAME         *
      *AUFTRAG !                                         !             *
      *--------!-----------------------------------------!-------------*
      *28.02.05!NEUERSTELLUNG wegen neuer Schnittstelle  ! P.Huebner   *
      *AKI00469!                                         !             *
      *--------!-----------------------------------------!-------------*
      *09.05.12!Nachname fuer die Satzart 2 (Person) und ! H.Schlenther*
      *AKI01100!Geburtsdatum fuer die Satzart 1(Vertrag) !             *
      *        !integriert                               !             *
      * ---------------------------------------------------------------*
100912*AKI01166! Adressdaten VP + Bankdaten              ! P. Hübner   *
      *        !   sowie Anpassungen NKDA                !             *
      *--------!-----------------------------------------!-------------*
150613*15.06.13! SEPA                                    ! P. Hübner   *
      *--------!-----------------------------------------!-------------*
111215*11.12.15! COC-KZ mit SL Änderung 344 auf 400      ! P. Hübner   *
      *--------!-----------------------------------------!-------------*
      *--------!-----------------------------------------!-------------*
250816*AKI01463! Fremdversicherer und Korrepondenzmakler ! tfl         *
      *--------!-----------------------------------------!-------------*
160318*AKI01568! Kennzeichen STVTBTG (Anwartschaft)      ! Boost       *
      *        ! neu                                     !             *
      *--------!-----------------------------------------!-------------*
      *AKI01799! neues Feld  BE-REDUK-BTG                ! T13AC20     *
      *--------!-----------------------------------------!-------------*
      *AKI01823! neues Feld  ANZ-TAR-DUP                 ! HHU         *
CCC009*    SKIP1
       01  CKVN-K-SATZ1.
           10  CKVN-K-KEY-FELDER.
              15  CKVN-K-SA1             PIC X.
              15  CKVN-K-SA              PIC XXX.
              15  CKVN-K-VNR             PIC 9(9).
              15  CKVN-K-PNR             PIC 9(04).
              15  CKVN-K-LFDNR           PIC 9(04).
              15  CKVN-K-TARNAM          PIC X(8).
           10  CKVN1.
              20  CKVN-K-FU              PIC X.
              20  CKVN-K-GS              PIC 999.
              20  CKVN-K-TVT             PIC 9(9).
              20  CKVN-K-SONDBETR        PIC X(11).
              20  CKVN-K-PARTINF         PIC X(15).
              20  CKVN-K-VBEG            PIC X(10).
              20  CKVN-K-VABL            PIC X(10).
              20  CKVN-K-VAEND           PIC X(10).
              20  CKVN-K-ANREDE          PIC XX.
              20  CKVN-K-NNAME           PIC X(40).
              20  CKVN-K-TITEL           PIC X(25).
              20  CKVN-K-ADTITEL         PIC X(20).
              20  CKVN-K-VNAME           PIC X(25).
              20  CKVN-K-AUSL            PIC XXX.
              20  CKVN-K-PLZ             PIC X(6).
              20  CKVN-K-ORT             PIC X(34).
              20  CKVN-K-ORTZUKZ         PIC X.
              20  CKVN-K-STRASSE         PIC X(40).
              20  CKVN-K-BERUF           PIC XXXX.
              20  CKVN-K-BERUF2          PIC X(9).
              20  CKVN-K-LAST            PIC X.
              20  CKVN-K-ZAHLWEISE       PIC X.
              20  CKVN-K-PNRVN           PIC 9999.
              20  CKVN-K-WHR             PIC X(03).
              20  CKVN-K-KDNR            PIC X(10).
              20  CKVN-K-ABVERM          PIC 9(09).
090512        20  CVKN-K-GEBURTSDATUM    PIC X(08).
090512*       20  CKVN-K-RESERVE1        PIC X(08).
151215        20  CKVN-K-COC-DATUM       PIC X(10).
151215        20  CKVN-K-COC-KZ          PIC X(01).
151215        20  CKVN-K-RESERVE1        PIC X(45).
              20  CKVN-K-AKTUAL-KZ       PIC X.
       01  CKVN-K-SATZ2.
           10  FILLER                   PIC X(42).
           10  CKVN2.
               20  CKVN-K-VORNAME        PIC X(25).
               20  CKVN-K-GEBDAT         PIC X(10).
               20  CKVN-K-GESCHL         PIC X.
               20  CKVN-K-BERUF3         PIC X(4).
               20  CKVN-K-BERUF4         PIC X(9).
               20  CKVN-K-PERSBEG        PIC X(10).
               20  CKVN-K-PERSABL        PIC X(10).
               20  CKVN-K-PERSAEND       PIC X(10).
               20  CKVN-K-DSS            PIC X(4).
090512         20  CKVN-K-NACHNAME       PIC X(40).
100912         20  CKVN-K-ANREDEVP       PIC X(01).
               20  CKVN-K-TITEL2VP       PIC X(25).
               20  CKVN-K-STRASSEVP      PIC X(25).
               20  CKVN-K-AUSLVP         PIC X(03).
               20  CKVN-K-PLZVP          PIC X(06).
               20  CKVN-K-ORTVP          PIC X(25).
250816         20  CKVN-K-VORVERSSCHL    PIC 9(05).
250816         20  CKVN-K-VORVERSDATUM   PIC X(10).
250816         20  CKVN-K-VORVERSKUENDKZ PIC X(01).
151215*        20  CKVN-K-RESERVEVP      PIC X(150).
250816         20  CKVN-K-RESERVEVP      PIC X(134).
       01  CKVN-K-SATZ3.
           10  FILLER                   PIC X(42).
           10  CKVN3.
               20  CKVN-K-ZUVST          PIC X(10).
               20  CKVN-K-ZUERSTELL      PIC X(10).
               20  CKVN-K-ABVST          PIC X(10).
               20  CKVN-K-TARNAM1        PIC X(8).
               20  CKVN-K-TARFORM        PIC X(10).
               20  CKVN-K-VLEIST         PIC S9(9)V99.
               20  CKVN-K-SBTEIL         PIC S9(9)V99.
               20  CKVN-K-BEITRAG        PIC S9(9)V99.
160318         20  CKVN-K-STVTBTG        PIC X(02).
               20  CKVN-K-BE-REDUK-BTG   PIC S9(9)V99.
               20  CKVN-K-RESERVE3       PIC X(262).
               20  CKVN-K-ANZ-TAR-DUP    PIC 99.
100912 01 CKVN-K-SATZ4.
           10  FILLER                   PIC X(42).
           10  CVN4.
              20 CKVN-K-LFD-BANK-NR          PIC 9(05).
      ** laufende Banknummer
              20 CKVN-K-BANKTYP              PIC X(01).
      ** Bank-TYP
              20 CKVN-K-BANKNAME             PIC X(30).
      ** Bankname
     ********  20 CKVN-K-KONTOINHABER         PIC X(40).
     **** Kontoinhaber
               20 CKVN-K-NAME-BZ              PIC X(40).
     **** NAME abweichender Beitragszahler
               20 CKVN-K-IBAN                 PIC X(34).
      ** IBAN / Kontonummer
      ** Die Kontonummer wird in seinem zukuenftigen Nachfolgerfeld
      ** IBAN geliefert. Mail von Herrn Chalupka(CENTRAL) vom
      ** 03.05.2012 um 14.11 Uhr.
               20 CKVN-K-BIC                  PIC X(11).
      ** Bankinterne Clearing Nummer
               20 CKVN-K-BLZ                  PIC X(08).
      ** BankLeitZahl -entfaellt in absehbarer Zeit-
150613******** neu für sepa
               20 CKVN-K-VORNAME-BZ               PIC X(25).
      *** Vorname abweichender Beitragszahler
               20 CKVN-K-STRASSE-BZ               PIC X(25).
      *** Strasse abweichender Beitragszahler
               20 CKVN-K-LAND-BZ                  PIC X(03).
      *** Landkz abweichender Beitragszahler
               20 CKVN-K-PLZ-BZ                   PIC X(06).
      *** PLZ abweichender Beitragszahler
               20 CKVN-K-ORT-BZ                   PIC X(25).
      *** Ort abweichender Beitragszahler
               20 CKVN-K-GEBDAT-BZ                PIC X(10).
      *** Geburtsdatum abweichender Beitragszahler
               20 CKVN-K-ANREDE-BZ                PIC X(02).
      *** Anrede abweichender Beitragszahler
     ****        20  CKVN-K-RESERVE4       PIC X(163).
150613*****    20  CKVN-K-RESERVE4       PIC X(67).
151215         20  CKVN-K-RESERVE4       PIC X(123).
       01  CKVN-K-SATZ.
           10 CKVN-K-KEY1                PIC X(029).
     ****  10 CKVN-K-REST                PIC X(315).
151215     10 CKVN-K-REST                PIC X(371).
      *  ----------------- COPY ENDE -------------
 
 



