      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3384.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI3384.CPv  $
      *
      *   Rev 2.9   May 10 2019 12:51:02   t13ae52
      *AKI01680: G42: Erweiterung Datenspiegel
      *
      *   Rev 2.8   Nov 22 2018 11:02:24   t13ae52
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.7   Nov 22 2018 10:48:14   t13ae52
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.6   Nov 05 2018 16:22:02   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.5   Nov 05 2018 16:07:54   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.4   Nov 05 2018 15:50:32   t13ex78
      *AKI01615: Erm./Aufb./Akt. SPARPLAN
      *
      *   Rev 2.3   Oct 31 2018 17:03:12   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.2   Oct 29 2018 11:55:54   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.1   Oct 25 2018 14:47:24   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.0   Aug 21 2018 16:23:24   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für DEPOTSTAMM            VON Generali Invest *
      *                                                                *
      *    SATZLÄNGE   250                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              ! WHO       *
      *--------!--------!----------------------------------!-----------*
      *AKI01680!15.04.19! NEUES FELD: DAT-WPHG-AKTU        !SCHUMACHER *
      *        !        ! ANPASSUNG FILLER                 !           *
      *----------------------------------------------------------------*
      *        !        !                                  !           *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
         05  'XX'-DTA-SATZ.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(01).
      *            SATZART = 'A'
               15  'XX'-V-ABSENDER      PIC  X(27).
      *            ABSENDER = 'GEI'
               15  'XX'-V-DAT-STAND     PIC  9(08).
      *            Dadtum = 'TTMMJJJJ' Stand
               15  'XX'-V-DATUM         PIC  9(08).
      *            Dadtum = 'TTMMJJJJ' Erstellung
               15  'XX'-V-EMPFAENGER    PIC  X(27).
      *            EMPFAENGER = 'ALAG'
               15  'XX'-V-TEXT          PIC  X(20).
      *            'DEPOTSTAMM'
               15  'XX'-V-ART           PIC  X(01).
      *            K = Komplettbestand, D für Delta
               15  FILLER               PIC  X(158).
      *            RESERVE
      *
           10  'XX'-SATZ REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-SA                PIC X(01).
      *            SATZART        = 'C'
               20  'XX'-GES               PIC 9(02).
      *            GESELLSCHAFT   = '42'
               20  'XX'-VNR               PIC X(15).
               20  FILLER REDEFINES 'XX'-VNR.
                   25 'XX'-VNR-NUM        PIC 9(15).
      *            Vertragsnummer
               20  'XX'-DAT-DEPOT-EROEFF  PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  'XX'-DEPOT-STATUS      PIC 9(01).
      *            1 = aktiv, 2 = geschlossen
               20  'XX'-RISIKOKLASSE      PIC X(01).
      *            RISIKOKLASSE  1 -4
               20  'XX'-ANLAGEZIEL        PIC X(01).
      *            ANLAGEZIEL
               20  'XX'-DEPOT-ART         PIC X(01).
      *            V = VL-Depot
               20  'XX'-IBAN              PIC X(34).
      *            IBAN des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
 
               20  'XX'-BIC               PIC X(11).
      *            BIC des Belastungskontos zur Lastschrift für
      *            Depotpreiseinzug
 
               20  'XX'-MANDREF           PIC X(35).
      *            Mandatsreferenz
 
               20  'XX'-NACHNAME-BZ       PIC X(30).
      *            Kontoinhaber des Belastungskontos zur Lastschrift
      *            Depotpreiseinzug
 
               20  'XX'-N-REAL-GW-VL-EUR-INKL PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-VH-INKL  PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-EUR-OHNE PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20  'XX'-N-REAL-GW-VL-VH-OHNE  PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
               20 'XX'-REAL-GW-EURO-EROEFF    PIC S9(09)V99.
      *           Realisierte Gewinne/Verluste in Euro seit Eröffnung
      *           pro Unterdepot
               20 'XX'-ONLINEBANKING          PIC X(01).
      *           T - Online Transaktion
      *           S - Online Sicht
               20 'XX'-DAT-WPHG-AKTU          PIC 9(08).
      *           DATUM LETZTE AKTUALISIERUNG WPHG: JJJJMMTT
      *
               20  FILLER                     PIC X(048).
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             '9'
               20  'XX'-N-ANZ           PIC 9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER               PIC X(245).
      *            RESERVE
 
 
 
 
 
 
 


