      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1232.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1232.CPv  $
      *
      *   Rev 2.1   04 Jul 2001 14:49:48   t13ex39
      *AKI00214 Auftragserfassung
      *
      *   Rev 2.0   22 Mar 2001 15:50:46   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00214! 04.7.01!1.0 ! NEU                       ! ABO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FÜR UPRO UPKI0139                                         *
      *    SchnittStelle fuer die Datenfernuebertragung              *
      *                                                              *
      * Erstellt am: 23.10.2000                                      *
      * Groesse    : 1479 Byte                                       *
      *                                                              *
      *                                                              *
      ****************************************************************
       01  PI-DRKI1232.

      *--2.1 PersonenDaten
         02 Personen-Daten.

      *---- 2.1.1 GleichHeitsPr³fung
            03 PZM-EVNVT1         PIC X          VALUE SPACE.
            03 PZM-EVNVT2         PIC X          VALUE SPACE.
      *                                                     2
      *---- 2.1.2 VersicherungsNehmer / Antragsteller
            03 PZM-PDVNAnred      PIC XX         VALUE SPACES.
            03 PZM-PDVNTitel      PIC X(14)      VALUE SPACES.
            03 PZM-PDVNVorNa      PIC X(16)      VALUE SPACES.
            03 PZM-PDVNZuNa       PIC X(33)      VALUE SPACES.
            03 PZM-PDVNZus        PIC X(15)      VALUE SPACES.

            03 PZM-PDVNGbNa       PIC X(21)      VALUE SPACES.
            03 PZM-PDVNStr        PIC X(25)      VALUE SPACES.
            03 PZM-PDVNLkz        PIC X(3)       VALUE SPACES.
            03 PZM-PDVNLand       PIC X(35)      VALUE SPACES.
            03 PZM-PDVNPlz        PIC X(6)       VALUE SPACES.
            03 PZM-PDVNOrt        PIC X(30)      VALUE SPACES.
            03 PZM-PDVNTelPr      PIC X(23)      VALUE SPACES.
            03 PZM-PDVNFax        PIC X(23)      VALUE SPACES.
            03 PZM-PDVNTelDi      PIC X(23)      VALUE SPACES.
            03 PZM-PDVNGbDat      PIC X(10)      VALUE SPACES.
            03 PZM-PDVNGbOrt      PIC X(30)      VALUE SPACES.
            03 PZM-PDVNFAM        PIC X(1)       VALUE SPACES.
            03 PZM-PDVNSELBST     PIC X          VALUE SPACES.
            03 PZM-PDVNBer        PIC X(40)      VALUE SPACES.
            03 PZM-PDVNBerKy      PIC X(6)       VALUE SPACES.
            03 PZM-PDVNStaat      PIC X(35)      VALUE SPACES.
            03 PZM-PDVNStaatKZ    PIC X(3)       VALUE SPACES.

      *                                                   391
      *---- 2.1.3 Erste Versicherte Person
            03 PZM-PDV1Anred      PIC XX         VALUE SPACES.
            03 PZM-PDV1Titel      PIC X(14)      VALUE SPACES.
            03 PZM-PDV1VorNa      PIC X(16)      VALUE SPACES.
            03 PZM-PDV1ZuNa       PIC X(21)      VALUE SPACES.
            03 PZM-PDV1Zus        PIC X(15)      VALUE SPACES.

            03 PZM-PDV1GbNa       PIC X(21)      VALUE SPACES.
            03 PZM-PDV1Str        PIC X(25)      VALUE SPACES.
            03 PZM-PDV1Lkz        PIC X(3)       VALUE SPACES.
            03 PZM-PDV1Land       PIC X(35)      VALUE SPACES.
            03 PZM-PDV1Plz        PIC X(6)       VALUE SPACES.
            03 PZM-PDV1Ort        PIC X(30)      VALUE SPACES.
            03 PZM-PDV1GbDat      PIC X(10)      VALUE SPACES.
            03 PZM-PDV1GbOrt      PIC X(30)      VALUE SPACES.
            03 PZM-PDV1FAM        PIC X          VALUE SPACES.
            03 PZM-PDV1SELBST     PIC X          VALUE SPACES.
            03 PZM-PDV1Ber        PIC X(40)      VALUE SPACES.
            03 PZM-PDV1BerKy      PIC X(6)       VALUE SPACES.
            03 PZM-PDV1Staat      PIC X(35)      VALUE SPACES.
            03 PZM-PDV1StaatKZ    PIC X(3)       VALUE SPACES.
      *                                                   310
      *---- 2.1.4 Zweite versicherte Person
            03 PZM-PDV2Anred      PIC XX         VALUE SPACES.
            03 PZM-PDV2Titel      PIC X(14)      VALUE SPACES.
            03 PZM-PDV2VorNa      PIC X(16)      VALUE SPACES.
            03 PZM-PDV2ZuNa       PIC X(21)      VALUE SPACES.
            03 PZM-PDV2Zus        PIC X(15)      VALUE SPACES.

            03 PZM-PDV2GbNa       PIC X(21)      VALUE SPACES.
            03 PZM-PDV2Str        PIC X(25)      VALUE SPACES.
            03 PZM-PDV2Lkz        PIC X(3)       VALUE SPACES.
            03 PZM-PDV2Land       PIC X(35)      VALUE SPACES.
            03 PZM-PDV2Plz        PIC X(6)       VALUE SPACES.
            03 PZM-PDV2Ort        PIC X(30)      VALUE SPACES.
            03 PZM-PDV2GbDat      PIC X(10)      VALUE SPACES.
            03 PZM-PDV2GbOrt      PIC X(30)      VALUE SPACES.
            03 PZM-PDV2FAM        PIC X          VALUE SPACES.
            03 PZM-PDV2SELBST     PIC X          VALUE SPACES.
            03 PZM-PDV2Ber        PIC X(40)      VALUE SPACES.
            03 PZM-PDV2BerKy      PIC X(6)       VALUE SPACES.
            03 PZM-PDV2Staat      PIC X(35)      VALUE SPACES.
            03 PZM-PDV2StaatKZ    PIC X(3)       VALUE SPACES.
      *                                                   310
      *--------------------------------------------------------------*

      *--2.2 VertriebsDaten / interne Daten
         02 Vertriebs-Interne-Daten.
            03 PZM-VDSparte       PIC X          VALUE SPACES.
            03 PZM-VDGKZ          PIC XX         VALUE SPACES.
           03 PZM-VDAbVerm       PIC 9(7)       VALUE ZEROES.
           03 PZM-VDAbVermPZ     PIC X(1)       VALUE SPACES.
           03 FILLER             PIC X(1)       VALUE SPACES.
            03 PZM-VDVMVORNA      PIC X(16)      VALUE SPACES.
            03 PZM-VDVMZUNA       PIC X(21)      VALUE SPACES.
           03 PZM-VDVMNR         PIC 9(7)       VALUE ZEROES.
           03 PZM-VDVMNRPZ       PIC X(1)       VALUE SPACES.
           03 PZM-VDVMNRB        PIC X(1)       VALUE SPACES.

            03 PZM-VDPRODKEY      PIC 9(4)       VALUE ZEROES.
            03 PZM-VDABLEIN       PIC 9(6)V9     VALUE ZEROES.
            03 PZM-VDBetreu       PIC X(7)       VALUE SPACES.
            03 PZM-VDDirekt       PIC 9(3)       VALUE ZEROES.
            03 PZM-VDPif          PIC X(15)      VALUE SPACES.
            03 PZM-VDProgKz       PIC X(3)       VALUE SPACES.
      *                                                     97
      *--------------------------------------------------------------*

      *--2.3 VersicherungsTechnische Daten
         02 Vers-Tech-Daten.

      *---- 2.3.1 VersionsDaten
            03 PZM-Version        PIC X(4)       VALUE SPACES.
      *                                                     4
      *---- 2.3.2 Daten zur Hauptversicherung
            03 PZM-HVANTNUM       PIC X(9)       VALUE SPACES.
            03 PZM-HVTARIF        PIC X(15)      VALUE SPACES.
            03 PZM-HVVSNR         PIC 9(8)       VALUE ZEROES.
            03 PZM-HVVBEGINN      PIC X(10)      VALUE SPACES.
            03 PZM-HVN            PIC 999        VALUE ZEROES.
            03 PZM-HVNS           PIC 999        VALUE ZEROES.
            03 PZM-HV8ENDALT      PIC 999        VALUE ZEROES.
            03 PZM-HVBVNL         PIC 99         VALUE ZEROES.
            03 PZM-EZAHLW         PIC 99         VALUE ZEROES.
            03 PZM-HVBEITRAG      PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HVRIS          PIC 9(3)V99    VALUE ZEROES.
            03 PZM-ZUEB           PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HVVSUMME       PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HVTAZGAR       PIC 9(9)       VALUE ZEROES.
      *                                                    99
            03 PZM-TAJADF-1       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-2       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-3       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-4       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-5       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-6       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-7       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-8       PIC 999        VALUE ZEROES.
            03 PZM-TAJADF-9       PIC 999        VALUE ZEROES.
      *                                                    27
      *      03 PZM-FONDS-TAB  OCCURS 3.
            03 PZM-DM1FONDS        PIC XX      VALUE SPACES.
            03 PZM-DM1FONDSPROZ    PIC 999     VALUE ZEROES.
            03 PZM-DM2FONDS        PIC XX      VALUE SPACES.
            03 PZM-DM2FONDSPROZ    PIC 999     VALUE ZEROES.
            03 PZM-DM3FONDS        PIC XX      VALUE SPACES.
            03 PZM-DM3FONDSPROZ    PIC 999     VALUE ZEROES.

      *                                                    15
            03 PZM-HVBEGREN       PIC X(10)      VALUE SPACES.
            03 PZM-HVREZAHLW      PIC 99         VALUE ZEROES.
            03 PZM-HVGARZEIT      PIC 99         VALUE ZEROES.
            03 PZM-HVRENTE        PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HVRENTH2       PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HVRENTH3       PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HVKAPAB        PIC 9(9)       VALUE ZEROES.
            03 PZM-HZVRENT1       PIC 9(9)V99    VALUE ZEROES.
            03 PZM-HZVP           PIC 999        VALUE ZEROES.
            03 PZM-HVRENTDMN      PIC 9(9)V99    VALUE ZEROES.
            03 PZM-AVMG           PIC X          VALUE SPACE.
            03 PZM-BBG            PIC X          VALUE SPACE.

      *                                                    81
      *---- 2.3.3 Berufsunfõhigkeits-Zusatzversicherung
            03 PZM-BuzKz          PIC X          VALUE SPACE.
            03 PZM-BerBuea        PIC 99         VALUE ZEROES.
            03 PZM-BuBarren1      PIC 9(9)V99    VALUE ZEROES.
            03 PZM-BuBn           PIC 99         VALUE ZEROES.
            03 PZM-BuBz           PIC 99         VALUE ZEROES.
            03 PZM-BuBt           PIC 99         VALUE ZEROES.
            03 PZM-BuBli          PIC 99         VALUE ZEROES.
            03 PZM-BuBlr          PIC 99         VALUE ZEROES.
            03 PZM-BuZi           PIC X          VALUE SPACE.
            03 PZM-BuZp           PIC 9(9)V9(2)  VALUE ZEROES.
            03 PZM-BuZr           PIC 9(9)V99    VALUE ZEROES.
            03 PZM-BuZd           PIC 99         VALUE ZEROES.
            03 PZM-BuzRis         PIC 9(3)V99    VALUE ZEROES.
            03 PZM-BuzBG          PIC 99         VALUE ZEROES.
      *                                                    56
      *---- 2.3.4 Dynamik
            03 PZM-DYDYNPROZ      PIC 99V9       VALUE ZEROES.
      *                                                     3
      *---- 2.3.5 Unfall-ZusatzVersicherung
            03 PZM-UZVKZ          PIC X          VALUE SPACE.
            03 PZM-UZVP           PIC 999V99     VALUE ZEROES.
            03 PZM-UZVS           PIC 9(9)V99    VALUE ZEROES.
            03 PZM-UZVRIS         PIC 9(3)V99    VALUE ZEROES.
      *                                                    22
      *---- 2.3.6 Erwerbsunfõhigkeits-Zusatzschutz
            03 PZM-EUN            PIC 9(9)V99    VALUE ZEROES.
            03 PZM-EUT            PIC 9(9)V99    VALUE ZEROES.
            03 PZM-EUZS           PIC X          VALUE SPACE.
            03 PZM-EUZRIS         PIC 9(3)V99    VALUE ZEROES.
            03 PZM-EUZBG          PIC 99         VALUE ZEROES.
      *                                                    30
      *---- 2.3.7 Depot
            03 PZM-Depot          PIC X          VALUE SPACE.
            03 PZM-SODepHoe       PIC 9(9)V99    VALUE ZEROES.
            03 PZM-SODEPINS       PIC 9(9)V99    VALUE ZEROES.
      *                                                    23
      *---- 2.3.8 Sonstige Daten
         02 Sonstige-Daten.
            03 PZM-GEWVA          PIC 9          VALUE ZERO.
            03 PZM-ASAMNR         PIC 9          VALUE ZERO.
            03 PZM-EKapWr         PIC X          VALUE SPACE.
            03 PZM-ENachV         PIC X          VALUE SPACE.
            03 PZM-EUmTau         PIC X          VALUE SPACE.
            03 PZM-EWiederAnl     PIC X          VALUE SPACE.
            03 PZM-BESV           PIC X          VALUE SPACE.
            03 PZM-GESKZ          PIC X          VALUE SPACE.
            03 PZM-EUROKZ         PIC 9          VALUE ZERO.
      *                                                     9
      *---- Freier Bereich
      *     03 FILLER           PIC X(21)      VALUE SPACES.
      *                                         Summe    1479
      *
       01  PO-DRKI1232.
           05  PO-DRKI1232-RC                 PIC  X(003).
               88 PO-DRKI1232-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1232-RC-NOK                       VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER ***************************************
