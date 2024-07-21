      *---------------------------------------------------------------*
      *    Standard f¸r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU616S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU616S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   May 14 1996 07:35:14   T13EX14
      *AFKD0091 Badenia neue Datenfelder
      *
      *   Rev 1.0   Nov 04 1995 10:03:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    20.01.94 16:45:28
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER AUFBEREITUNG KUNDENSAETZE FUER PC          *
      *    --------------------------------------------------          *
      *                                                                *
      *    NAME:    UU616S                                             *
      *                                                                *
      *    VARIABLE SATZLAENGE               ERSTELLT AM 18.05.90      *
      *             MINIMUM 150 BYTES        BIEHL                     *
      *             MAXIMUM 500 BYTES                                  *
      *                                                                *
      *             ADRESSTEIL                  ==>   180 B            *
      *             PGES = 02    PUNR = 0       ==>   280 B            *
      *                          PUNR > 0       ==>   150 B            *
      *             PGES = 03                   ==>   280 B            *
      *             PGES = 10    PGES-KZ = 01   ==>   280 B            *
      *                          PGES-KZ = 02   ==>   170 B            *
      *                          PGES-KZ = 03   ==>   293 B            *
      *                          PGES-KZ = 04   ==>   180 B            *
      *             PGES = 30,40,41,60          ==>   440 B            *
      *             PGES = 50                   ==>   500 B            *
      *             PGES = 70  - alt -          ==>   164 B            *
      *             PGES = 70  - neu -          ==>   330 B            *
      *             PGES = 80,81,82             ==>   291 B            *
      *                                                                *
      *    BEI DIESER DATEI HANDELT ES SICH UM EINE VSAM-DATEI MIT     *
      *    ORGANISATION ESDS.                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    GEAENDERT:                                 HECKER  23.10.90 *
      *    PROLAND IN PRECHTSSCHUTZ UMBENANNT                          *
      *                                                                *
      *    GEAENDERT:                                 KUHN    27.11.90 *
      *    FILLER VARIABLER TEIL AM-LEBEN GRUNDVERTRAG                 *
      *    UM 1 BYTE GEKUERZT UND EIN NEUES FELD                       *
      *    PVPRZ EINGEFUEGT.                                           *
      *                                                                *
      *     GEAENDERT:                         VON DER GOLTZ 12.08.93  *
      *       DEFINITION FUER EINEN TRENNSATZ (IN UU254 EINGEFUEGT):   *
      *       LAENGE = 180 BYTES:
      *        PSA-TRENNSATZ     SATZART = 0                           *
      *        PDATUM-TRENNSATZ  DATUM DER AKTUALISIERUNG (= 0)        *
      *        PKZ-TRENNSATZ     KZ ERSTAUST./AKTUALISIERUNG (= SPACE) *
      *        PPCNR-TRENNSATZ   PC WO AUSGELAGERTE STRUKTUR EIGENTLICH*
      *                          HINGEHOERT.                           *
      *        PVBNR-TRENNSATZ   VBNR DES BETREUERS, DER MIT SEINER    *
      *                          STRUKTUR AUSGELAGERT WIRD.            *
      *        PTEXT-TRENNSATZ   'AUSGELAGERTE STRUKTUR'               *
      *       AUSSERDEM:
      *        FILLER ADRESSTEIL                                       *
      *        UM 3 BYTES GEKUERZT UND ZWEI NEUE FELDER EIGEFUEGT:     *
      *         PVBNR-PZ         PRUEFZIFFER VBNR                      *
      *         PIDNR-PZ         PRUEFZIFFER KIDNR                     *
      *        IN FIXTEIL MUSS DESWEGEN KIDNR EXTRA DEFINIERT WERDEN.  *
      *                                                                *
      *     GEAENDERT:                         VON DER GOLTZ 20.01.94  *
      *       DEFINITION FUER DEN TRENNSATZ GEAENDERT.                 *
      *        PKZ-TRENNSATZ  =  2 BYTES                               *
      *                                                                *
      *     AFKD0091                                                   *
      *     GEAENDERT:                         W.SCHMIDT     14.05.96  *
      *      BADENIA NEUE FELDER EINGEBAUT. NEUE SATZL‰NGE = 330 BYTES *
      *----------------------------------------------------------------*
CCC009*    SKIP1
      *
      ***      TRENNSATZ
      *
120893 01  PTRENNSATZ.
           10  PSA-TRENNSATZ           PIC 9(001).
           10  PDATUM-TRENNSATZ        PIC 9(006).
200194*    10  PKZ-TRENNSATZ           PIC X(001).
200194     10  PKZ-TRENNSATZ           PIC X(002).
           10  PVBNR-TRENNSATZ         PIC 9(007).
           10  PPCNR-TRENNSATZ         PIC 9(005).
           10  PTEXT-TRENNSATZ         PIC X(025).
200194*    10  FILLER                  PIC X(135).
200194     10  FILLER                  PIC X(134).
      *
      ***      ADRESSTEIL
      *
       01  PADRESSTEIL.
           10  PSA                     PIC 9.
           10  PVBNR                   PIC 9(7).
120893*    10  PVBNR-PZ                PIC X(1).
           10  PIDNR                   PIC 9(7).
120893*    10  PIDNR-PZ                PIC X(2).
           10  PPCNR                   PIC 9(5).
           10  PANREDE                 PIC 9.
           10  PNAME                   PIC X(60).
           10  PSTR                    PIC X(26).
           10  PAUSL                   PIC XXX.
           10  PPLZ                    PIC 9(5).
           10  PORT                    PIC X(21).
           10  PGEBDAT                 PIC 9(6).
           10  PBEW-PUNKTE             PIC 9(9).
120893     10  FILLER                  PIC X(29).
120893*    10  FILLER                  PIC X(26).
      *
      ***      A M - L E B E N
      *
      *
      ***      VERTRAGSTEIL (IN JEDEM GRUND- UND FOLGESATZ VORHANDEN)
      *
       01  PAM-LEBEN.
120893*    10  FILLER                  PIC X(15).
120893     10  FILLER                  PIC X(08).
120893     10  PIDNR-FIX               PIC 9(07).
           10  PGES                    PIC 99.
           10  PGES-KZ                 PIC 99.
           10  PVNR                    PIC 9(15).
           10  PUNR                    PIC 99.
           10  PVBEG                   PIC 9(6).
           10  PVABL                   PIC 9(6).
           10  PZAHLW                  PIC 9.
           10  PBW-KZ                  PIC X.
      ***    GEAENDERT 25.10.88  B.POHL FOL. FELDER V ANFANG ANS ENDE
           10  PAEND-KZ                PIC 99.
           10  PAEND-DAT               PIC 9(6).
           10  PAEND-VBNR              PIC 9(7).
           10  PAEND-IDNR              PIC 9(7).
           10  PAVM                    PIC 9(7).
           10  FILLER                  PIC X(21).
      *
      ***      VARIABLER TEIL AM-LEBEN (GRUNDVERTRAG)
      *
           10  VARIABEL-PAM-LEBEN.
               20  PTARIF-AML          PIC X(10).
               20  PBA-KZ              PIC 9.
               20  PBA-DAT             PIC 9(6).
               20  PVSR                PIC 9(9).
               20  PRBTG               PIC 9(5)V99.
               20  PIART               PIC 99.
               20  PVZUST              PIC 9.
               20  PVZBETR             PIC 9(7)V99.
               20  PDYNVSU             PIC 9(9).
               20  PDYNRBTR            PIC 9(5)V99.
               20  PNAM2               PIC X(34).
               20  PGEB2               PIC 9(6).
               20  PNAM3               PIC X(34).
               20  PGEB3               PIC 9(6).
      ***    GEAENDERT 27.11.90  KUHN   WG. PRUEFZIFFER VOFUE
               20  PVPRZ               PIC X.
               20  FILLER              PIC X(38).
      *
      ***      A M - L E B E N - D Y N A M I K
      *
       01  PAML-DYNAMIK.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL AM-LEBEN-DYNAMIK
      *
           10  VARIABEL-DYNAMIK.
               20  PVSRD               PIC 9(9).
               20  PRBTGD              PIC 9(5)V99.
               20  PIARTD              PIC 99.
               20  PVZUSTD             PIC 9.
               20  PVZBETRD            PIC 9(7)V99.
               20  FILLER              PIC X(22).
      *
      ***      U N F A L L - V E R S.
      *
       01  PUNFALL.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL UNFALL-VERS.
      *
           10  VARIABEL-PUNFALL.
               20  PU-ZWGT             PIC X(3).
               20  PU-KZBG             PIC X.
               20  PU-BTG              PIC 9(7)V99.
               20  PU-DATEN         OCCURS 5.
                   30  PU-GBJAHR       PIC 99.
                   30  PU-VSUTOD       PIC 9(7).
                   30  PU-VSUINV       PIC 9(7).
                   30  PU-VSUKTG       PIC 9(3).
                   30  PU-VSUTG        PIC 9(3).
                   30  PU-VSUGG        PIC 9(3).
                   30  PU-VSUHK        PIC 9(3).
               20  FILLER              PIC X(27).
      *
      ***      H A F T P F L I C H T - V E R S.
      *
       01  PHAFTPFLICHT.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL HAFTPFLICHT-VERS.
      *
           10  VARIABEL-PHAFTPFLICHT.
               20  PH-ZWGT             PIC X(3).
               20  PH-BTG              PIC 9(7)V99.
               20  PH-WAGNMENG         PIC 9(7).
               20  PH-WAGNTEXT         PIC X(6).
               20  PH-VSUP             PIC 9(5).
               20  PH-VSUS             PIC 9(5).
               20  PH-VSUV             PIC 9(5).
               20  PH-VSUE             PIC 9(5).
               20  FILLER              PIC X(25).
      *
      ***      A M - S A C H
      *
       01  PAMS-SACH.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL AM-SACH
      *
           10  VARIABEL-AMS-SACH.
               20  PSACH-DATEN      OCCURS 4.
                   30  PS-ZWGT         PIC X(3).
                   30  PS-VSU          PIC 9(9).
                   30  PS-BTG          PIC 9(7)V99.
                   30  PS-GN           PIC X.
                   30  PS-VGEF         PIC X(21).
               20  FILLER              PIC X(21).
      *
      ***      K F Z -, M O P E D - V E R S.
      *
       01  PKFZ.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL  KFZ-, MOPED-VERS.
      *
           10  VARIABEL-PKFZ.
               20  PKFZ-ZWGT           PIC X(3).
               20  PKFZ-AKZ            PIC X(12).
               20  PKFZ-RUHE           PIC X.
               20  PKFZ-WKZ            PIC X(3).
               20  PKFZ-ZUJAHR         PIC 99.
               20  PKFZ-DATEN       OCCURS 3.
                   30 PKFZ-DS          PIC XX.
                   30 PKFZ-BTG         PIC 9(5)V99.
                   30 PKFZ-RBKL        PIC 9(3).
               20  FILLER              PIC X(23).
      *
      ***      B H I   D I T   I I I   H Y P O
      *
       01  PBANKEN.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL BHI DIT III HYPO
      *
           10  VARIABEL-PBANKEN.
               20  PPRODTAB         OCCURS 16.
                   30  PPROD           PIC XX.
                   30  PKTO            PIC 9(6)V999.
                   30  PSUMME          PIC 9(9).
               20  FILLER              PIC X(20).
      *
      ***      C K V
      *
       01  PCKV.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL CKV
      *
           10  VARIABEL-PCKV.
               20  PVNAME              PIC X(10).
               20  PLAST               PIC X.
               20  PGS                 PIC 9(3).
               20  PGEBD               PIC 9(6).
               20  PVBEGP              PIC 9(4).
               20  PVABLP              PIC 9(4).
               20  PTARIFTAB        OCCURS 10.
                   30  PTARIF          PIC X(8).
                   30  PBEITR          PIC 9(5)V99.
                   30  PZUGV           PIC 9(4).
                   30  PABGV           PIC 9(4).
                   30  PFORM           PIC X.
                   30  PSBET           PIC 9(5).
                   30  PLEIST          PIC 9(5).
               20  FILLER              PIC X(32).
      *
      ***      B A D E N I A
      *
       01  PBADENIA.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL BADENIA
      *
           10  VARIABEL-PBADENIA.
               20  PVSUM               PIC 9(7).
               20  PBEITRAG            PIC 9(5)V99.
               20  PDARL               PIC 9(7).
               20  PPRODTEXT           PIC X(12).
               20  PKONTOST            PIC X.
               20  PGHABEN             PIC 9(7)V99.
               20  PGHABEN-VORZ        PIC 9.
      ***    GEAENDERT 25.10.88  B.POHL PBWZAHL UM 4 NACHKOMMAST. ERW.
               20  PBWZAHL             PIC 99V9(5).
140596         20  PTARIF-T3           PIC X(01).
140596         20  PZINS-ALLG          PIC 9(3)V99.
140596         20  PZINS-DARLEHEN      PIC 9(3)V99.
140596         20  PDISAGIO            PIC X(1).
140596         20  PFREI-BETRAG        PIC 9(5)V99.
140596         20  PVFREI-BETRAG       PIC 9(5)V99.
140596         20  PFEHL-BETRAG        PIC 9(7).
140596         20  PENDE-ST-LAUFZEIT   PIC 9(8).
140596         20  PUMS-TABELLE.
140596             25 PUMS-TAB         OCCURS 5.
140596                30 PUMS-BETRAG-V PIC 9(1).
140596                30 PUMS-BETRAG   PIC 9(7)V99.
140596                30 PUMS-SCHL     PIC 9(3).
140596                30 PUMS-DATUM    PIC 9(6).
140596         20  PDATUM-WOHN         PIC 9(8).
140596         20  PDATUM-ZUTEIL       PIC 9(8).
140596         20  PBLAND              PIC X(2).
140596         20  PFEHL-BETRAG-DATUM  PIC 9(8).
140596         20  FILLER              PIC X(17).
140596*        20  FILLER              PIC X(13).
      *
      ***      R E C H T S S C H U T Z
      *
       01  PRECHTSSCHUTZ.
           10  FILLER                  PIC X(100).
      *
      ***      VARIABLER TEIL RECHTSSCHUTZ
      *
           10  VARIABEL-PRECHTSSCHUTZ.
               20  PVSU                PIC 9(7).
               20  PPRAEMIE            PIC 9(6)V99.
               20  PLBADAT             PIC 9(7).
               20  PLASTR              PIC X.
               20  PVTEXT-TAB       OCCURS 5.
                   30  PVTEXT          PIC X(25).
               20  PVART-TAB        OCCURS 5.
                   30  PVART           PIC XXX.
               20  FILLER              PIC X(28).

