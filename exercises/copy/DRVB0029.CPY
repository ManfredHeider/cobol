      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0029.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0029.CPv  $
      *
      *   Rev 2.2   Feb 10 2000 17:39:28   T13AE42
      *AVB00474 - Korrektur
      *
      *   Rev 2.1   Feb 10 2000 17:14:08   T13AE42
      *AVB00474 - Anpassung wg. neuem
      *VB-Vertrag
      *
      *   Rev 2.0   Jun 17 1999 14:42:28   T13AE28
      *AVB00426: Anpassungen für Jahr2000
      *
      *   Rev 1.2   Jun 17 1999 14:41:10   T13AE28
      *AVB00426: Anpassungen für Jahr2000
      *
      *   Rev 1.1   17 Dec 1998 08:35:02   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:08:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: (DRVB0029)                                    *
      *    ZENTRALE DATENFELDER DER SCHNITTSTELLENMODULE              *
      *    SM1, SM2, SM11 FÜR ONLINE-ALTANWENDUNGEN                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: O. NAUMANN                                       *
      *    DATUM:    14.04.1993                                       *
      *****************************************************************
      *    VERSION   02   01.10.93                                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *
      *         !30.09.93!    ! EINFÜHRUNG VON I-IND2,    !          *
      *         !        !    ! K-ANSCHR-TYP,K-ANZ-ANSCHR ! NAUMANN  *
      *         !01.10.93! 02 ! PROD.-UEBERGABE           ! NAUMANN  *
      * AVB00474!10.02.00! 03 ! AUSBAU VB-VERTRAG         ! SCHMIDT,B*
      *         !        !    ! NEU DRAT0045              !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *      MAX. WIEDERHOLUNG BEI RESOURCE NICHT VERFÜGBAR/DEADLOCK
       01    WK-LESEN-MAX         PIC S9(006)
                VALUE             +5.
       01    WK-FEHLER-TEXT1.
          05 FILLER               PIC  X(030) VALUE
             'DATENINKONSISTENZ, ST-NR EX., '.
          05 WK-TABLE-KEY         PIC  X(010).
          05 FILLER               PIC  X(019) VALUE
             'EX. NICHT. ST-NR = '.
          05 WK-ST-NR1            PIC  9(007).
       01    WK-FEHLER-TEXT2.
          05 FILLER               PIC  X(041) VALUE
             'DATENINKONSISTENZ, ES EX. KEINE AKTUELLE '.
          05 WK-TABLE-NAME        PIC  X(004).
          05 FILLER               PIC  X(013) VALUE
             ' FÜR ST-NR = '.
          05 WK-ST-NR2            PIC  9(007).
       01    WK-FEHLER-TEXT3.
          05 FILLER               PIC  X(042) VALUE
             'ES EX. EIN BVH UND EIN EDD ZU EINEM ZTPKT.'.
          05 FILLER               PIC  X(013) VALUE
             ' FÜR ST-NR = '.
          05 WK-ST-NR3            PIC  9(007).
       01    WK-FEHLER-TEXT4.
          05 FILLER               PIC  X(041) VALUE
             'SCHNITTSTELLENMODUL IST NICHT FÜR ALTPGM '.
          05 WK-PGM-ID            PIC  X(008).
          05 FILLER               PIC  X(012) VALUE
             ' KONZIPIERT.'.
       01    WK-FEHLER-TEXT5.
          05 FILLER               PIC  X(020) VALUE
             'SATZ FEHLT IN TABLE '.
          05 WK-TABLE-NAME5       PIC  X(004).
          05 FILLER               PIC  X(014) VALUE
             ' FÜR KEY-ID = '.
          05 WK-KEY-ID5           PIC  9(010).
       01    WK-FEHLER-TEXT6.
          05 FILLER               PIC  X(025) VALUE
             'LOG. FEHLER IN PARAGRAPH '.
          05 WK-PARAGRAPH         PIC  X(032).
       01    WK-FEHLER-TEXT7.
          05 FILLER               PIC  X(042) VALUE
             'KL/JUR KANN NICHT IN DAS ALT-SYSTEM KONV. '.
          05 FILLER               PIC  X(007) VALUE
             'WERDEN.'.
          05 FILLER               PIC  X(013) VALUE
             ' FÜR ST-NR = '.
          05 WK-ST-NR7            PIC  9(007).
       01    WK-FEHLER-TEXT8.
          05 FILLER               PIC  X(047) VALUE
             'MAX. ANZAHL ZU ERZEUGENDER HISTORIEN ERREICHT. '.
          05 FILLER               PIC  X(013) VALUE
             ' FÜR ST-NR = '.
          05 WK-ST-NR8            PIC  9(007).
       01    WK-FEHLER-TEXT9.
          05 FILLER               PIC  X(012) VALUE
             'ES EX. KEIN '.
          05 WK-TABLE-NAME9       PIC  X(004).
          05 FILLER               PIC  X(018) VALUE
             'ZU EINEM ZEITRAUM.'.
          05 FILLER               PIC  X(013) VALUE
             ' FÜR ST-NR = '.
          05 WK-KEY-ID9           PIC  9(010).

      *      KONSTANTEN
       01    K-KONSTANTEN.
          05 K-RF-R               PIC  9(003) COMP-3 VALUE 889.
          05 K-RF-A               PIC  9(003) COMP-3 VALUE 890.
          05 K-ST-BELEGT          PIC  X(001) VALUE 'N'.
          05 K-TAETIG             PIC  X(001) VALUE '1'.
          05 K-UNTAETIG           PIC  X(001) VALUE '0'.
          05 K-VM09               PIC  9(003) COMP-3 VALUE 009.
          05 K-MAX-ANZ-HIST       PIC  S9(04) COMP   VALUE 50.
          05 K-DAT-BIS-MAX-TSTMP-AA  PIC  X(26)
                            VALUE '9999-12-31-24.00.00.000000'.
          05 K-DAT-BIS-MAX-DATE-AA   PIC  X(10)
                            VALUE '31.12.9999'.
          05 K-DAT-BIS-MAX-VSAM   PIC  9(6)   COMP-3 VALUE 999999.
          05 K-EOF                PIC  S9(4)  COMP   VALUE 100.
100200*   05 K-VB-VERTRAG         PIC S9(004)    COMP    VALUE 2.
          05 K-ZUSTELLUNG-VB1.
             10 FILLER            PIC  S9(04) COMP   VALUE 11.
             10 FILLER            PIC  S9(04) COMP   VALUE 22.
             10 FILLER            PIC  S9(04) COMP   VALUE 01.
             10 FILLER            PIC  S9(04) COMP   VALUE 02.
             10 FILLER            PIC  S9(04) COMP   VALUE 12.
             10 FILLER            PIC  S9(04) COMP   VALUE 21.
          05 K-ZUSTELL-TAB-VB1 REDEFINES K-ZUSTELLUNG-VB1.
             10 K-KNR-ZUSTELLUNG-VB1  PIC  S9(04) COMP OCCURS 6.
          05 K-ANZ-ZUSTELL-VB1        PIC  S9(04) COMP VALUE 6.
          05 K-ZUSTELLUNG-VB2.
             10 FILLER            PIC  S9(04)    COMP    VALUE ZERO.
             10 FILLER            PIC  S9(04)    COMP    VALUE 10.
             10 FILLER            PIC  S9(04)    COMP    VALUE 20.
             10 FILLER            PIC  S9(04)    COMP    VALUE 12.
             10 FILLER            PIC  S9(04)    COMP    VALUE 21.
          05 K-ZUSTELL-TAB-VB2 REDEFINES K-ZUSTELLUNG-VB2.
             10 K-KNR-ZUSTELLUNG-VB2  PIC  S9(04) COMP OCCURS 5.
          05 K-ANZ-ZUSTELL-VB2        PIC  S9(04) COMP VALUE 5.
300993    05 K-ANSCHR-TYP.
300993       10 FILLER            PIC  S9(04)    COMP    VALUE 01.
300993       10 FILLER            PIC  S9(04)    COMP    VALUE 02.
300993    05 K-ANSCHR-TAB REDEFINES K-ANSCHR-TYP.
300993       10 K-KNR-ANSCHR          PIC  S9(04) COMP OCCURS 2.
300993    05 K-ANZ-ANSCHR             PIC  S9(04) COMP VALUE 2.

       01   INDIZES.

         05  IND-I                PIC S9(006)    COMP-3  VALUE ZERO.
         05  I-INDEX              PIC S9(006)    COMP-3  VALUE ZERO.
         05  I                    PIC S9(006)    COMP-3  VALUE ZERO.
         05  J                    PIC S9(006)    COMP-3  VALUE ZERO.
         05  K                    PIC S9(006)    COMP-3  VALUE ZERO.
         05  I-IND1               PIC S9(006)    COMP-3  VALUE ZERO.
300993   05  I-IND2               PIC S9(006)    COMP-3  VALUE ZERO.

      *      ZWISCHENBEREICHE
       01    Z-ZWISCHENBEREICH.
          05 Z-ST-NR              PIC  S9(09)    COMP.
          05 Z-SQLCODE            PIC  S9(09)    COMP.
          05 Z-VORNAME-LAENG      PIC  S9(04)    COMP.
          05 Z-TITEL-ADEL-LAENG   PIC  S9(04)    COMP.
          05 Z-TITEL-AKAD-LAENG   PIC  S9(04)    COMP.
          05 Z-PROV-STZ-AP        PIC  S999V99   COMP-3.
          05 Z-PROV-STZ-GP        PIC  S999V99   COMP-3.
          05 Z-PROZ-STZ           PIC  S999V99   COMP-3.
          05 Z-VBP-NR-1           PIC  9.
          05 Z-VSAM-DAT           PIC  9(6).
          05 Z-VSAM-DAT1 REDEFINES Z-VSAM-DAT.
             10 Z-JJ              PIC  9(002).
             10 Z-MM              PIC  9(002).
             10 Z-TT              PIC  9(002).
      *      ZWISCHENFELDER, DIE DEN VB1-HISTORIEN-DATENFELDERN ENT-
      *      SPRECHEN
          05 Z-VONDAT             PIC  9(6)      COMP-3.
          05 Z-BISDAT             PIC  9(6)      COMP-3.
          05 Z-DIR                PIC  999       COMP-3.
          05 Z-STUFE              PIC  999       COMP-3.
          05 Z-STATUS             PIC  X.
          05 Z-BMANR              PIC  9(7)      COMP-3.
          05 Z-AUSPROV            PIC  999V9     COMP-3.
          05 Z-EPROVKZ            PIC  X.
          05 Z-VARPROV            PIC  9(5)V9    COMP-3.
      *      SONSTIGE ZWISCHENFELDER
          05 Z-STRASSE            PIC  X(20).
          05 Z-PLZ                PIC  X(6).
          05 Z-AUSL               PIC  X(3).
          05 Z-ORT                PIC  X(16).
          05 Z-INSTITUT           PIC  X(22).
          05 Z-KTOINH             PIC  X(25).
          05 Z-SPERRE             PIC  9(1).
          05 Z-BLZ                PIC  9(8)      COMP-3.
          05 Z-KTONR              PIC  9(11)     COMP-3.
          05 Z-SPACE              PIC  99 VALUE ZERO.
      *      DATUMS-FORMATE
       01    Z-ZTPKT-DATE-ISO.
          05 Z-JJJJ               PIC  9(004).
          05 Z-STRICH1            PIC  X.
          05 Z-MM                 PIC  9(002).
          05 Z-STRICH2            PIC  X.
          05 Z-TT                 PIC  9(002).
       01    Z-ZTPKT-DATE-EUR.
          05 Z-TT                 PIC  9(002).
          05 Z-PUNKT1             PIC  X.
          05 Z-MM                 PIC  9(002).
          05 Z-PUNKT2             PIC  X.
          05 Z-JJJJ               PIC  9(004).
       01    Z-ZTPKT-DATE1.
          05 Z-TT                 PIC  9(002).
          05 FILLER               PIC  X(001) VALUE '.'.
          05 Z-MM                 PIC  9(002).
          05 FILLER               PIC  X(003) VALUE '.19'.
          05 Z-JJ                 PIC  9(002).
       01    Z-ZTPKT-DATE         PIC  X(10).
       01    Z-ZTPKT-TSTMP1.
          05 FILLER               PIC  X(002) VALUE '19'.
          05 Z-JJ                 PIC  9(002).
          05 FILLER               PIC  X(001) VALUE '-'.
          05 Z-MM                 PIC  9(002).
          05 FILLER               PIC  X(001) VALUE '-'.
          05 Z-TT                 PIC  9(002).
          05 FILLER               PIC  X(001) VALUE '-'.
          05 FILLER               PIC  X(015) VALUE '24.00.00.000000'.
       01    Z-ZTPKT-TSTMP        PIC  X(26).
       01    Z-VGL-DATE-1         PIC  X(10).
       01    Z-VGL-DATE-2         PIC  X(10).
      *
      *01    WB-KTO-NR            PIC  X(11) JUST RIGHT.
      *
       01    WK-ERGEBNIS          PIC  S9(4)  COMP.
       01    WK-SUMME             PIC  S9(4)  COMP.
       01    WK-MODULO            PIC  S9(4)  COMP.
      *
       01    WK-BVH-DAT-VON       PIC  X(26).
       01    WK-EDD-DAT-VON       PIC  X(26).
       01    WK-EDT-DAT-VON       PIC  X(26).
       01    WK-MAT-DAT-VON       PIC  X(26).
       01    WK-DZ-DAT-VON        PIC  X(26).
       01    WK-MSBZ-DAT-VON      PIC  X(26).
       01    WK-IA-DAT-VON        PIC  X(26).
       01    WK-RF-DAT-VON        PIC  X(26).
       01    WK-VBPZ-DAT-VON      PIC  X(26).
       01    WK-EDED-DAT-VON      PIC  X(26).
      *
      *      ARBEITSVARIABLEN FÜR VERARBEITUNG HISTORIE DES MA
       01    WK-MIN-DAT-VON       PIC  X(26).
       01    WK-MIN-VSAM-VONDAT   PIC  9(6)             COMP-3.
       01    WK-TAETIG-AUS-VON    PIC  9(006)           COMP-3.
       01    WK-TAETIG-AUS-BIS    PIC  9(006)           COMP-3.
       01    WK-IA-AUS-VON        PIC  9(006)           COMP-3.
       01    WK-IA-AUS-STUFE      PIC  9(003)           COMP-3.
       01    WK-RF-AUS-VON        PIC  9(006)           COMP-3.
       01    WK-RF-AUS-STUFE      PIC  9(003)           COMP-3.
       01    WK-MS-NR-UNTST       PIC S9(004)           COMP.
       01    WK-BS-NR-UNTST       PIC S9(004)           COMP.
       01    WK-VBP-NR            PIC S9(004)           COMP.
       01    WK-PROV-GP-UNTST     PIC S9(003)V99        COMP-3.
       01    WK-PROV-GP-BETR      PIC S9(003)V99        COMP-3.
       01    WK-MA-ST-NR-BETR     PIC S9(009)           COMP.
       01    WK-ED-ST-NR-BETR     PIC S9(009)           COMP.
       01    WK-PROV-AP-UNTST     PIC S9(003)V99        COMP-3.
       01    WK-PROZ-STZ          PIC S9(003)V99        COMP-3.
       01    WK-BETREUER-TYP      PIC  X(004).
       01    WK-ZEITRAUM-ANFANG   PIC  9(006)           COMP-3.
       01    WK-ZEITRAUM-ENDE     PIC  9(006)           COMP-3.
       01    WK-ANZ-RF            PIC S9(004)           COMP.
       01    WK-VSAM-DATUM        PIC  9(006)           COMP-3.
       01    WK-RF-VSAM-VON-BETR  PIC  9(006)           COMP-3.
       01    WK-RF-VSAM-BIS-BETR  PIC  9(006)           COMP-3.
       01    WK-MAT-ED-PROV-DAT   PIC  9(006)           COMP-3.
       01    WK-RF-DAT-VON-BETR   PIC  X(26).
       01    WK-RF-DAT-BIS-BETR   PIC  X(10).

