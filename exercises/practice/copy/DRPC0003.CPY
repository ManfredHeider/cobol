      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPC0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPC0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:47:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:01:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    25.08.93 16:02:05
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BA-FTP-DATEI AM-VERS     DRPC0003         *
      *    -------------------------                                   *
      *                                                                *
      *    LAENGE = 700 BYTES        ERSTELLT AM 05.02.93 / BORK       *
      *                                                                *
      *    BAZ-DATEI FUER PC: INDEX-DATEI MIT INDEX = BAZ-SCHL         *
      *                      DATEINAME = U.UU0222                      *
      *                                                                *
      *    BAZ-DATEI FUER FFM: OHNE INDEX                              *
      *                       DATEINAME = U.UU0223 (UNSORTIERT)        *
      *                       DATEINAME = U.UU0224 (SORTIERT)          *
      *                       DATEINAME = U.UU0225 (SORTIERT)          *
      *                                                                *
      *    BAZ-SATZ  =  BESUCHSAUFTRAGS-ZWISCHEN-SATZ                  *
      *                 =      =        =        ====                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    SKIP1
       01  DRPC0003.
           05  BAZ-SCHL.
               10  BAZ-PCNR                PIC  9(005).
               10  BAZ-GES                 PIC  9(002).
      * 10 = AM-VERS
               10  BAZ-VSNR                PIC  9(015).
               10  BAZ-ABSDAT              PIC  9(006).
               10  BAZ-GES-KZ              PIC  9(002).
      * SPARTEN-KZ:  04 = KFZ  00 = SONSTIGE
           05  BAZ-SATZ.
               10  BAZ-STATKZ              PIC  9(001).
               10  BAZ-STATDAT             PIC  9(006).
               10  BAZ-ART                 PIC  9(001).
               10  BAZ-VBNR                PIC  9(007).
               10  BAZ-VBNAM               PIC  X(060).
               10  BAZ-PIF                 PIC  X(015).
               10  BAZ-V-RUEDAT            PIC  9(006).
               10  BAZ-VN.
                   15  BAZ-V-VNNAM         PIC  X(039).
                   15  BAZ-V-VNSTR         PIC  X(025).
                   15  BAZ-V-VNPLZ         PIC  X(006).
                   15  BAZ-V-VNORT         PIC  X(026).
                   15  BAZ-V-VNGEBDAT      PIC  9(006).
               10  BAZ-V-VDIR.
                   15  BAZ-V-VDNR          PIC  X(003).
                   15  BAZ-V-VD-ORT        PIC  X(009).
                   15  BAZ-V-VD-TELNR      PIC  X(018).
               10  BAZ-V-BTGRUECKD.
                   15  BAZ-V-BTGRUECKDM    PIC S9(007)V99   COMP-3.
                   15  BAZ-V-MAHNDAT       PIC  9(006).
                   15  BAZ-V-BTG-R-M       PIC S9(007)V99   COMP-3.
               10  BAZ-V-BANKVERB.
                   15  BAZ-V-KTONR         PIC  X(010).
                   15  BAZ-V-BLZ           PIC  9(008).
               10  BAZ-V-ZUST              PIC  X(010).
               10  BAZ-V-BEG               PIC  9(006).
               10  BAZ-V-ABL               PIC  9(006).
               10  BAZ-V-ZAHLW             PIC  X(012).
               10  BAZ-V-INKART            PIC  X(012).
               10  BAZ-V-OSCHUTZ           PIC  9(006).
               10  BAZ-V-KFZ-DATEN.
                   15  BAZ-V-AMTL-KZ       PIC  X(012).
                   15  BAZ-V-KFZART        PIC  9(003).
                   15  BAZ-V-VERWEND       PIC  X(025).
                   15  BAZ-V-HERST         PIC  X(016).
                   15  BAZ-V-LEIST-KW      PIC  9(005)      COMP-3.
                   15  BAZ-V-LEIST-PS      PIC  9(005)      COMP-3.
               10  BAZ-V-BERUF             PIC  X(012).
               10  BAZ-V-HPF-DATEN.
                   15  BAZ-V-DECK-HPF      PIC  X(026).
                   15  BAZ-V-JBTG-HPF      PIC S9(007)V99   COMP-3.
                   15  BAZ-V-SF-HPF        PIC  9(002).
                   15  BAZ-V-RAB-HPF       PIC  9(003)      COMP-3.
               10  BAZ-V-VK-TK.
                   15  BAZ-V-VK            PIC  X(002).
                   15  BAZ-V-SB-VK         PIC  X(012).
                   15  BAZ-V-TK            PIC  X(002).
                   15  BAZ-V-SB-TK         PIC  X(012).
                   15  BAZ-V-JBTG-VK-TK    PIC S9(007)V99   COMP-3.
                   15  BAZ-V-SF-VK-TK      PIC  9(002).
                   15  BAZ-V-RAB-VK-TK     PIC  9(003)      COMP-3.
                   15  BAZ-V-SONDER        PIC  X(004).
               10  BAZ-V-UNF.
                   15  BAZ-V-U-TOD         PIC  9(005).
                   15  BAZ-V-U-TOD-TXT     PIC  X(012).
                   15  BAZ-V-U-INV         PIC  9(005).
                   15  BAZ-V-U-TOD1        PIC  9(005).
                   15  BAZ-V-U-TOD1-TXT    PIC  X(012).
                   15  BAZ-V-U-INV1        PIC  9(005).
                   15  BAZ-V-JBTG-UNF      PIC S9(007)V99   COMP-3.
               10  BAZ-V-SONST.
                   15  BAZ-V-BTRKZ         PIC  9(001).
                   15  BAZ-V-VBNR-ALT      PIC  9(006).
                   15  BAZ-V-VB-ALT-NAM    PIC  X(060).
                   15  BAZ-V-VBTEL         PIC  X(014).
                   15  BAZ-V-VBFAX         PIC  X(014).
                   15  FILLER              PIC  X(062).
      *
           05  BAZ-DUMMYSATZ    REDEFINES     BAZ-SATZ.
               10  BAZ-BEREINDAT           PIC  9(007)      COMP-3.
               10  BAZ-ANZNEU              PIC  9(007)      COMP-3.
               10  FILLER                  PIC  X(692).
