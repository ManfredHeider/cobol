      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/06/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0469.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0469.CPv  $
      *
      *   Rev 2.4   Jun 17 2020 10:19:24   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.3   Jun 23 2016 08:45:02   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZT/ZM
      *
      *   Rev 2.2   Oct 08 2014 13:44:32   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:07:04   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Jun 6 2014 11:14:28   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G10KSDSSUHSA401 TABLE
CCC004*    ( G10EZVVERTRAGSNR               CHAR(15) NOT NULL,
CCC004*      G10EZVHOSTNR                   CHAR(15) NOT NULL,
CCC004*      G10EZVSPARTE                   INTEGER NOT NULL,
CCC004*      G10EZVPBRDNR                   INTEGER NOT NULL,
CCC004*      G10EZVBEGINN                   DATE NOT NULL,
CCC004*      G10EZVABLAUF                   DATE NOT NULL,
CCC004*      G10EZVSTATUS                   CHAR(9) NOT NULL,
CCC004*      G10EZVNVEZBTRG                 DECIMAL(10, 2) NOT NULL,
CCC004*      G10EZVSUMME                    INTEGER NOT NULL,
CCC004*      G10EZVVART                     INTEGER NOT NULL,
CCC004*      G10EZVAENDGRD                  CHAR(50) NOT NULL,
CCC004*      G10EZVAENDDAT                  DATE NOT NULL,
CCC004*      G10EZVNETJBTR                  DECIMAL(10, 2) NOT NULL,
CCC004*      G10EZVBRUTJBTR                 DECIMAL(10, 2) NOT NULL,
CCC004*      G10EZVNBTRZW                   DECIMAL(10, 2) NOT NULL,
CCC004*      G10EZVLETBEARB                 DATE NOT NULL,
CCC004*      G10EZVZWEIG                    CHAR(2) NOT NULL,
CCC004*      G10EZVRABNETTO                 DECIMAL(10, 2) NOT NULL,
CCC004*      G10EZVSUAKTBTR                 DECIMAL(10, 2) NOT NULL,
CCC004*      G10EZVREOBPRDNR                CHAR(5) NOT NULL,
CCC004*      ZUGANG                         TIMESTAMP(12) NOT NULL,
CCC004*      VON                            TIMESTAMP(12) NOT NULL,
CCC004*      BIS                            TIMESTAMP(12) NOT NULL,
CCC004*      G10EZVPRODBEZ                  CHAR(50),
CCC004*      G10EZVFLEXNSP                  DECIMAL(11, 2),
CCC004*      G10EZVFLEXNSPGW                CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G10KSDSSUHSA401             *
      ******************************************************************
       01  G10KSDSSUHSA401-GESAMT.
        05 NI-G10KSDSSUHSA401-TAB.
           10 NI-G10KSDSSUHSA401-ELE PIC S9(04) COMP OCCURS 26 TIMES.
        05 NI-G10KSDSSUHSA401 REDEFINES NI-G10KSDSSUHSA401-TAB.
           10 NI-G10EZVVERTRAGSNR   PIC S9(4) COMP.
           10 NI-G10EZVHOSTNR       PIC S9(4) COMP.
           10 NI-G10EZVSPARTE       PIC S9(4) COMP.
           10 NI-G10EZVPBRDNR       PIC S9(4) COMP.
           10 NI-G10EZVBEGINN       PIC S9(4) COMP.
           10 NI-G10EZVABLAUF       PIC S9(4) COMP.
           10 NI-G10EZVSTATUS       PIC S9(4) COMP.
           10 NI-G10EZVNVEZBTRG     PIC S9(4) COMP.
           10 NI-G10EZVSUMME        PIC S9(4) COMP.
           10 NI-G10EZVVART         PIC S9(4) COMP.
           10 NI-G10EZVAENDGRD      PIC S9(4) COMP.
           10 NI-G10EZVAENDDAT      PIC S9(4) COMP.
           10 NI-G10EZVNETJBTR      PIC S9(4) COMP.
           10 NI-G10EZVBRUTJBTR     PIC S9(4) COMP.
           10 NI-G10EZVNBTRZW       PIC S9(4) COMP.
           10 NI-G10EZVLETBEARB     PIC S9(4) COMP.
           10 NI-G10EZVZWEIG        PIC S9(4) COMP.
           10 NI-G10EZVRABNETTO     PIC S9(4) COMP.
           10 NI-G10EZVSUAKTBTR     PIC S9(4) COMP.
           10 NI-G10EZVREOBPRDNR    PIC S9(4) COMP.
           10 NI-ZUGANG             PIC S9(4) COMP.
           10 NI-VON                PIC S9(4) COMP.
           10 NI-BIS                PIC S9(4) COMP.
           10 NI-G10EZVPRODBEZ      PIC S9(4) COMP.
           10 NI-G10EZVFLEXNSP      PIC S9(4) COMP.
           10 NI-G10EZVFLEXNSPGW    PIC S9(4) COMP.
        05 DCL-G10KSDSSUHSA401.
           10 G10EZVVERTRAGSNR     PIC X(15).
           10 G10EZVHOSTNR         PIC X(15).
           10 G10EZVSPARTE         PIC S9(9) USAGE COMP.
           10 G10EZVPBRDNR         PIC S9(9) USAGE COMP.
           10 G10EZVBEGINN         PIC X(10).
           10 G10EZVABLAUF         PIC X(10).
           10 G10EZVSTATUS         PIC X(9).
           10 G10EZVNVEZBTRG       PIC S9(8)V9(2) USAGE COMP-3.
           10 G10EZVSUMME          PIC S9(9) USAGE COMP.
           10 G10EZVVART           PIC S9(9) USAGE COMP.
           10 G10EZVAENDGRD        PIC X(50).
           10 G10EZVAENDDAT        PIC X(10).
           10 G10EZVNETJBTR        PIC S9(8)V9(2) USAGE COMP-3.
           10 G10EZVBRUTJBTR       PIC S9(8)V9(2) USAGE COMP-3.
           10 G10EZVNBTRZW         PIC S9(8)V9(2) USAGE COMP-3.
           10 G10EZVLETBEARB       PIC X(10).
           10 G10EZVZWEIG          PIC X(2).
           10 G10EZVRABNETTO       PIC S9(8)V9(2) USAGE COMP-3.
           10 G10EZVSUAKTBTR       PIC S9(8)V9(2) USAGE COMP-3.
           10 G10EZVREOBPRDNR      PIC X(5).
           10 ZUGANG               PIC X(32).
           10 VON                  PIC X(32).
           10 BIS                  PIC X(32).
           10 G10EZVPRODBEZ        PIC X(50).
           10 G10EZVFLEXNSP        PIC S9(9)V9(2) USAGE COMP-3.
           10 G10EZVFLEXNSPGW      PIC X(1).
      ************************** ENDE-DCLGEN *************************

