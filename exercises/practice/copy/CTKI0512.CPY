      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0512.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0512.CPv  $
      *
      *   Rev 2.6   May 10 2019 12:53:42   t13ae52
      *AKI01680: G42: Erweiterung Datenspiegel
      *
      *   Rev 2.5   Nov 22 2018 10:00:50   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.4   Nov 05 2018 15:48:04   t13ex78
      *AKI01615: Erm./Aufb./Akt. DEPOTSTAMM
      *
      *   Rev 2.3   Oct 27 2018 15:16:14   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.2   Oct 25 2018 14:37:10   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.0   Sep 7 2018 11:04:21   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G42_DEPOTSTAMM TABLE
CCC004*    ( G42DVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G42DOEFFNUNGSDAT               CHAR(8) NOT NULL,
CCC004*      G42DSTATUS                     CHAR(1) NOT NULL,
CCC004*      G42DRISIKOKLASSE               CHAR(1) NOT NULL,
CCC004*      G42DANLAGEZIEL                 CHAR(1) NOT NULL,
CCC004*      G42DDEPOTART                   CHAR(1) NOT NULL,
CCC004*      G42DIBAN                       CHAR(34) NOT NULL,
CCC004*      G42DBIC                        CHAR(11) NOT NULL,
CCC004*      G42DMANDREF                    CHAR(35) NOT NULL,
CCC004*      G42DKONTOINHABER               CHAR(30) NOT NULL,
CCC004*      G42DNRGWINKLBBA                DECIMAL(13, 2) NOT NULL,
CCC004*      G42DNRGWBBAVH                  DECIMAL(8, 2) NOT NULL,
CCC004*      G42DNRGWOHNEBBA                DECIMAL(13, 2) NOT NULL,
CCC004*      G42DNRGWOBBAVH                 DECIMAL(8, 2) NOT NULL,
CCC004*      G42DRGWUDEPOT                  DECIMAL(11, 2) NOT NULL,
CCC004*      G42DONLINEKZ                   CHAR(1) NOT NULL,
CCC004*      G42DWPHGAKTUDAT                CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_DEPOTSTAMM              *
      ******************************************************************
       01  G42-DEPOTSTAMM-GESAMT.
        05 NI-G42-DEPOTSTAMM-TAB.
           10 NI-G42-DEPOTSTAMM-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-G42-DEPOTSTAMM REDEFINES NI-G42-DEPOTSTAMM-TAB.
           10 NI-G42DVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G42DOEFFNUNGSDAT   PIC S9(4) COMP.
           10 NI-G42DSTATUS         PIC S9(4) COMP.
           10 NI-G42DRISIKOKLASSE   PIC S9(4) COMP.
           10 NI-G42DANLAGEZIEL     PIC S9(4) COMP.
           10 NI-G42DDEPOTART       PIC S9(4) COMP.
           10 NI-G42DIBAN           PIC S9(4) COMP.
           10 NI-G42DBIC            PIC S9(4) COMP.
           10 NI-G42DMANDREF        PIC S9(4) COMP.
           10 NI-G42DKONTOINHABER   PIC S9(4) COMP.
           10 NI-G42DNRGWINKLBBA    PIC S9(4) COMP.
           10 NI-G42DNRGWBBAVH      PIC S9(4) COMP.
           10 NI-G42DNRGWOHNEBBA    PIC S9(4) COMP.
           10 NI-G42DNRGWOBBAVH     PIC S9(4) COMP.
           10 NI-G42DRGWUDEPOT      PIC S9(4) COMP.
           10 NI-G42DONLINEKZ       PIC S9(4) COMP.
           10 NI-G42DWPHGAKTUDAT    PIC S9(4) COMP.
        05 DCL-G42_DEPOTSTAMM.
           10 G42DVERTRAGSNR       PIC X(15).
           10 G42DOEFFNUNGSDAT     PIC X(8).
           10 G42DSTATUS           PIC X(1).
           10 G42DRISIKOKLASSE     PIC X(1).
           10 G42DANLAGEZIEL       PIC X(1).
           10 G42DDEPOTART         PIC X(1).
           10 G42DIBAN             PIC X(34).
           10 G42DBIC              PIC X(11).
           10 G42DMANDREF          PIC X(35).
           10 G42DKONTOINHABER     PIC X(30).
           10 G42DNRGWINKLBBA      PIC S9(11)V9(2) USAGE COMP-3.
           10 G42DNRGWBBAVH        PIC S9(6)V9(2) USAGE COMP-3.
           10 G42DNRGWOHNEBBA      PIC S9(11)V9(2) USAGE COMP-3.
           10 G42DNRGWOBBAVH       PIC S9(6)V9(2) USAGE COMP-3.
           10 G42DRGWUDEPOT        PIC S9(9)V9(2) USAGE COMP-3.
           10 G42DONLINEKZ         PIC X(1).
           10 G42DWPHGAKTUDAT      PIC X(8).
      ************************** ENDE-DCLGEN *************************

