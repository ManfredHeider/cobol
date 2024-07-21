      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17/08/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0546.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0546.CPv  $
      *
      *   Rev 1.1   Oct 16 2020 10:41:54   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 1.0   Oct 16 2020 08:23:50   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *
      *   Rev 2.0   Aug 17 2020 09:32:26   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_DEPOTSTAMM TABLE
CCC004*    ( G48DSVNR                       CHAR(15) NOT NULL,
CCC004*      G48DSUNTERNR                   CHAR(2) NOT NULL,
CCC004*      G48DSWAEHRUNGSKZ               CHAR(3) NOT NULL,
CCC004*      G48DSOFFNUNGSDATUM             CHAR(8) NOT NULL,
CCC004*      G48DSSCHLIESSDATUM             CHAR(8) NOT NULL,
CCC004*      G48DSRISIKOKLASSE              CHAR(1) NOT NULL,
CCC004*      G48DSANLAGEZIEL                CHAR(1) NOT NULL,
CCC004*      G48DSDEPOTART                  CHAR(1) NOT NULL,
CCC004*      G48DSDELKZ                     CHAR(1) NOT NULL,
CCC004*      G48DSIBAN                      CHAR(34) NOT NULL,
CCC004*      G48DSBIC                       CHAR(11) NOT NULL,
CCC004*      G48DSMANDREF                   CHAR(35) NOT NULL,
CCC004*      G48DSKONTOINH                  CHAR(28) NOT NULL,
CCC004*      G48DSNRGWINKLBBA               DECIMAL(13, 2) NOT NULL,
CCC004*      G48DSNRGWBARPRO                DECIMAL(8, 2) NOT NULL,
CCC004*      G48DSNRGWOHNEBBA               DECIMAL(13, 2) NOT NULL,
CCC004*      G48DSNRGWOHNPRO                DECIMAL(8, 2) NOT NULL,
CCC004*      G48DSRGWUDEPOT                 DECIMAL(11, 2) NOT NULL,
CCC004*      G48DSSONDERKOND                CHAR(1) NOT NULL,
CCC004*      G48DSWPHGDAT                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_DEPOTSTAMM              *
      ******************************************************************
       01  G48-DEPOTSTAMM-GESAMT.
        05 NI-G48-DEPOTSTAMM-TAB.
           10 NI-G48-DEPOTSTAMM-ELE PIC S9(04) COMP OCCURS 20 TIMES.
        05 NI-G48-DEPOTSTAMM REDEFINES NI-G48-DEPOTSTAMM-TAB.
           10 NI-G48DSVNR           PIC S9(4) COMP.
           10 NI-G48DSUNTERNR       PIC S9(4) COMP.
           10 NI-G48DSWAEHRUNGSKZ   PIC S9(4) COMP.
           10 NI-G48DSOFFNUNGSDATUM PIC S9(4) COMP.
           10 NI-G48DSSCHLIESSDATUM PIC S9(4) COMP.
           10 NI-G48DSRISIKOKLASSE  PIC S9(4) COMP.
           10 NI-G48DSANLAGEZIEL    PIC S9(4) COMP.
           10 NI-G48DSDEPOTART      PIC S9(4) COMP.
           10 NI-G48DSDELKZ         PIC S9(4) COMP.
           10 NI-G48DSIBAN          PIC S9(4) COMP.
           10 NI-G48DSBIC           PIC S9(4) COMP.
           10 NI-G48DSMANDREF       PIC S9(4) COMP.
           10 NI-G48DSKONTOINH      PIC S9(4) COMP.
           10 NI-G48DSNRGWINKLBBA   PIC S9(4) COMP.
           10 NI-G48DSNRGWBARPRO    PIC S9(4) COMP.
           10 NI-G48DSNRGWOHNEBBA   PIC S9(4) COMP.
           10 NI-G48DSNRGWOHNPRO    PIC S9(4) COMP.
           10 NI-G48DSRGWUDEPOT     PIC S9(4) COMP.
           10 NI-G48DSSONDERKOND    PIC S9(4) COMP.
           10 NI-G48DSWPHGDAT       PIC S9(4) COMP.
        05 DCL-G48-DEPOTSTAMM.
           10 G48DSVNR             PIC X(15).
           10 G48DSUNTERNR         PIC X(2).
           10 G48DSWAEHRUNGSKZ     PIC X(3).
           10 G48DSOFFNUNGSDATUM   PIC X(8).
           10 G48DSSCHLIESSDATUM   PIC X(8).
           10 G48DSRISIKOKLASSE    PIC X(1).
           10 G48DSANLAGEZIEL      PIC X(1).
           10 G48DSDEPOTART        PIC X(1).
           10 G48DSDELKZ           PIC X(1).
           10 G48DSIBAN            PIC X(34).
           10 G48DSBIC             PIC X(11).
           10 G48DSMANDREF         PIC X(35).
           10 G48DSKONTOINH        PIC X(28).
           10 G48DSNRGWINKLBBA     PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DSNRGWBARPRO      PIC S9(6)V9(2) USAGE COMP-3.
           10 G48DSNRGWOHNEBBA     PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DSNRGWOHNPRO      PIC S9(6)V9(2) USAGE COMP-3.
           10 G48DSRGWUDEPOT       PIC S9(9)V9(2) USAGE COMP-3.
           10 G48DSSONDERKOND      PIC X(1).
           10 G48DSWPHGDAT         PIC X(8).
      ************************** ENDE-DCLGEN *************************


