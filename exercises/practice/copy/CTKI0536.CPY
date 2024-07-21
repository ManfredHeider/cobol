      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22/10/19
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0536.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0536.CPv  $
      *
      *   Rev 2.5   Apr 08 2020 14:41:50   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.4   Mar 17 2020 09:53:04   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.3   Feb 19 2020 10:54:06   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.2   Feb 19 2020 10:15:54   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.1   Feb 18 2020 15:26:46   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.0   Oct 22 2019 09:58:08   I13AZ84
      *Archiverstellung
      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AKI01726!19.02.20! NEUE FSA TT               !MERT ATAR *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_FSA TABLE
CCC004*    ( G48FSAVNR                      CHAR(15) NOT NULL,
CCC004*      G48FSAVBNR                     INTEGER NOT NULL,
CCC004*      G48FSAGUELTIGAB                CHAR(8) NOT NULL,
CCC004*      G48FSAGUELTIGBIS               CHAR(8) NOT NULL,
CCC004*      G48FSAGEMELDET                 DECIMAL(7, 2) NOT NULL,
CCC004*      G48FSAVERBRAUCHT               DECIMAL(7, 2) NOT NULL,
CCC004*      G48FSASTUECKZINS               DECIMAL(12, 2) NOT NULL,
CCC004*      G48FSADEPOTAKTIV               CHAR(1) NOT NULL,
CCC004*      G48FSADELKZ                    CHAR(1) NOT NULL,
CCC004*      G48FSASTEUERAUSL               CHAR(1) NOT NULL,
CCC004*      G48FSANVBBIS                   CHAR(8) NOT NULL,
CCC004*      G48FSAAUSFREIVJ                DECIMAL(7, 2) NOT NULL,
CCC004*      G48FSAAUFGERT                  DECIMAL(9, 2) NOT NULL,
CCC004*      G48FSAEINBKEST                 DECIMAL(9, 2) NOT NULL,
CCC004*      G48FSAEINBSOLI                 DECIMAL(7, 2) NOT NULL,
CCC004*      G48FSAEINBKIST                 DECIMAL(7, 2) NOT NULL,
CCC004*      G48FSAVERLAKTIE                DECIMAL(9, 2) NOT NULL,
CCC004*      G48FSAVERLSONST                DECIMAL(9, 2) NOT NULL,
CCC004*      G48FSAANRQUELL                 DECIMAL(9, 2) NOT NULL,
CCC004*      G48FSAVERBAKTIE                CHAR(1) NOT NULL,
CCC004*      G48FSAVERBSONST                CHAR(1) NOT NULL,
CCC004*      G48FSAZUGKONF1                 CHAR(2) NOT NULL,
CCC004*      G48FSAZUGKONF2                 CHAR(2) NOT NULL,
CCC004*      G48FSAKIST1                    DECIMAL(4, 2) NOT NULL,
CCC004*      G48FSAKIST2                    DECIMAL(4, 2) NOT NULL,
CCC004*      G48FSAKISTAB                   CHAR(8) NOT NULL,
CCC004*      G48FSAAUFMAS1                  INTEGER NOT NULL,
CCC004*      G48FSAAUFMAS2                  INTEGER NOT NULL,
CCC004*      G48FSANR                       CHAR(30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_FSA                     *
      ******************************************************************
       01  G48-FSA-GESAMT.
        05 NI-G48-FSA-TAB.
           10 NI-G48-FSA-ELE PIC S9(04) COMP OCCURS 29 TIMES.
        05 NI-G48-FSA REDEFINES NI-G48-FSA-TAB.
           10 NI-G48FSAVNR          PIC S9(4) COMP.
           10 NI-G48FSAVBNR         PIC S9(4) COMP.
           10 NI-G48FSAGUELTIGAB    PIC S9(4) COMP.
           10 NI-G48FSAGUELTIGBIS   PIC S9(4) COMP.
           10 NI-G48FSAGEMELDET     PIC S9(4) COMP.
           10 NI-G48FSAVERBRAUCHT   PIC S9(4) COMP.
           10 NI-G48FSASTUECKZINS   PIC S9(4) COMP.
           10 NI-G48FSADEPOTAKTIV   PIC S9(4) COMP.
           10 NI-G48FSADELKZ        PIC S9(4) COMP.
           10 NI-G48FSASTEUERAUSL   PIC S9(4) COMP.
           10 NI-G48FSANVBBIS       PIC S9(4) COMP.
           10 NI-G48FSAAUSFREIVJ    PIC S9(4) COMP.
           10 NI-G48FSAAUFGERT      PIC S9(4) COMP.
           10 NI-G48FSAEINBKEST     PIC S9(4) COMP.
           10 NI-G48FSAEINBSOLI     PIC S9(4) COMP.
           10 NI-G48FSAEINBKIST     PIC S9(4) COMP.
           10 NI-G48FSAVERLAKTIE    PIC S9(4) COMP.
           10 NI-G48FSAVERLSONST    PIC S9(4) COMP.
           10 NI-G48FSAANRQUELL     PIC S9(4) COMP.
           10 NI-G48FSAVERBAKTIE    PIC S9(4) COMP.
           10 NI-G48FSAVERBSONST    PIC S9(4) COMP.
           10 NI-G48FSAZUGKONF1     PIC S9(4) COMP.
           10 NI-G48FSAZUGKONF2     PIC S9(4) COMP.
           10 NI-G48FSAKIST1        PIC S9(4) COMP.
           10 NI-G48FSAKIST2        PIC S9(4) COMP.
           10 NI-G48FSAKISTAB       PIC S9(4) COMP.
           10 NI-G48FSAAUFMAS1      PIC S9(4) COMP.
           10 NI-G48FSAAUFMAS2      PIC S9(4) COMP.
           10 NI-G48FSANR           PIC S9(4) COMP.
        05 DCL-G48_FSA.
           10 G48FSAVNR            PIC X(15).
           10 G48FSAVBNR           PIC S9(9) USAGE COMP.
           10 G48FSAGUELTIGAB      PIC X(8).
           10 G48FSAGUELTIGBIS     PIC X(8).
           10 G48FSAGEMELDET       PIC S9(5)V9(2) USAGE COMP-3.
           10 G48FSAVERBRAUCHT     PIC S9(5)V9(2) USAGE COMP-3.
           10 G48FSASTUECKZINS     PIC S9(10)V9(2) USAGE COMP-3.
           10 G48FSADEPOTAKTIV     PIC X(1).
           10 G48FSADELKZ          PIC X(1).
           10 G48FSASTEUERAUSL     PIC X(1).
           10 G48FSANVBBIS         PIC X(8).
           10 G48FSAAUSFREIVJ      PIC S9(5)V9(2) USAGE COMP-3.
           10 G48FSAAUFGERT        PIC S9(7)V9(2) USAGE COMP-3.
           10 G48FSAEINBKEST       PIC S9(7)V9(2) USAGE COMP-3.
           10 G48FSAEINBSOLI       PIC S9(5)V9(2) USAGE COMP-3.
           10 G48FSAEINBKIST       PIC S9(5)V9(2) USAGE COMP-3.
           10 G48FSAVERLAKTIE      PIC S9(7)V9(2) USAGE COMP-3.
           10 G48FSAVERLSONST      PIC S9(7)V9(2) USAGE COMP-3.
           10 G48FSAANRQUELL       PIC S9(7)V9(2) USAGE COMP-3.
           10 G48FSAVERBAKTIE      PIC X(1).
           10 G48FSAVERBSONST      PIC X(1).
           10 G48FSAZUGKONF1       PIC X(2).
           10 G48FSAZUGKONF2       PIC X(2).
           10 G48FSAKIST1          PIC S9(2)V9(2) USAGE COMP-3.
           10 G48FSAKIST2          PIC S9(2)V9(2) USAGE COMP-3.
           10 G48FSAKISTAB         PIC X(8).
           10 G48FSAAUFMAS1        PIC S9(9) USAGE COMP.
           10 G48FSAAUFMAS2        PIC S9(9) USAGE COMP.
           10 G48FSANR             PIC X(30).
      ************************** ENDE-DCLGEN *************************




