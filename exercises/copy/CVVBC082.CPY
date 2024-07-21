      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBC082.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBC082.CPv  $
      *
      *   Rev 2.1   Mar 30 2015 15:50:58   t13ae36
      *AAS01119: Änderung Sonderleistungen 2015/ Elite-Club:PMAS014
      *1 unter neuem Namen
      *
      *   Rev 2.0   Feb 27 2015 13:45:54   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
 
CCC005*    EXEC SQL DECLARE VIVBC082 TABLE
CCC004*    (
CCC004*       MA_ST_NR                       INT
CCC004*                                      NOT NULL,
CCC004*       LIST_MONAT                     DATE
CCC004*                                      NOT NULL,
CCC004*       KTY_WB_TYP                     SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KNR_WB_TYP                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       KTY_WERTUNGS_TYP               SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KNR_WERTUNGS_TYP               SMALLINT
CCC004*                                      NOT NULL,
CCC004*       LIST_STUF_BEZ                  CHAR (10)
CCC004*                                      NOT NULL,
CCC004*       LIST_LAST_UPDATE               TIMESTAMP
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       DIR_NR                         SMALLINT
CCC004*                                      NOT NULL,
CCC004*       LIST_NAME                      CHAR (25)
CCC004*                                      NOT NULL,
CCC004*       LIST_VORNAME                   CHAR (25)
CCC004*                                      NOT NULL,
CCC004*       LIST_RANG                      INT
CCC004*                                      NOT NULL,
CCC004*       LIST_GEWINNER_KZ               CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       LIST_EH_1                      DECIMAL (10, 1),
CCC004*       LIST_EH_2                      DECIMAL (10, 1),
CCC004*       LIST_EH_3                      DECIMAL (10, 1),
CCC004*       LIST_EH_4                      DECIMAL (10, 1),
CCC004*       LIST_EH_5                      DECIMAL (10, 1),
CCC004*       LIST_SUM_EH                    DECIMAL (10, 1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KTY_REGION                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       KNR_REGION                     SMALLINT
CCC004*                                      NOT NULL,
CCC004*       LIST_BETRAG_1                  DECIMAL (11, 2),
CCC004*       LIST_BETRAG_2                  DECIMAL (11, 2),
CCC004*       LIST_BETRAG_3                  DECIMAL (11, 2),
CCC004*       LIST_BETRAG_4                  DECIMAL (11, 2),
CCC004*       LIST_BETRAG_5                  DECIMAL (11, 2),
CCC004*       LIST_SUM_BETRAG                DECIMAL (11, 2),
CCC004*       LIST_PROZSATZ                  DECIMAL (7, 2),
CCC004*       LIST_BEMERKUNG                 CHAR (20),
CCC004*       KTY_LIST_TYP                   SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KNR_LIST_TYP                   SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KTY_GESELL_TYP                 SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KNR_GESELL_TYP                 SMALLINT
CCC004*                                      NOT NULL,
CCC004*       LIST_WERTUNGSSTUFE             SMALLINT,
CCC004*       LIST_REISEZIEL                 CHAR (64)
CCC004*    )
CCC003*    END-EXEC.
 
 
       01  VIVBC082-GESAMT.
        05  NI-VIVBC082-TAB.
           10 NI-VIVBC082-ELE      PIC S9(04) BINARY OCCURS 35 TIMES.
        05  NI-VIVBC082            REDEFINES   NI-VIVBC082-TAB.
           10 NI-MA-ST-NR                PIC S9(04)  BINARY.
           10 NI-LIST-MONAT              PIC S9(04)  BINARY.
           10 NI-KTY-WB-TYP              PIC S9(04)  BINARY.
           10 NI-KNR-WB-TYP              PIC S9(04)  BINARY.
           10 NI-KTY-WERTUNGS-TYP        PIC S9(04)  BINARY.
           10 NI-KNR-WERTUNGS-TYP        PIC S9(04)  BINARY.
           10 NI-LIST-STUF-BEZ           PIC S9(04)  BINARY.
           10 NI-LIST-LAST-UPDATE        PIC S9(04)  BINARY.
           10 NI-DIR-NR                  PIC S9(04)  BINARY.
           10 NI-LIST-NAME               PIC S9(04)  BINARY.
           10 NI-LIST-VORNAME            PIC S9(04)  BINARY.
           10 NI-LIST-RANG               PIC S9(04)  BINARY.
           10 NI-LIST-GEWINNER-KZ        PIC S9(04)  BINARY.
           10 NI-LIST-EH-1               PIC S9(04)  BINARY.
           10 NI-LIST-EH-2               PIC S9(04)  BINARY.
           10 NI-LIST-EH-3               PIC S9(04)  BINARY.
           10 NI-LIST-EH-4               PIC S9(04)  BINARY.
           10 NI-LIST-EH-5               PIC S9(04)  BINARY.
           10 NI-LIST-SUM-EH             PIC S9(04)  BINARY.
           10 NI-KTY-REGION              PIC S9(04)  BINARY.
           10 NI-KNR-REGION              PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-1           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-2           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-3           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-4           PIC S9(04)  BINARY.
           10 NI-LIST-BETRAG-5           PIC S9(04)  BINARY.
           10 NI-LIST-SUM-BETRAG         PIC S9(04)  BINARY.
           10 NI-LIST-PROZSATZ           PIC S9(04)  BINARY.
           10 NI-LIST-BEMERKUNG          PIC S9(04)  BINARY.
           10 NI-KTY-LIST-TYP            PIC S9(04)  BINARY.
           10 NI-KNR-LIST-TYP            PIC S9(04)  BINARY.
           10 NI-KTY-GESELL-TYP          PIC S9(04)  BINARY.
           10 NI-KNR-GESELL-TYP          PIC S9(04)  BINARY.
           10 NI-LIST-WERTUNGSSTUFE      PIC S9(04)  BINARY.
           10 NI-LIST-REISEZIEL          PIC S9(04)  BINARY.
        05 DCL-VIVBC082.
           10 MA-ST-NR                   PIC S9(9) COMP.
           10 LIST-MONAT                 PIC X(10).
           10 KTY-WB-TYP                 PIC S9(4) COMP.
           10 KNR-WB-TYP                 PIC S9(4) COMP.
           10 KTY-WERTUNGS-TYP           PIC S9(4) COMP.
           10 KNR-WERTUNGS-TYP           PIC S9(4) COMP.
           10 LIST-STUF-BEZ              PIC X(10).
           10 LIST-LAST-UPDATE           PIC X(26).
           10 DIR-NR                     PIC S9(4) COMP.
           10 LIST-NAME                  PIC X(25).
           10 LIST-VORNAME               PIC X(20).
           10 LIST-RANG                  PIC S9(9) COMP.
           10 LIST-GEWINNER-KZ           PIC X.
           10 LIST-EH-1                  PIC S9(9)V9(1) COMP-3.
           10 LIST-EH-2                  PIC S9(9)V9(1) COMP-3.
           10 LIST-EH-3                  PIC S9(9)V9(1) COMP-3.
           10 LIST-EH-4                  PIC S9(9)V9(1) COMP-3.
           10 LIST-EH-5                  PIC S9(9)V9(1) COMP-3.
           10 LIST-SUM-EH                PIC S9(9)V9(1) COMP-3.
           10 KTY-REGION                 PIC S9(4) COMP.
           10 KNR-REGION                 PIC S9(4) COMP.
           10 LIST-BETRAG-1              PIC S9(9)V9(2) COMP-3.
           10 LIST-BETRAG-2              PIC S9(9)V9(2) COMP-3.
           10 LIST-BETRAG-3              PIC S9(9)V9(2) COMP-3.
           10 LIST-BETRAG-4              PIC S9(9)V9(2) COMP-3.
           10 LIST-BETRAG-5              PIC S9(9)V9(2) COMP-3.
           10 LIST-SUM-BETRAG            PIC S9(9)V9(2) COMP-3.
           10 LIST-PROZSATZ              PIC S9(5)V9(2) COMP-3.
           10 LIST-BEMERKUNG             PIC X(20).
           10 KTY-LIST-TYP               PIC S9(4) COMP.
           10 KNR-LIST-TYP               PIC S9(4) COMP.
           10 KTY-GESELL-TYP             PIC S9(4) COMP.
           10 KNR-GESELL-TYP             PIC S9(4) COMP.
           10 LIST-WERTUNGSSTUFE         PIC S9(4) COMP.
           10 LIST-REISEZIEL             PIC X(64).

