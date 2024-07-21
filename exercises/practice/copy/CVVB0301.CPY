      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0301.CPv  $
      *
      *   Rev 2.1   Mar 25 2009 09:32:24   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Mar 24 2009 14:45:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    24.03.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!24.03.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0301) DETAILAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0301)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0301 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      DETAILAS_MONAT              DATE NOT NULL,
CCC004*      DETAILAS_UNTST              INTEGER NOT NULL,
CCC004*      KNR_WB_TYP                  SMALLINT NOT NULL,
CCC004*      KNR_WERTUNGS_TYP            SMALLINT NOT NULL,
CCC004*      KTY_WB_TYP                  SMALLINT NOT NULL,
CCC004*      KTY_WERTUNGS_TYP            SMALLINT NOT NULL,
CCC004*      DETAILAS_LAST_UP            TIMESTAMP NOT NULL,
CCC004*      DETAILAS_MS_NR              SMALLINT NOT NULL,
CCC004*      UNTST_MS_NR                 SMALLINT NOT NULL,
CCC004*      DETAILAS_EH_IST             DECIMAL(10, 1) NOT NULL,
CCC004*      DETAILAS_EH_SOLL            DECIMAL(10, 1) NOT NULL,
CCC004*      DETAILAS_ERF                CHAR(01) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0301
      ******************************************************************
       01  VIVB0301-GESAMT.
        05  NI-VIVB0301-TAB.
           10 NI-VIVB0301-ELE      PIC S9(04) BINARY OCCURS 13 TIMES.
        05  NI-VIVB0301            REDEFINES   NI-VIVB0301-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-DETAILAS-MONAT           PIC S9(04)       BINARY.
           10 NI-DETAILAS-UNTST           PIC S9(04)       BINARY.
           10 NI-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 NI-KNR-WERTUNGS-TYP         PIC S9(04)       BINARY.
           10 NI-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 NI-KTY-WERTUNGS-TYP         PIC S9(04)       BINARY.
           10 NI-DETAILAS-LAST-UP         PIC S9(04)       BINARY.
           10 NI-DETAILAS-MS-NR           PIC S9(04)       BINARY.
           10 NI-UNTST-MS-NR              PIC S9(04)       BINARY.
           10 NI-DETAILAS-EH-IST          PIC S9(04)       BINARY.
           10 NI-DETAILAS-EH-SOLL         PIC S9(04)       BINARY.
           10 NI-DETAILAS-ERF             PIC S9(04)       BINARY.

        05  DCL-VIVB0301.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 DETAILAS-MONAT              PIC  X(10).
           10 DETAILAS-UNTST              PIC S9(09)       BINARY.
           10 KNR-WB-TYP                  PIC S9(04)       BINARY.
           10 KNR-WERTUNGS-TYP            PIC S9(04)       BINARY.
           10 KTY-WB-TYP                  PIC S9(04)       BINARY.
           10 KTY-WERTUNGS-TYP            PIC S9(04)       BINARY.
           10 DETAILAS-LAST-UP            PIC  X(26).
           10 DETAILAS-MS-NR              PIC S9(04)       BINARY.
           10 UNTST-MS-NR                 PIC S9(04)       BINARY.
           10 DETAILAS-EH-IST             PIC S9(09)V9(01) COMP-3.
           10 DETAILAS-EH-SOLL            PIC S9(09)V9(01) COMP-3.
           10 DETAILAS-ERF                PIC  X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
