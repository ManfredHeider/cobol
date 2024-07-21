      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0300.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0300.CPv  $
      *
      *   Rev 2.1   Mar 25 2009 09:31:44   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Mar 24 2009 14:45:20   t13ex43
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
      * DCLGEN TABLE(VIVB0300) CPROF                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0300)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0300 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,
CCC004*      CPROF_MONAT                 DATE NOT NULL,
CCC004*      KTY_DISZIPLIN               SMALLINT NOT NULL,
CCC004*      KNR_DISZIPLIN               SMALLINT NOT NULL,
CCC004*      CPROF_STERN                 SMALLINT NOT NULL,
CCC004*      CPROF_ART                   CHAR(01) NOT NULL,
CCC004*      CPROF_LAST_UPDATE           TIMESTAMP NOT NULL,
CCC004*      CPROF_U_EH_GES              DECIMAL(10, 1),
CCC004*      CPROF_U_EH_EGFR             DECIMAL(10, 1),
CCC004*      CPROF_U_EH_WERTG            DECIMAL(10, 1),
CCC004*      CPROF_EQF_GES               DECIMAL(05, 2),
CCC004*      CPROF_EQF_EGFR              DECIMAL(05, 2),
CCC004*      CPROF_EQF_WERTG             DECIMAL(05, 2),
CCC004*      CPROF_EGFR_AB               DATE,
CCC004*      CPROF_EGFR_BIS              DATE,
CCC004*      CPROF_W_EH1                 DECIMAL(10, 1),
CCC004*      CPROF_W_EH2                 DECIMAL(10, 1),
CCC004*      CPROF_W_EH3                 DECIMAL(10, 1),
CCC004*      CPROF_W_EH4                 DECIMAL(10, 1),
CCC004*      CPROF_W_STEIG1              DECIMAL(05, 2),
CCC004*      CPROF_W_STEIG2              DECIMAL(05, 2),
CCC004*      CPROF_W_STEIG3              DECIMAL(05, 2),
CCC004*      CPROF_W_STEIGGES            DECIMAL(05, 2),
CCC004*      CPROF_ANZ_ASTRUK            SMALLINT,
CCC004*      CPROF_EH_NP                 DECIMAL(10, 1),
CCC004*      CPROF_EH_GES                DECIMAL(10, 1),
CCC004*      CPROF_ANTEIL_NP             DECIMAL(05, 2),
CCC004*      CPROF_ED_ANZ                SMALLINT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0300
      ******************************************************************
       01  VIVB0300-GESAMT.
        05  NI-VIVB0300-TAB.
           10 NI-VIVB0300-ELE      PIC S9(04) BINARY OCCURS 28 TIMES.
        05  NI-VIVB0300            REDEFINES   NI-VIVB0300-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-CPROF-MONAT              PIC S9(04)       BINARY.
           10 NI-KTY-DISZIPLIN            PIC S9(04)       BINARY.
           10 NI-KNR-DISZIPLIN            PIC S9(04)       BINARY.
           10 NI-CPROF-STERN              PIC S9(04)       BINARY.
           10 NI-CPROF-ART                PIC S9(04)       BINARY.
           10 NI-CPROF-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-CPROF-U-EH-GES           PIC S9(04)       BINARY.
           10 NI-CPROF-U-EH-EGFR          PIC S9(04)       BINARY.
           10 NI-CPROF-U-EH-WERTG         PIC S9(04)       BINARY.
           10 NI-CPROF-EQF-GES            PIC S9(04)       BINARY.
           10 NI-CPROF-EQF-EGFR           PIC S9(04)       BINARY.
           10 NI-CPROF-EQF-WERTG          PIC S9(04)       BINARY.
           10 NI-CPROF-EGFR-AB            PIC S9(04)       BINARY.
           10 NI-CPROF-EGFR-BIS           PIC S9(04)       BINARY.
           10 NI-CPROF-W-EH1              PIC S9(04)       BINARY.
           10 NI-CPROF-W-EH2              PIC S9(04)       BINARY.
           10 NI-CPROF-W-EH3              PIC S9(04)       BINARY.
           10 NI-CPROF-W-EH4              PIC S9(04)       BINARY.
           10 NI-CPROF-W-STEIG1           PIC S9(04)       BINARY.
           10 NI-CPROF-W-STEIG2           PIC S9(04)       BINARY.
           10 NI-CPROF-W-STEIG3           PIC S9(04)       BINARY.
           10 NI-CPROF-W-STEIGGES         PIC S9(04)       BINARY.
           10 NI-CPROF-ANZ-ASTRUK         PIC S9(04)       BINARY.
           10 NI-CPROF-EH-NP              PIC S9(04)       BINARY.
           10 NI-CPROF-EH-GES             PIC S9(04)       BINARY.
           10 NI-CPROF-ANTEIL-NP          PIC S9(04)       BINARY.
           10 NI-CPROF-ED-ANZ             PIC S9(04)       BINARY.

        05  DCL-VIVB0300.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 CPROF-MONAT                 PIC  X(10).
           10 KTY-DISZIPLIN               PIC S9(04)       BINARY.
           10 KNR-DISZIPLIN               PIC S9(04)       BINARY.
           10 CPROF-STERN                 PIC S9(04)       BINARY.
           10 CPROF-ART                   PIC  X(01).
           10 CPROF-LAST-UPDATE           PIC  X(26).
           10 CPROF-U-EH-GES              PIC S9(09)V9(01) COMP-3.
           10 CPROF-U-EH-EGFR             PIC S9(09)V9(01) COMP-3.
           10 CPROF-U-EH-WERTG            PIC S9(09)V9(01) COMP-3.
           10 CPROF-EQF-GES               PIC S9(03)V9(02) COMP-3.
           10 CPROF-EQF-EGFR              PIC S9(03)V9(02) COMP-3.
           10 CPROF-EQF-WERTG             PIC S9(03)V9(02) COMP-3.
           10 CPROF-EGFR-AB               PIC  X(10).
           10 CPROF-EGFR-BIS              PIC  X(10).
           10 CPROF-W-EH1                 PIC S9(09)V9(01) COMP-3.
           10 CPROF-W-EH2                 PIC S9(09)V9(01) COMP-3.
           10 CPROF-W-EH3                 PIC S9(09)V9(01) COMP-3.
           10 CPROF-W-EH4                 PIC S9(09)V9(01) COMP-3.
           10 CPROF-W-STEIG1              PIC S9(03)V9(02) COMP-3.
           10 CPROF-W-STEIG2              PIC S9(03)V9(02) COMP-3.
           10 CPROF-W-STEIG3              PIC S9(03)V9(02) COMP-3.
           10 CPROF-W-STEIGGES            PIC S9(03)V9(02) COMP-3.
           10 CPROF-ANZ-ASTRUK            PIC S9(04)       BINARY.
           10 CPROF-EH-NP                 PIC S9(09)V9(01) COMP-3.
           10 CPROF-EH-GES                PIC S9(09)V9(01) COMP-3.
           10 CPROF-ANTEIL-NP             PIC S9(03)V9(02) COMP-3.
           10 CPROF-ED-ANZ                PIC S9(04)       BINARY.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 28      *
      ******************************************************************
