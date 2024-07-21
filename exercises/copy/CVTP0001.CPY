      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  26.02.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVTP0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVTP0001.CPv  $
      *
      *   Rev 2.2   Nov 04 2005 11:56:28   T13AE27
      *AAS00572: Siegeradler 2005
      *
      *   Rev 2.1   Oct 29 2003 15:41:44   t13ae27
      *neu
      *
      *   Rev 2.0   Feb 26 2003 12:10:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBTP0001)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVTP0001))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBTP0001 TABLE
CCC004*    (
CCC004*            TMP_POS_VOR_ABZPRZ        SMALLINT        NOT NULL,
CCC004*            TMP_VBNR                  INTEGER         NOT NULL,
CCC004*            TMP_E_VOR_ABZPRZ          DECIMAL(10, 1)  NOT NULL,
CCC004*            TMP_STUFE_AKT             SMALLINT        NOT NULL,
CCC004*            TMP_MAN_VORNAME           CHAR(20)        NOT NULL,
CCC004*            TMP_MAN_NAME              CHAR(40)        NOT NULL,
CCC004*            TMP_MAN_TITEL_ADEL        CHAR(10)        NOT NULL,
CCC004*            TMP_MAN_TITEL_AKAD        CHAR(10)        NOT NULL,
CCC004*            TMP_DIR_NR                SMALLINT        NOT NULL,
CCC004*            TMP_EQF_12_MON            DECIMAL(05, 2)  NOT NULL,
CCC004*            TMP_E_NACH_ABZPRZ         DECIMAL(10, 1)  NOT NULL,
CCC004*            TMP_STUFE_WERTUNG         SMALLINT        NOT NULL,
CCC004*            TMP_POS_WERTUNG_V         SMALLINT        NOT NULL,
CCC004*            TMP_POS_WERTUNG_N         SMALLINT        NOT NULL,
CCC004*            TMP_PROZ                  DECIMAL(05, 2)  NOT NULL,
CCC004*            TMP_EBB_KZ                CHAR(01)        NOT NULL
CCC004*    )
CCC003*    END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBTP0001                           *
      ******************************************************************
       01  TBTP0001-GESAMT.
        05  NI-TBTP0001.
            10 NI-TMP-POS-VOR-ABZPRZ         PIC S9(04)   COMP.
            10 NI-TMP-VBNR                   PIC S9(04)   COMP.
            10 NI-TMP-E-VOR-ABZPRZ           PIC S9(04)   COMP.
            10 NI-TMP-STUFE-AKT              PIC S9(04)   COMP.
            10 NI-TMP-MAN-TITEL-ADEL         PIC S9(04)   COMP.
            10 NI-TMP-MAN-TITEL-AKAD         PIC S9(04)   COMP.
            10 NI-TMP-MAN-NAME               PIC S9(04)   COMP.
            10 NI-TMP-MAN-VORNAME            PIC S9(04)   COMP.
            10 NI-TMP-DIR-NR                 PIC S9(04)   COMP.
            10 NI-TMP-EQF-12-MON             PIC S9(04)   COMP.
            10 NI-TMP-E-NACH-ABZPRZ          PIC S9(04)   COMP.
            10 NI-TMP-STUFE-WERTUNG          PIC S9(04)   COMP.
            10 NI-TMP-POS-WERTUNG-V          PIC S9(04)   COMP.
            10 NI-TMP-POS-WERTUNG-N          PIC S9(04)   COMP.
            10 NI-TMP-PROZ                   PIC S9(04)   COMP.
            10 NI-TMP-EBB-KZ                 PIC S9(04)   COMP.

        05  DCL-TBTP0001.
            10 TMP-POS-VOR-ABZPRZ            PIC S9(04)   COMP.
            10 TMP-VBNR                      PIC S9(09)   COMP.
            10 TMP-E-VOR-ABZPRZ              PIC S9(09)V9 COMP-3.
            10 TMP-STUFE-AKT                 PIC S9(04)   COMP.
            10 TMP-MAN-VORNAME               PIC  X(20).
            10 TMP-MAN-NAME                  PIC  X(40).
            10 TMP-MAN-TITEL-ADEL            PIC  X(10).
            10 TMP-MAN-TITEL-AKAD            PIC  X(10).
            10 TMP-DIR-NR                    PIC S9(04)   COMP.
            10 TMP-EQF-12-MON                PIC S9(03)V99 COMP-3.
            10 TMP-E-NACH-ABZPRZ             PIC S9(09)V9 COMP-3.
            10 TMP-STUFE-WERTUNG             PIC S9(04)   COMP.
            10 TMP-POS-WERTUNG-V             PIC S9(04)   COMP.
            10 TMP-POS-WERTUNG-N             PIC S9(04)   COMP.
            10 TMP-PROZ                      PIC S9(03)V99 COMP-3.
            10 TMP-EBB-KZ                    PIC  X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
