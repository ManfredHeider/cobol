      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS VM
      *            Datum      :  26.02.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVTP0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CVTP0002.CPv  $
      *
      *   Rev 2.1   Oct 29 2003 15:41:56   t13ae27
      *neu
      *
      *   Rev 2.0   Feb 26 2003 12:10:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBTP0002)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVTP0002))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBTP0002 TABLE
CCC004*    (
CCC004*            TMP_KZ                    CHAR(03)        NOT NULL,
CCC004*            TMP_VBNR                  INTEGER         NOT NULL,
CCC004*            TMP_EQF_12_MON            DECIMAL(05, 2),
CCC004*            TMP_BES                   SMALLINT,
CCC004*            TMP_UNTST_NR              INTEGER,
CCC004*            TMP_UNTST_DAT_VON         DATE,
CCC004*            TMP_UNTST_DAT_BIS         DATE,
CCC004*            TMP_E_BASIS_ABZPRZ        DECIMAL(10, 1),
CCC004*            TMP_E_BETR_BASIS          DECIMAL(10, 1),
CCC004*            TMP_E_UNTST               DECIMAL(10, 1),
CCC004*            TMP_DATUM                 DATE
CCC004*    )
CCC003*    END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0085                           *
      ******************************************************************
       01  TBTP0002-GESAMT.
        05  NI-TBTP0002.
            10 NI-TMP-KZ                     PIC S9(04)   COMP.
            10 NI-TMP-VBNR                   PIC S9(04)   COMP.
            10 NI-TMP-EQF-12-MON             PIC S9(04)   COMP.
            10 NI-TMP-BES                    PIC S9(04)   COMP.
            10 NI-TMP-UNTST-NR               PIC S9(04)   COMP.
            10 NI-TMP-UNTST-DAT-VON          PIC S9(04)   COMP.
            10 NI-TMP-UNTST-DAT-BIS          PIC S9(04)   COMP.
            10 NI-TMP-E-BASIS-ABZPRZ         PIC S9(04)   COMP.
            10 NI-TMP-E-BETR-BASIS           PIC S9(04)   COMP.
            10 NI-TMP-E-UNTST                PIC S9(04)   COMP.
            10 NI-TMP-DATUM                  PIC S9(04)   COMP.

        05  DCL-TBTP0002.
            10 TMP-KZ                        PIC  X(03).
            10 TMP-VBNR                      PIC S9(09)   COMP.
            10 TMP-EQF-12-MON                PIC S9(03)V99 COMP-3.
            10 TMP-BES                       PIC S9(04)   COMP.
            10 TMP-UNTST-NR                  PIC S9(09)   COMP.
            10 TMP-UNTST-DAT-VON             PIC  X(10).
            10 TMP-UNTST-DAT-BIS             PIC  X(10).
            10 TMP-E-BASIS-ABZPRZ            PIC S9(09)V9 COMP-3.
            10 TMP-E-BETR-BASIS              PIC S9(09)V9 COMP-3.
            10 TMP-E-UNTST                   PIC S9(09)V9 COMP-3.
            10 TMP-DATUM                     PIC  X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
