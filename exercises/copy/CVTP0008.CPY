      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVTP0008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVTP0008.CPv  $
      *
      *   Rev 2.1   Jul 13 2005 14:29:30   T13AE27
      *AAS00563: Jahres-WBW 2005 DVAG-Wien
      *
      *   Rev 2.0   Jul 12 2005 08:50:16   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBTP0008)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVTP0008))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBTP0008 TABLE
CCC004*    (
CCC004*            ST_NR                INTEGER         NOT NULL,
CCC004*            MONAT                DATE            NOT NULL,
CCC004*            GES_NR               SMALLINT        NOT NULL,
CCC004*            PRODUKT_NR           SMALLINT        NOT NULL,
CCC004*            EH_GS_ZU_EG          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_GS_ZU_GR          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_GS_AB_EG          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_GS_AB_GR          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_FG_ZU_EG          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_FG_ZU_GR          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_FG_AB_EG          DECIMAL(10, 1)  NOT NULL,
CCC004*            EH_FG_AB_GR          DECIMAL(10, 1)  NOT NULL
CCC004*    )
CCC003*    END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBTP0008                           *
      ******************************************************************
       01  TBTP0001-GESAMT.
        05  NI-TBTP0008.
            10 NI-ST-NR                     PIC S9(04)   COMP.
            10 NI-MONAT                     PIC S9(04)   COMP.
            10 NI-GES-NR                    PIC S9(04)   COMP.
            10 NI-PRODUKT-NR                PIC S9(04)   COMP.
            10 NI-EH-GS-ZU-EG               PIC S9(04)   COMP.
            10 NI-EH-GS-ZU-GR               PIC S9(04)   COMP.
            10 NI-EH-GS-AB-EG               PIC S9(04)   COMP.
            10 NI-EH-GS-AB-GR               PIC S9(04)   COMP.
            10 NI-EH-FG-ZU-EG               PIC S9(04)   COMP.
            10 NI-EH-FG-ZU-GR               PIC S9(04)   COMP.
            10 NI-EH-FG-AB-EG               PIC S9(04)   COMP.
            10 NI-EH-FG-AB-GR               PIC S9(04)   COMP.

        05  DCL-TBTP0008.
            10 ST-NR                        PIC S9(09)   COMP.
            10 MONAT                        PIC X(010).
            10 GES-NR                       PIC S9(04)   COMP.
            10 PRODUKT-NR                   PIC S9(04)   COMP.
            10 EH-GS-ZU-EG                  PIC S9(09)V9 COMP-3.
            10 EH-GS-ZU-GR                  PIC S9(09)V9 COMP-3.
            10 EH-GS-AB-EG                  PIC S9(09)V9 COMP-3.
            10 EH-GS-AB-GR                  PIC S9(09)V9 COMP-3.
            10 EH-FG-ZU-EG                  PIC S9(09)V9 COMP-3.
            10 EH-FG-ZU-GR                  PIC S9(09)V9 COMP-3.
            10 EH-FG-AB-EG                  PIC S9(09)V9 COMP-3.
            10 EH-FG-AB-GR                  PIC S9(09)V9 COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
