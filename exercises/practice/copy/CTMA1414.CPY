      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1414.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1414.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:16:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1414-T
CCC005*    EXEC SQL DECLARE TBOVPSEV TABLE
CCC004*     (ST_NR                INTEGER              NOT NULL
CCC004*     ,MONAT                DATE                 NOT NULL
CCC004*     ,KNR_PROD_SPARTE      SMALLINT             NOT NULL
CCC004*     ,KNR_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,KTY_PROD_SPARTE      SMALLINT             NOT NULL
CCC004*     ,KTY_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,EH_GS_ZU_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_GS_ZU_GR          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_GS_AB_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_GS_AB_GR          DECIMAL(10, 1)       NOT NULL
CCC004*     ,PV_GS_NT_EG          DECIMAL(11, 2)       NOT NULL
CCC004*     ,PV_GS_NT_GR          DECIMAL(11, 2)       NOT NULL
CCC004*     ,PV_FG_NT_EG          DECIMAL(11, 2)       NOT NULL
CCC004*     ,PV_FG_NT_GR          DECIMAL(11, 2)       NOT NULL
CCC004*     ,EH_FG_ZU_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_FG_ZU_GR          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_FG_AB_EG          DECIMAL(10, 1)       NOT NULL
CCC004*     ,EH_FG_AB_GR          DECIMAL(10, 1)       NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1414-T
        01 TBOVPSEV-SATZ.
         10 ST-NR                             PIC S9(9) COMP.
         10 MONAT                             PIC X(10).
         10 KNR-PROD-SPARTE                   PIC S9(4) COMP.
         10 KNR-WAEHRUNG                      PIC S9(4) COMP.
         10 KTY-PROD-SPARTE                   PIC S9(4) COMP.
         10 KTY-WAEHRUNG                      PIC S9(4) COMP.
         10 EH-GS-ZU-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-GS-ZU-GR                       PIC S9(9)V9(1) COMP-3.
         10 EH-GS-AB-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-GS-AB-GR                       PIC S9(9)V9(1) COMP-3.
         10 PV-GS-NT-EG                       PIC S9(9)V9(2) COMP-3.
         10 PV-GS-NT-GR                       PIC S9(9)V9(2) COMP-3.
         10 PV-FG-NT-EG                       PIC S9(9)V9(2) COMP-3.
         10 PV-FG-NT-GR                       PIC S9(9)V9(2) COMP-3.
         10 EH-FG-ZU-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-FG-ZU-GR                       PIC S9(9)V9(1) COMP-3.
         10 EH-FG-AB-EG                       PIC S9(9)V9(1) COMP-3.
         10 EH-FG-AB-GR                       PIC S9(9)V9(1) COMP-3.
