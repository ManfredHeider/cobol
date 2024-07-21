      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0075.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\CVVB0075.CPv  $
      *
      *   Rev 2.2   Mar 02 2000 11:36:46   T13AE30
      *ASL00050 - DIMENSION 7
      *
      *   Rev 2.1   Mar 01 2000 16:53:48   T13AE30
      *ASL00050 - Dimension 7
      *
      *   Rev 2.0   Feb 18 2000 10:15:40   P13SP20
      *Archiverstellung
      *
      *++++++***********************************************************
      * DCLGEN TABLE(VIVB0075)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0075)               *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0075 TABLE
CCC004*    ( OLBB_BETR_NR                   INTEGER NOT NULL,
CCC004*      OLBB_BETR_STUFE                SMALLINT NOT NULL,
CCC004*      OLBB_BETR_UMS                  DECIMAL(10, 1) NOT NULL,
CCC004*      OLBB_BETR_EQF                  DECIMAL(5, 2) NOT NULL,
CCC004*      OLBB_ERM_DAT                   DATE NOT NULL,
CCC004*      OLBB_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      OLBB_PARTNER_NR                INTEGER NOT NULL,
CCC004*      OLBB_PARTNER_STUFE             SMALLINT NOT NULL,
CCC004*      OLBB_PARTNER_UMS               DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0075
      ******************************************************************
       01  VIVB0075-GESAMT.
        05  NI-VIVB0075-TAB.
           10 NI-VIVB0075-ELE      PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-VIVB0075            REDEFINES   NI-VIVB0075-TAB.
           10 NI-OLBB-BETR-NR               PIC S9(04)    COMP.
           10 NI-OLBB-BETR-STUFE            PIC S9(04)    COMP.
           10 NI-OLBB-BETR-UMS              PIC S9(04)    COMP.
           10 NI-OLBB-BETR-EQF              PIC S9(04)    COMP.
           10 NI-OLBB-ERM-DAT               PIC S9(04)    COMP.
           10 NI-OLBB-LAST-UPDATE           PIC S9(04)    COMP.
           10 NI-OLBB-PARTNER-NR            PIC S9(04)    COMP.
           10 NI-OLBB-PARTNER-STUFE         PIC S9(04)    COMP.
           10 NI-OLBB-PARTNER-UMS           PIC S9(04)    COMP.
        05  DCL-VIVB0075.
           10 OLBB-BETR-NR                  PIC S9(9)     USAGE COMP.
           10 OLBB-BETR-STUFE               PIC S9(4)     USAGE COMP.
           10 OLBB-BETR-UMS                 PIC S9(09)V9  USAGE COMP-3.
           10 OLBB-BETR-EQF                 PIC S9(03)V99 USAGE COMP-3.
           10 OLBB-ERM-DAT                  PIC X(10).
           10 OLBB-LAST-UPDATE              PIC X(26).
           10 OLBB-PARTNER-NR               PIC S9(9)     USAGE COMP.
           10 OLBB-PARTNER-STUFE            PIC S9(4)     USAGE COMP.
           10 OLBB-PARTNER-UMS              PIC S9(09)V9  USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
