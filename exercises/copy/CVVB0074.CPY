      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0074.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CVVB0074.CPv  $
      *
      *   Rev 2.1   Feb 21 2000 14:14:12   T13AE35
      *ASL00049 - DCLGEN fuer VIVB0074 AOBV
      *
      *   Rev 2.0   Feb 18 2000 10:15:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0074)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0074)               *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0074 TABLE
CCC004*    ( AOBV_BETR_NR                   INTEGER NOT NULL,
CCC004*      AOBV_UNTST_NR                  INTEGER NOT NULL,
CCC004*      AOBV_ERM_DAT                   DATE NOT NULL,
CCC004*      AOBV_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      AOBV_VB_SEIT                   TIMESTAMP NOT NULL,
CCC004*      AOBV_VB_BIS                    DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0074                           *
      ******************************************************************
       01  VIVB0074-GESAMT.
        05  NI-VIVB0074-TAB.
           10 NI-VIVB0074-ELE      PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-VIVB0074            REDEFINES   NI-VIVB0074-TAB.
           10 NI-AOBV-BETR-NR      PIC S9(04)  COMP.
           10 NI-AOBV-UNTST-NR     PIC S9(04)  COMP.
           10 NI-AOBV-ERM-DAT      PIC S9(04)  COMP.
           10 NI-AOBV-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-AOBV-VB-SEIT      PIC S9(04)  COMP.
           10 NI-AOBV-VB-BIS       PIC S9(04)  COMP.
        05  DCL-VIVB0074.
           10 AOBV-BETR-NR         PIC S9(9) USAGE COMP.
           10 AOBV-UNTST-NR        PIC S9(9) USAGE COMP.
           10 AOBV-ERM-DAT         PIC X(10).
           10 AOBV-LAST-UPDATE     PIC X(26).
           10 AOBV-VB-SEIT         PIC X(26).
           10 AOBV-VB-BIS          PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************

