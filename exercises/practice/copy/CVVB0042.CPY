      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0042.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0042.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:20:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0042)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0042))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0042 TABLE
CCC004*    ( BRW_UNTST_MS_NR                SMALLINT NOT NULL,
CCC004*      BRW_BETR_MS_NR                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0042                           *
      ******************************************************************
       01  VIVB0042-GESAMT.
        05  NI-VIVB0042-TAB.
           10  NI-VIVB0042-ELE     PIC S9(04)  COMP   OCCURS  2  TIMES.
        05  NI-VIVB0042            REDEFINES   NI-VIVB0042-TAB.
           10 NI-BRW-UNTST-MS-NR   PIC S9(04)  COMP.
           10 NI-BRW-BETR-MS-NR    PIC S9(04)  COMP.
        05  DCL-VIVB0042.
           10 BRW-UNTST-MS-NR      PIC S9(4) USAGE COMP.
           10 BRW-BETR-MS-NR       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
