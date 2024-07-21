      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:12:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0007)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0007))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0007 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      VBP_NR                         SMALLINT NOT NULL,
CCC004*      VBPZ_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      VBPZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      VBPZ_DAT_BIS                   DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0007                           *
      ******************************************************************
       01  VIVB0007-GESAMT.
        05  NI-VIVB0007-TAB.
           10  NI-VIVB0007-ELE     PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  NI-VIVB0007            REDEFINES   NI-VIVB0007-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-VBP-NR            PIC S9(04)  COMP.
           10 NI-VBPZ-DAT-VON      PIC S9(04)  COMP.
           10 NI-VBPZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-VBPZ-DAT-BIS      PIC S9(04)  COMP.
        05  DCL-VIVB0007.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 VBP-NR               PIC S9(4) USAGE COMP.
           10 VBPZ-DAT-VON         PIC X(26).
           10 VBPZ-LAST-UPDATE     PIC X(26).
           10 VBPZ-DAT-BIS         PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
