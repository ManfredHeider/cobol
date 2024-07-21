      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0048.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0048.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:01:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0048)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0048))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0048 TABLE
CCC004*    ( MSA_AKTUELL_STF                SMALLINT NOT NULL,
CCC004*      MSA_NEU_STF                    SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0048                           *
      ******************************************************************
       01  TBVB0048-GESAMT.
        05  NI-TBVB0048-TAB.
           10  NI-TBVB0048-ELE     PIC S9(04)  COMP   OCCURS  2  TIMES.
        05  NI-TBVB0048            REDEFINES   NI-TBVB0048-TAB.
           10 NI-MSA-AKTUELL-STF   PIC S9(4) USAGE COMP.
           10 NI-MSA-NEU-STF       PIC S9(4) USAGE COMP.
        05  DCL-TBVB0048.
           10 MSA-AKTUELL-STF      PIC S9(4) USAGE COMP.
           10 MSA-NEU-STF          PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
