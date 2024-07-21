      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0048.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0048.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:20:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0048)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0048))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0048 TABLE
CCC004*    ( MSA_AKTUELL_STF                SMALLINT NOT NULL,
CCC004*      MSA_NEU_STF                    SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0048                           *
      ******************************************************************
       01  VIVB0048-GESAMT.
        05  NI-VIVB0048-TAB.
           10  NI-VIVB0048-ELE     PIC S9(04)  COMP   OCCURS  2  TIMES.
        05  NI-VIVB0048            REDEFINES   NI-VIVB0048-TAB.
           10 NI-MSA-AKTUELL-STF   PIC S9(4) USAGE COMP.
           10 NI-MSA-NEU-STF       PIC S9(4) USAGE COMP.
        05  DCL-VIVB0048.
           10 MSA-AKTUELL-STF      PIC S9(4) USAGE COMP.
           10 MSA-NEU-STF          PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
