      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:49:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0013)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0013))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0013 TABLE
CCC004*    ( MEF_MAPNAME                    CHAR(7) NOT NULL,
CCC004*      MEF_FELDNAME                   CHAR(7) NOT NULL,
CCC004*      MEF_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      FGR_FELDGR_ID                  CHAR(6)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0013                           *
      ******************************************************************
       01  TBAT0013-GESAMT.
        05  NI-TBAT0013-TAB.
           10 NI-TBAT0013-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-TBAT0013            REDEFINES  NI-TBAT0013-TAB.
           10 NI-MEF-MAPNAME       PIC S9(4) USAGE COMP.
           10 NI-MEF-FELDNAME      PIC S9(4) USAGE COMP.
           10 NI-MEF-LAST-UPDATE   PIC S9(4) USAGE COMP.
           10 NI-FGR-FELDGR-ID     PIC S9(4) USAGE COMP.
        05  DCL-TBAT0013.
           10 MEF-MAPNAME          PIC X(7).
           10 MEF-FELDNAME         PIC X(7).
           10 MEF-LAST-UPDATE      PIC X(26).
           10 FGR-FELDGR-ID        PIC X(6).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
