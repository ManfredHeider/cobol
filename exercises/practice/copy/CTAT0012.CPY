      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0012.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0012.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:48:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0012)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0012))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0012 TABLE
CCC004*    ( FGR_FELDGR_ID                  CHAR(6) NOT NULL,
CCC004*      FGR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      HBL_HBILD_ID                   CHAR(6)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0012                           *
      ******************************************************************
       01  TBAT0012-GESAMT.
        05  NI-TBAT0012-TAB.
           10 NI-TBAT0012-ELE      PIC S9(04) COMP  OCCURS 3 TIMES.
        05  NI-TBAT0012            REDEFINES  NI-TBAT0012-TAB.
           10 NI-FGR-FELDGR-ID     PIC S9(4) USAGE COMP.
           10 NI-FGR-LAST-UPDATE   PIC S9(4) USAGE COMP.
           10 NI-HBL-HBILD-ID      PIC S9(4) USAGE COMP.
        05  DCL-TBAT0012.
           10 FGR-FELDGR-ID        PIC X(6).
           10 FGR-LAST-UPDATE      PIC X(26).
           10 HBL-HBILD-ID         PIC X(6).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
