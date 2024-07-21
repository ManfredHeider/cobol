      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0009.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0009.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:48:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0009)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0009))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0009 TABLE
CCC004*    ( HBL_HBILD_ID                   CHAR(6) NOT NULL,
CCC004*      HBL_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      HBL_HBILDTYP                   CHAR(1) NOT NULL,
CCC004*      HBL_HILFEGEBIET                CHAR(66) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0009                           *
      ******************************************************************
       01  TBAT0009-GESAMT.
        05  NI-TBAT0009-TAB.
           10 NI-TBAT0009-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-TBAT0009            REDEFINES  NI-TBAT0009-TAB.
           10 NI-HBL-HBILD-ID      PIC S9(4) USAGE COMP.
           10 NI-HBL-LAST-UPDATE   PIC S9(4) USAGE COMP.
           10 NI-HBL-HBILDTYP      PIC S9(4) USAGE COMP.
           10 NI-HBL-HILFEGEBIET   PIC S9(4) USAGE COMP.
        05  DCL-TBAT0009.
           10 HBL-HBILD-ID         PIC X(6).
           10 HBL-LAST-UPDATE      PIC X(26).
           10 HBL-HBILDTYP         PIC X(1).
           10 HBL-HILFEGEBIET      PIC X(66).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
