      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0010.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0010.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:48:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0010)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0010))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0010 TABLE
CCC004*    ( HBL_HBILD_ID                   CHAR(6) NOT NULL,
CCC004*      HBLT_HZEILENR                  SMALLINT NOT NULL,
CCC004*      HBLT_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      HBLT_HTEXT                     CHAR(76) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0010                           *
      ******************************************************************
       01  TBAT0010-GESAMT.
        05  NI-TBAT0010-TAB.
           10 NI-TBAT0010-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-TBAT0010            REDEFINES  NI-TBAT0010-TAB.
           10 NI-HBL-HBILD-ID      PIC S9(4) USAGE COMP.
           10 NI-HBLT-HZEILENR     PIC S9(4) USAGE COMP.
           10 NI-HBLT-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-HBLT-HTEXT        PIC S9(4) USAGE COMP.
        05  DCL-TBAT0010.
           10 HBL-HBILD-ID         PIC X(6).
           10 HBLT-HZEILENR        PIC S9(4) USAGE COMP.
           10 HBLT-LAST-UPDATE     PIC X(26).
           10 HBLT-HTEXT           PIC X(76).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
