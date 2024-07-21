      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:05:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0004)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0004))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0004 TABLE
CCC004*    ( HELP_MAP_ID                    CHAR(7) NOT NULL,
CCC004*      HELP_FELD_NAME                 CHAR(7)  NOT NULL,
CCC004*      HELP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      HELP_TEXT                      CHAR(70) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0004                           *
      ******************************************************************
       01  VIAT0004-GESAMT.
        05  NI-VIAT0004-TAB.
           10 NI-VIAT0004-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-VIAT0004            REDEFINES  NI-VIAT0004-TAB.
           10 NI-HELP-MAP-ID       PIC S9(4) USAGE COMP.
           10 NI-HELP-FELD-NAME    PIC S9(4) USAGE COMP.
           10 NI-HELP-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-HELP-TEXT         PIC S9(4) USAGE COMP.
        05 DCL-VIAT0004.
           10 HELP-MAP-ID          PIC X(7).
           10 HELP-FELD-NAME       PIC X(7).
           10 HELP-LAST-UPDATE     PIC X(26).
           10 HELP-TEXT            PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
