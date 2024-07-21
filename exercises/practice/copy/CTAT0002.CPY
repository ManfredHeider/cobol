      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:46:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0002)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0002))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0002 TABLE
CCC004*    ( ERR_PROG_NAME                  CHAR(8) NOT NULL,
CCC004*      ERR_TYP                        CHAR(1) NOT NULL,
CCC004*      ERR_NR                         SMALLINT NOT NULL,
CCC004*      ERR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      ERR_TEXT                       CHAR(70) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0002                           *
      ******************************************************************
       01  TBAT0002-GESAMT.
        05  NI-TBAT0002-TAB.
           10 NI-TBAT0002-ELE      PIC S9(04) COMP  OCCURS 5 TIMES.
        05  NI-TBAT0002            REDEFINES  NI-TBAT0002-TAB.
           10 NI-ERR-PROG-NAME     PIC S9(4) USAGE COMP.
           10 NI-ERR-TYP           PIC S9(4) USAGE COMP.
           10 NI-ERR-NR            PIC S9(4) USAGE COMP.
           10 NI-ERR-LAST-UPDATE   PIC S9(4) USAGE COMP.
           10 NI-ERR-TEXT          PIC S9(4) USAGE COMP.
        05 DCL-TBAT0002.
           10 ERR-PROG-NAME        PIC X(8).
           10 ERR-TYP              PIC X(1).
           10 ERR-NR               PIC S9(4) USAGE COMP.
           10 ERR-LAST-UPDATE      PIC X(26).
           10 ERR-TEXT             PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
