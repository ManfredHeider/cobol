      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:05:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0006)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0006))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0006 TABLE
CCC004*    ( DUMMY_DATE                     DATE  NOT NULL,
CCC004*      CICS_DAT                       DATE  NOT NULL WITH DEFAULT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0006                           *
      ******************************************************************
       01  TBAT0006-GESAMT.
         05 NI-TBAT0006-TAB.
           10 NI-TBAT0006-ELE      PIC S9(4)  COMP   OCCURS   2 TIMES.
         05 NI-TBAT0006            REDEFINES  NI-TBAT0006-TAB.
           10 NI-DUMMY-DATE        PIC S9(4)  COMP.
           10 NI-CICS-DAT          PIC S9(4)  COMP.
         05 DCL-VIAT0006.
           10 DUMMY-DATE           PIC X(10).
           10 CICS-DAT             PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 1       *
      ******************************************************************
