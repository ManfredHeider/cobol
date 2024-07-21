      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:04:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0001)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0001))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0001 TABLE
CCC004*    ( DI_TRANS_ID                    CHAR(4) NOT NULL,
CCC004*      DI_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      DI_TRANS_UP                    CHAR(4) NOT NULL,
CCC004*      DI_PROG_NAME                   CHAR(8) NOT NULL,
CCC004*      DI_TYP                         CHAR(1) NOT NULL,
CCC004*      DI_BILD_ANZ                    SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0001                           *
      ******************************************************************
       01  VIAT0001-GESAMT.
        05  NI-VIAT0001-TAB.
           10 NI-VIAT0001-ELE      PIC S9(04) COMP  OCCURS 6 TIMES.
        05  NI-VIAT0001            REDEFINES  NI-VIAT0001-TAB.
           10 NI-DI-TRANS-ID       PIC S9(4) USAGE COMP.
           10 NI-DI-LAST-UPDATE    PIC S9(4) USAGE COMP.
           10 NI-DI-TRANS-UP       PIC S9(4) USAGE COMP.
           10 NI-DI-PROG-NAME      PIC S9(4) USAGE COMP.
           10 NI-DI-TYP            PIC S9(4) USAGE COMP.
           10 NI-DI-BILD-ANZ       PIC S9(4) USAGE COMP.
        05 DCL-VIAT0001.
           10 DI-TRANS-ID          PIC X(4).
           10 DI-LAST-UPDATE       PIC X(26).
           10 DI-TRANS-UP          PIC X(4).
           10 DI-PROG-NAME         PIC X(8).
           10 DI-TYP               PIC X(1).
           10 DI-BILD-ANZ          PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
