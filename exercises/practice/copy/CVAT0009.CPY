      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0009.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0009.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:06:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0009)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0009))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0009 TABLE
CCC004*    ( HBL_HBILD_ID                   CHAR(6) NOT NULL,
CCC004*      HBL_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      HBL_HBILDTYP                   CHAR(1) NOT NULL,
CCC004*      HBL_HILFEGEBIET                CHAR(66) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0009                           *
      ******************************************************************
       01  VIAT0009-GESAMT.
        05  NI-VIAT0009-TAB.
           10 NI-VIAT0009-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-VIAT0009            REDEFINES  NI-VIAT0009-TAB.
           10 NI-HBL-HBILD-ID      PIC S9(4) USAGE COMP.
           10 NI-HBL-LAST-UPDATE   PIC S9(4) USAGE COMP.
           10 NI-HBL-HBILDTYP      PIC S9(4) USAGE COMP.
           10 NI-HBL-HILFEGEBIET   PIC S9(4) USAGE COMP.
        05  DCL-VIAT0009.
           10 HBL-HBILD-ID         PIC X(6).
           10 HBL-LAST-UPDATE      PIC X(26).
           10 HBL-HBILDTYP         PIC X(1).
           10 HBL-HILFEGEBIET      PIC X(66).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
