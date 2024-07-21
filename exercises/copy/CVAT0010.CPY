      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0010.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0010.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:06:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0010)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0010))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0010 TABLE
CCC004*    ( HBL_HBILD_ID                   CHAR(6) NOT NULL,
CCC004*      HBLT_HZEILENR                  SMALLINT NOT NULL,
CCC004*      HBLT_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      HBLT_HTEXT                     CHAR(76) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0010                           *
      ******************************************************************
       01  VIAT0010-GESAMT.
        05  NI-VIAT0010-TAB.
           10 NI-VIAT0010-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-VIAT0010            REDEFINES  NI-VIAT0010-TAB.
           10 NI-HBL-HBILD-ID      PIC S9(4) USAGE COMP.
           10 NI-HBLT-HZEILENR     PIC S9(4) USAGE COMP.
           10 NI-HBLT-LAST-UPDATE  PIC S9(4) USAGE COMP.
           10 NI-HBLT-HTEXT        PIC S9(4) USAGE COMP.
        05  DCL-VIAT0010.
           10 HBL-HBILD-ID         PIC X(6).
           10 HBLT-HZEILENR        PIC S9(4) USAGE COMP.
           10 HBLT-LAST-UPDATE     PIC X(26).
           10 HBLT-HTEXT           PIC X(76).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
