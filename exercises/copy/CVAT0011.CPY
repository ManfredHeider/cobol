      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0011.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0011.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:07:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0011)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0011))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0011 TABLE
CCC004*    ( DI_TRANS_ID                    CHAR(4) NOT NULL,
CCC004*      TRB_BILDFNR                    SMALLINT NOT NULL,
CCC004*      TRB_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      HBL_HBILD_ID                   CHAR(6)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0011                           *
      ******************************************************************
       01  VIAT0011-GESAMT.
        05  NI-VIAT0011-TAB.
           10 NI-VIAT0011-ELE      PIC S9(04) COMP  OCCURS 4 TIMES.
        05  NI-VIAT0011            REDEFINES  NI-VIAT0011-TAB.
           10 NI-DI-TRANS-ID       PIC S9(4) USAGE COMP.
           10 NI-TRB-BILDFNR       PIC S9(4) USAGE COMP.
           10 NI-TRB-LAST-UPDATE   PIC S9(4) USAGE COMP.
           10 NI-HBL-HBILD-ID      PIC S9(4) USAGE COMP.
        05  DCL-VIAT0011.
           10 DI-TRANS-ID          PIC X(4).
           10 TRB-BILDFNR          PIC S9(4) USAGE COMP.
           10 TRB-LAST-UPDATE      PIC X(26).
           10 HBL-HBILD-ID         PIC X(6).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
