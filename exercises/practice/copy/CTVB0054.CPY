      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0054.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0054.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:02:46   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0054)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0054))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0054 TABLE
CCC004*    ( WV_TABLE_ID                    CHAR(8) NOT NULL,
CCC004*      WV_MA_NR                       INTEGER NOT NULL,
CCC004*      WV_TIMESTAMP                   TIMESTAMP NOT NULL,
CCC004*      WV_DAT_AEND                    DATE NOT NULL,
CCC004*      WV_DAT_VON                     DATE NOT NULL,
CCC004*      WV_PROGRAMM_ID                 CHAR(8) NOT NULL,
CCC004*      WV_USER_ID                     CHAR(8) NOT NULL,
CCC004*      WV_TERMINAL_ID                 CHAR(8) NOT NULL,
CCC004*      WV_FELDNAME                    CHAR(16) NOT NULL,
CCC004*      WV_FELDTYP                     CHAR(1) NOT NULL,
CCC004*      WV_AEND_LANG                   INTEGER  NOT NULL,
CCC004*      WV_AEND_WERT                   CHAR(70) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0054                           *
      ******************************************************************
       01  TBVB0054-GESAMT.
        05  NI-TBVB0054-TAB.
           10 NI-TBVB0054-ELE      PIC S9(04) COMP  OCCURS 12 TIMES.
        05  NI-TBVB0054            REDEFINES  NI-TBVB0054-TAB.
           10 NI-WV-TABLE-ID       PIC S9(4) USAGE COMP.
           10 NI-WV-MA-NR          PIC S9(4) USAGE COMP.
           10 NI-WV-TIMESTAMP      PIC S9(4) USAGE COMP.
           10 NI-WV-DAT-AEND       PIC S9(4) USAGE COMP.
           10 NI-WV-DAT-VON        PIC S9(4) USAGE COMP.
           10 NI-WV-PROGRAMM-ID    PIC S9(4) USAGE COMP.
           10 NI-WV-USER-ID        PIC S9(4) USAGE COMP.
           10 NI-WV-TERMINAL-ID    PIC S9(4) USAGE COMP.
           10 NI-WV-FELDNAME       PIC S9(4) USAGE COMP.
           10 NI-WV-FELDTYP        PIC S9(4) USAGE COMP.
           10 NI-WV-AEND-LANG      PIC S9(4) USAGE COMP.
           10 NI-WV-AEND-WERT      PIC S9(4) USAGE COMP.
        05 DCL-TBVB0054.
           10 WV-TABLE-ID          PIC X(8).
           10 WV-MA-NR             PIC S9(9) USAGE COMP.
           10 WV-TIMESTAMP         PIC X(26).
           10 WV-DAT-AEND          PIC X(10).
           10 WV-DAT-VON           PIC X(10).
           10 WV-PROGRAMM-ID       PIC X(8).
           10 WV-USER-ID           PIC X(8).
           10 WV-TERMINAL-ID       PIC X(8).
           10 WV-FELDNAME          PIC X(16).
           10 WV-FELDTYP           PIC X(1).
           10 WV-AEND-LANG         PIC S9(9) USAGE COMP.
           10 WV-AEND-WERT         PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
