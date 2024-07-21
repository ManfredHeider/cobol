      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:47:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBAT0005)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTAT0005))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0005 TABLE
CCC004*    ( LOG_TIMESTAMP                  TIMESTAMP NOT NULL,
CCC004*      LOG_TABLE_ID                   CHAR(8) NOT NULL,
CCC004*      LOG_MA_NR                      INTEGER NOT NULL,
CCC004*      LOG_DAT_AB                     DATE NOT NULL,
CCC004*      LOG_DAT_AEND                   DATE NOT NULL,
CCC004*      LOG_PROGRAMM_ID                CHAR(8) NOT NULL,
CCC004*      LOG_USER_ID                    CHAR(8) NOT NULL,
CCC004*      LOG_TERMINAL_ID                CHAR(8) NOT NULL,
CCC004*      LOG_AEND_KZ                    CHAR(1) NOT NULL,
CCC004*      LOG_AEND_TYP                   CHAR(1) NOT NULL,
CCC004*      LOG_TUPEL_ALT_GS               VARCHAR(1990),
CCC004*      LOG_TUPEL_NEU_GS               VARCHAR(1990)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0005                           *
      ******************************************************************
       01  TBAT0005-GESAMT.
         05 NI-TBAT0005-TAB.
           10 NI-TBAT0005-ELE      PIC S9(4)  COMP   OCCURS  12 TIMES.
         05 NI-TBAT0005            REDEFINES  NI-TBAT0005-TAB.
           10 NI-LOG-TIMESTAMP     PIC S9(4)  COMP.
           10 NI-LOG-TABLE-ID      PIC S9(4)  COMP.
           10 NI-LOG-MA-NR         PIC S9(4)  COMP.
           10 NI-LOG-DAT-AB        PIC S9(4)  COMP.
           10 NI-LOG-DAT-AEND      PIC S9(4)  COMP.
           10 NI-LOG-PROGRAMM-ID   PIC S9(4)  COMP.
           10 NI-LOG-USER-ID       PIC S9(4)  COMP.
           10 NI-LOG-TERMINAL-ID   PIC S9(4)  COMP.
           10 NI-LOG-AEND-KZ       PIC S9(4)  COMP.
           10 NI-LOG-AEND-TYP      PIC S9(4)  COMP.
           10 NI-LOG-TUPEL-ALT-GS  PIC S9(4)  COMP.
           10 NI-LOG-TUPEL-NEU-GS  PIC S9(4)  COMP.
         05  DCL-TBAT0005.
           10 LOG-TIMESTAMP        PIC X(26).
           10 LOG-TABLE-ID         PIC X(8).
           10 LOG-MA-NR            PIC S9(9) USAGE COMP.
           10 LOG-DAT-AB           PIC X(10).
           10 LOG-DAT-AEND         PIC X(10).
           10 LOG-PROGRAMM-ID      PIC X(8).
           10 LOG-USER-ID          PIC X(8).
           10 LOG-TERMINAL-ID      PIC X(8).
           10 LOG-AEND-KZ          PIC X(1).
           10 LOG-AEND-TYP         PIC X(1).
           10 LOG-TUPEL-ALT-GS.
              49 LOG-TUPEL-ALT-GS-LEN    PIC S9(4) USAGE COMP.
              49 LOG-TUPEL-ALT-GS-TEXT   PIC X(1990).
           10 LOG-TUPEL-NEU-GS.
              49 LOG-TUPEL-NEU-GS-LEN    PIC S9(4) USAGE COMP.
              49 LOG-TUPEL-NEU-GS-TEXT   PIC X(1990).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
