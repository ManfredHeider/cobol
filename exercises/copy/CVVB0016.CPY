      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:14:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0016)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0016))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0016 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      MS_NR                          SMALLINT NOT NULL,
CCC004*      BS_NR                          SMALLINT NOT NULL,
CCC004*      MSBZ_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      IA_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      IA_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      IA_DAT_BIS                     DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0016                           *
      ******************************************************************
       01  VIVB0016-GESAMT.
        05  NI-VIVB0016-TAB.
           10  NI-VIVB0016-ELE     PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-VIVB0016            REDEFINES   NI-VIVB0016-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-BS-NR             PIC S9(04)  COMP.
           10 NI-MSBZ-DAT-VON      PIC S9(04)  COMP.
           10 NI-IA-DAT-VON        PIC S9(04)  COMP.
           10 NI-IA-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-IA-DAT-BIS        PIC S9(04)  COMP.
        05  DCL-VIVB0016.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 BS-NR                PIC S9(4) USAGE COMP.
           10 MSBZ-DAT-VON         PIC X(26).
           10 IA-DAT-VON           PIC X(26).
           10 IA-LAST-UPDATE       PIC X(26).
           10 IA-DAT-BIS           PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
