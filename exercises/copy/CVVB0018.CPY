      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:14:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0018)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0018))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0018 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      DIR_NR                         SMALLINT NOT NULL,
CCC004*      DZ_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      DZ_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      DZ_DAT_BIS                     DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0018                           *
      ******************************************************************
       01  VIVB0018-GESAMT.
        05  NI-VIVB0018-TAB.
           10  NI-VIVB0018-ELE     PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  NI-VIVB0018            REDEFINES   NI-VIVB0018-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-DIR-NR            PIC S9(04)  COMP.
           10 NI-DZ-DAT-VON        PIC S9(04)  COMP.
           10 NI-DZ-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-DZ-DAT-BIS        PIC S9(04)  COMP.
        05  DCL-VIVB0018.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 DIR-NR               PIC S9(4) USAGE COMP.
           10 DZ-DAT-VON           PIC X(26).
           10 DZ-LAST-UPDATE       PIC X(26).
           10 DZ-DAT-BIS           PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
