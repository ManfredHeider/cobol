      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0056.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0056.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:03:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0056)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0056))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0056 TABLE
CCC004*    ( MS_NR                          SMALLINT NOT NULL,
CCC004*      MA_ST_NR                       INTEGER  NOT NULL,
CCC004*      UMSF_UMSATZ_KZ                 CHAR(1)  NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0056                           *
      ******************************************************************
       01  TBVB0056-GESAMT.
        05  NI-TBVB0056-TAB.
           10  NI-TBVB0056-ELE     PIC S9(04)  COMP   OCCURS   3 TIMES.
        05  NI-TBVB0056            REDEFINES   NI-TBVB0056-TAB.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-UMSF-UMSATZ-KZ    PIC S9(04)  COMP.
        05  DCL-TBVB0056.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 UMSF-UMSATZ-KZ       PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
