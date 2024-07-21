      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVATA007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVATA007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   11 Aug 1997 15:08:22   T13EX23
      *afpa0011 - COPY-NAME eigefuegt
      *
      *   Rev 1.0   11 Aug 1997 09:45:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      * T13EX23   Stand: 24/07/97    08:50:07
      ******************************************************************
      * DCLGEN TABLE(VIATA007)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0091))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIATA007 TABLE
CCC004*    ( KTY_NR                         SMALLINT NOT NULL,
CCC004*      KTY_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KTY_BEZ                        CHAR(40) NOT NULL,
CCC003*      KTY_COPY_NAME                  CHAR(08) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA007                            *
      ********************************************************************
       01  VIATA007-GESAMT.
        05  NI-VIATA007-TAB.
           10  NI-VIATA007-ELE     PIC S9(04)  COMP   OCCURS  4  TIMES.
        05  NI-VIATA007            REDEFINES   NI-VIATA007-TAB.
           10 NI-KTY-NR            PIC S9(04)  COMP.
           10 NI-KTY-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-KTY-BEZ           PIC S9(04)  COMP.
           10 NI-KTY-COPY-NAME     PIC S9(04)  COMP.
        05  DCL-VIATA007.
           10 KTY-NR               PIC S9(4) USAGE COMP.
           10 KTY-LAST-UPDATE      PIC X(26).
           10 KTY-BEZ              PIC X(40).
           10 KTY-COPY-NAME        PIC X(08).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
