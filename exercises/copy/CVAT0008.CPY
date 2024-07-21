      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:06:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0008)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0098))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0008 TABLE
CCC004*    ( KTY_NR                         SMALLINT NOT NULL,
CCC004*      KNR_NR                         SMALLINT NOT NULL,
CCC004*      KNR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KNR_BEZ                        CHAR(80) NOT NULL,
CCC004*      KNR_BEZ_KURZ                   CHAR(4) NOT NULL,
CCC004*      KNR_HINDERNIS                  CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0008                           *
      ******************************************************************
       01  VIAT0008-GESAMT.
        05  NI-VIAT0008-TAB.
           10  NI-VIAT0008-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-VIAT0008            REDEFINES   NI-VIAT0008-TAB.
           10 NI-KTY-NR            PIC S9(04)  COMP.
           10 NI-KNR-NR            PIC S9(04)  COMP.
           10 NI-KNR-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-KNR-BEZ           PIC S9(04)  COMP.
           10 NI-KNR-BEZ-KURZ      PIC S9(04)  COMP.
           10 NI-KNR-HINDERNIS     PIC S9(04)  COMP.
        05  DCL-VIAT0008.
           10 KTY-NR               PIC S9(4) USAGE COMP.
           10 KNR-NR               PIC S9(4) USAGE COMP.
           10 KNR-LAST-UPDATE      PIC X(26).
           10 KNR-BEZ              PIC X(80).
           10 KNR-BEZ-KURZ         PIC X(4).
           10 KNR-HINDERNIS        PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
