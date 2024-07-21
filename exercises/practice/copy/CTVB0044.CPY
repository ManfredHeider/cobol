      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0044.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0044.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:00:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0044)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0044))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0044 TABLE
CCC004*    ( KTY_SSTS_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_SSTS_TYP                   SMALLINT NOT NULL,
CCC004*      MS_NR                          SMALLINT NOT NULL,
CCC004*      VBP_NR                         SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0044                           *
      ******************************************************************
       01  TBVB0044-GESAMT.
        05  NI-TBVB0044-TAB.
           10  NI-TBVB0044-ELE     PIC S9(04)  COMP   OCCURS  4  TIMES.
        05  NI-TBVB0044            REDEFINES   NI-TBVB0044-TAB.
           10 NI-KTY-SSTS-TYP      PIC S9(04)  COMP.
           10 NI-KNR-SSTS-TYP      PIC S9(04)  COMP.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-VBP-NR            PIC S9(04)  COMP.
        05  DCL-TBVB0044.
           10 KTY-SSTS-TYP         PIC S9(4) USAGE COMP.
           10 KNR-SSTS-TYP         PIC S9(4) USAGE COMP.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 VBP-NR               PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
