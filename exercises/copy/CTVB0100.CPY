      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0100.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0100.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:04:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13VB.TBVB0100)                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0100))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      *O   EXEC SQL DECLARE T13VB.TBVB0100 TABLE
CCC005*    EXEC SQL DECLARE       TBVB0100 TABLE
CCC004*    ( TB_LFNR                        INTEGER NOT NULL,
CCC004*      TB_CHAR                        CHAR(200) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13VB.TBVB0100                     *
      ******************************************************************
       01  DCLTBVB0100.
           10 TB-LFNR              PIC S9(9) USAGE COMP.
           10 TB-CHAR              PIC X(200).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
