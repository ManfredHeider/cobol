      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0124.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPA0124.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:14:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   04 Aug 1997 13:13:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0124)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0124 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      MPIN_MAX_PIN                   INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0124                           *
      ******************************************************************
       01  TBPA0124-GESAMT.
         05  DCL-TBPA0124.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 MPIN-MAX-PIN         PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
