      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0361.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0361.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:06   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERTRAGSART  TABLE
CCC004*    ( VERTRAGSARTID           SMALLINT NOT NULL,
CCC004*      VERTRAGSARTTEXT         CHAR (25) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERTRAGSART-GESAMT.
        05  NI-VERTRAGSART-TAB.
           10 NI-VERTRAGSART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-VERTRAGSART  REDEFINES  NI-VERTRAGSART-TAB.
           10 NI-VERTRAGSARTID            PIC S9(4) COMP.
           10 NI-VERTRAGSARTTEXT          PIC S9(4) COMP.
        05  DCL-VERTRAGSART.
           10 VERTRAGSARTID               PIC S9(4) COMP.
           10 VERTRAGSARTTEXT             PIC X(25).
      ******************************************************************
