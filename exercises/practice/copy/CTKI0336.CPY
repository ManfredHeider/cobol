      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0336.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0336.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:20   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  TEILTYPLEBEN  TABLE
CCC004*    ( TEILTYPID               SMALLINT NOT NULL,
CCC004*      TEILTYPTEXT             CHAR (25)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  TEILTYPLEBEN-GESAMT.
        05  NI-TEILTYPLEBEN-TAB.
           10 NI-TEILTYPLEBEN-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-TEILTYPLEBEN  REDEFINES  NI-TEILTYPLEBEN-TAB.
           10 NI-TEILTYPID                PIC S9(4) COMP.
           10 NI-TEILTYPTEXT              PIC S9(4) COMP.
        05  DCL-TEILTYPLEBEN.
           10 TEILTYPID                   PIC S9(4) COMP.
           10 TEILTYPTEXT                 PIC X(25).
      ******************************************************************
