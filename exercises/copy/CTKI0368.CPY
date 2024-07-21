      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0368.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0368.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   25 Mar 1998 18:20:58   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  ZAHLUNGSART  TABLE
CCC004*    ( ZAHLUNGSARTID           SMALLINT NOT NULL,
CCC004*      ZAHLUNGSARTTEXT         CHAR (25) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  ZAHLUNGSART-GESAMT.
        05  NI-ZAHLUNGSART-TAB.
           10 NI-ZAHLUNGSART-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-ZAHLUNGSART  REDEFINES  NI-ZAHLUNGSART-TAB.
           10 NI-ZAHLUNGSARTID            PIC S9(4) COMP.
           10 NI-ZAHLUNGSARTTEXT          PIC S9(4) COMP.
        05  DCL-ZAHLUNGSART.
           10 ZAHLUNGSARTID               PIC S9(4) COMP.
           10 ZAHLUNGSARTTEXT             PIC X(25).
      ******************************************************************
