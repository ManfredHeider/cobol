      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0360.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0360.CPv  $
      *
      *   Rev 2.1   Apr 29 2009 14:58:30   t13ex14
      *AKI00818: Tabelle VERTRAGRABATT OZP-Objekte
      *
      *   Rev 2.0   Feb 27 1999 12:50:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:04   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERTRAGRABATT  TABLE
CCC004*    ( VRAVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VRALFDNR SMALLINT NOT NULL,
CCC004*      VRARABTEXT CHAR(26),
CCC004*      VRARABWERT DECIMAL (11, 2),
CCC004*      VRARABWERWKZ CHAR NOT NULL,
CCC004*      VRARABEINHEIT SMALLINT,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERTRAGRABATT-GESAMT.
        05  NI-VERTRAGRABATT-TAB.
           10 NI-VERTRAGRABATT-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05  NI-VERTRAGRABATT  REDEFINES  NI-VERTRAGRABATT-TAB.
           10 NI-VRAVERTRAGSID            PIC S9(4) COMP.
           10 NI-VRALFDNR                 PIC S9(4) COMP.
           10 NI-VRARABTEXT               PIC S9(4) COMP.
           10 NI-VRARABWERT               PIC S9(4) COMP.
           10 NI-VRARABWERWKZ             PIC S9(4) COMP.
           10 NI-VRARABEINHEIT            PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VERTRAGRABATT.
           10 VRAVERTRAGSID               PIC X(13).
           10 VRALFDNR                    PIC S9(4) COMP.
           10 VRARABTEXT                  PIC X(26).
           10 VRARABWERT                  PIC S9(9)V9(2) COMP-3.
           10 VRARABWERWKZ                PIC X(01).
           10 VRARABEINHEIT               PIC S9(4) COMP.
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
