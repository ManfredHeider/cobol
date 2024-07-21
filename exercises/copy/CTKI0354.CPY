      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0354.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0354.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:52   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSTARIFROLLE  TABLE
CCC004*    ( VTRVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VTRVERSTARLFDNR SMALLINT NOT NULL,
CCC004*      VTRPERSONENID CHAR (13) NOT NULL,
CCC004*      VTRPERSONENROLLEKZ CHAR NOT NULL,
CCC004*      VTRABLAUFDATUM DATE,
CCC004*      VTRGUELTIGKEITSDAT DATE,
CCC004*      VTRBEZIEHUNGSARTKZ CHAR NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSTARIFROLLE-GESAMT.
        05  NI-VERSTARIFROLLE-TAB.
           10 NI-VERSTARIFROLLE-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-VERSTARIFROLLE  REDEFINES  NI-VERSTARIFROLLE-TAB.
           10 NI-VTRVERTRAGSID            PIC S9(4) COMP.
           10 NI-VTRVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-VTRPERSONENID            PIC S9(4) COMP.
           10 NI-VTRPERSONENROLLEKZ       PIC S9(4) COMP.
           10 NI-VTRABLAUFDATUM           PIC S9(4) COMP.
           10 NI-VTRGUELTIGKEITSDAT       PIC S9(4) COMP.
           10 NI-VTRBEZIEHUNGSARTKZ       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VERSTARIFROLLE.
           10 VTRVERTRAGSID               PIC X(13).
           10 VTRVERSTARLFDNR             PIC S9(4) COMP.
           10 VTRPERSONENID               PIC X(13).
           10 VTRPERSONENROLLEKZ          PIC X(01).
           10 VTRABLAUFDATUM              PIC X(10).
           10 VTRGUELTIGKEITSDAT          PIC X(10).
           10 VTRBEZIEHUNGSARTKZ          PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
