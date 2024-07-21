      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0359.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CTKI0359.CPv  $
      *
      *   Rev 2.1   Oct 15 2002 15:35:42   t13ex27
      *AKI00308 DB-Änderung für Fixpack 27
      *
      *   Rev 2.0   Feb 27 1999 12:50:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:30:02   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERTRAGROLLEBEZ  TABLE
CCC004*    ( VRBVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VRBPERSONENID CHAR (13) NOT NULL,
CCC004*      VRBPERSONENROLLEKZ CHAR NOT NULL,
CCC004*      VRBPABPERSONENID CHAR (13),
CCC004*      VRBPABADRESSEID CHAR (13),
CCC004*      VRBABLAUFDATUM DATE,
CCC004*      VRBGUELTIGKEITSDAT DATE,
CCC004*      VRBAVKENNZEICHEN CHAR NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP,
CCC004*      VRBKOMMBETRKZ CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERTRAGROLLEBEZ-GESAMT.
        05  NI-VERTRAGROLLEBEZ-TAB.
300901*    10 NI-VERTRAGROLLEBEZ-ELE PIC S9(04) COMP OCCURS 9 TIMES.
           10 NI-VERTRAGROLLEBEZ-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05  NI-VERTRAGROLLEBEZ  REDEFINES  NI-VERTRAGROLLEBEZ-TAB.
           10 NI-VRBVERTRAGSID            PIC S9(4) COMP.
           10 NI-VRBPERSONENID            PIC S9(4) COMP.
           10 NI-VRBPERSONENROLLEKZ       PIC S9(4) COMP.
           10 NI-VRBPABPERSONENID         PIC S9(4) COMP.
           10 NI-VRBPABADRESSEID          PIC S9(4) COMP.
           10 NI-VRBABLAUFDATUM           PIC S9(4) COMP.
           10 NI-VRBGUELTIGKEITSDAT       PIC S9(4) COMP.
           10 NI-VRBAVKENNZEICHEN         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
300902     10 NI-VRBKOMMBETRKZ            PIC S9(4) COMP.
        05  DCL-VERTRAGROLLEBEZ.
           10 VRBVERTRAGSID               PIC X(13).
           10 VRBPERSONENID               PIC X(13).
           10 VRBPERSONENROLLEKZ          PIC X(01).
           10 VRBPABPERSONENID            PIC X(13).
           10 VRBPABADRESSEID             PIC X(13).
           10 VRBABLAUFDATUM              PIC X(10).
           10 VRBGUELTIGKEITSDAT          PIC X(10).
           10 VRBAVKENNZEICHEN            PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
300902     10 VRBKOMMBETRKZ               PIC X(1).
      ******************************************************************
