      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0380.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0380.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:51:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:38   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   17 Jan 1998 12:08:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERMOEGLEIST  TABLE
CCC004*    ( VMLVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VMLARBEITNEHMSPAR CHAR NOT NULL,
CCC004*      VMLARBSPARZULSUM DECIMAL (11, 2),
CCC004*      VMLARBSPZULSUMWKZ CHAR NOT NULL,
CCC004*      VMLARBSPARZULDAT DATE,
CCC004*      VMLVLBEITRAGGES DECIMAL (11, 2),
CCC004*      VMLBEITRGESWKZ CHAR NOT NULL,
CCC004*      VMLARBEITGEBZUSCH DECIMAL (11, 2),
CCC004*      VMLARBGEBZUSCHWKZ CHAR NOT NULL,
CCC004*      VMLENDESTEUERLFZ DATE,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERMOEGLEIST-GESAMT.
        05  NI-VERMOEGLEIST-TAB.
           10 NI-VERMOEGLEIST-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  NI-VERMOEGLEIST  REDEFINES  NI-VERMOEGLEIST-TAB.
           10 NI-VMLVERTRAGSID            PIC S9(4) COMP.
           10 NI-VMLARBEITNEHMSPAR        PIC S9(4) COMP.
           10 NI-VMLARBSPARZULSUM         PIC S9(4) COMP.
           10 NI-VMLARBSPZULSUMWKZ        PIC S9(4) COMP.
           10 NI-VMLARBSPARZULDAT         PIC S9(4) COMP.
           10 NI-VMLVLBEITRAGGES          PIC S9(4) COMP.
           10 NI-VMLBEITRGESWKZ           PIC S9(4) COMP.
           10 NI-VMLARBEITGEBZUSCH        PIC S9(4) COMP.
           10 NI-VMLARBGEBZUSCHWKZ        PIC S9(4) COMP.
           10 NI-VMLENDESTEUERLFZ         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VERMOEGLEIST.
           10 VMLVERTRAGSID               PIC X(13).
           10 VMLARBEITNEHMSPAR           PIC X(01).
           10 VMLARBSPARZULSUM            PIC S9(9)V9(2) COMP-3.
           10 VMLARBSPZULSUMWKZ           PIC X(01).
           10 VMLARBSPARZULDAT            PIC X(10).
           10 VMLVLBEITRAGGES             PIC S9(9)V9(2) COMP-3.
           10 VMLBEITRGESWKZ              PIC X(01).
           10 VMLARBEITGEBZUSCH           PIC S9(9)V9(2) COMP-3.
           10 VMLARBGEBZUSCHWKZ           PIC X(01).
           10 VMLENDESTEUERLFZ            PIC X(10).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
