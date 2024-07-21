      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:49:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:04   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:53:24   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  JURPERSONANSPRPERS  TABLE
CCC004*    ( JPANATPERSONENID CHAR (13) NOT NULL,
CCC004*      JPAJPPERSONENID CHAR (13) NOT NULL,
CCC004*      JPAFIRMENBEZARTID SMALLINT,
CCC004*      JPAJPPERSONSPOS CHAR (30),
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  JURPERSONANSPRPERS-GESAMT.
        05  NI-JURPERSONANSPRPERS-TAB.
           10 NI-JURPERSONANSPRPERS-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-JURPERSONANSPRPERS  REDEFINES  NI-JURPERSONANSPRPERS-TAB.
           10 NI-JPANATPERSONENID         PIC S9(4) COMP.
           10 NI-JPAJPPERSONENID          PIC S9(4) COMP.
           10 NI-JPAFIRMENBEZARTID        PIC S9(4) COMP.
           10 NI-JPAJPPERSONSPOS          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-JURPERSONANSPRPERS.
           10 JPANATPERSONENID            PIC X(13).
           10 JPAJPPERSONENID             PIC X(13).
           10 JPAFIRMENBEZARTID           PIC S9(4) COMP.
           10 JPAJPPERSONSPOS             PIC X(30).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
