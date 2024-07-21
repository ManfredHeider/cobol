      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0044.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0044.CPv  $
      *
      *   Rev 2.1   Aug 11 2003 10:53:52   t13ex27
      *AKI00360 - DB-Änderung für Fix 46 und
      *           neues Adressverfahren
      *
      *   Rev 2.0   Feb 27 1999 12:49:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:27:24   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:54:30   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSONADRESSBEZ  TABLE
CCC004*    ( PERSABPERSONENID               CHAR(13) NOT NULL,
CCC004*      PERSABADRESSEID                CHAR(13) NOT NULL,
CCC004*      PERSABADRESSARTID              SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      PERSABVERTRADRKZ               CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSONADRESSBEZ-GESAMT.
        05  NI-PERSONADRESSBEZ-TAB.
           10 NI-PERSONADRESSBEZ-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-PERSONADRESSBEZ  REDEFINES  NI-PERSONADRESSBEZ-TAB.
           10 NI-PERSABPERSONENID         PIC S9(4) COMP.
           10 NI-PERSABADRESSEID          PIC S9(4) COMP.
           10 NI-PERSABADRESSARTID        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
           10 NI-PERSABVERTRADRKZ         PIC S9(4) COMP.
        05  DCL-PERSONADRESSBEZ.
           10 PERSABPERSONENID     PIC X(13).
           10 PERSABADRESSEID      PIC X(13).
           10 PERSABADRESSARTID    PIC S9(4)  COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 PERSABVERTRADRKZ     PIC X(1).
      ******************************************************************
