      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0048.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0048.CPv  $
      *
      *   Rev 2.3   Mar 03 2005 10:12:22   t13ex27
      *AKI00474: DB-Änderung für Fix 73 CTKIxxxx
      *
      *   Rev 2.2   Nov 04 2002 15:43:48   t13ex27
      *FKI00672 Fehlerhafte Auftbereitung PERSREFERENZNUMMER
      *
      *   Rev 2.1   Oct 15 2002 15:35:26   t13ex27
      *AKI00308 DB-Änderung für Fixpack 27
      *
      *   Rev 2.0   Feb 27 1999 12:49:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:27:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:54:46   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  PERSREFERENZNUMMER  TABLE
CCC004*    ( PREFNUMPERSONENID              CHAR(13) NOT NULL,
CCC004*      PREFNUMARTID                   SMALLINT NOT NULL,
CCC004*      PREFNUMLFDNUMMER               INTEGER NOT NULL,
CCC004*      PREFNUMMER                     CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      PREFAUSGESTELLTAM              DATE NOT NULL,
CCC004*      PREFAUSGESTELLTORT             VARCHAR(30) NOT NULL,
CCC004*      PREFGUELTIGBIS                 DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  PERSREFERENZNUMMER-GESAMT.
        05  NI-PERSREFERENZNUMMER-TAB.
030305     10 NI-PERSREFERENZNUMMER-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-PERSREFERENZNUMMER  REDEFINES  NI-PERSREFERENZNUMMER-TAB.
           10 NI-PREFNUMPERSONENID        PIC S9(4) COMP.
           10 NI-PREFNUMARTID             PIC S9(4) COMP.
           10 NI-PREFNUMLFDNUMMER         PIC S9(4) COMP.
           10 NI-PREFNUMMER               PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
300902     10 NI-PREFAUSGESTELLTAM        PIC S9(4) COMP.
300902     10 NI-PREFAUSGESTELLTORT       PIC S9(4) COMP.
030305     10 NI-PREFGUELTIGBIS           PIC S9(4) COMP.
        05  DCL-PERSREFERENZNUMMER.
           10 PREFNUMPERSONENID           PIC X(13).
           10 PREFNUMARTID                PIC S9(4) COMP.
           10 PREFNUMLFDNUMMER            PIC S9(9) COMP.
           10 PREFNUMMER                  PIC X(30).
           10 LETZTEAENDERUNG             PIC X(26).
300902     10 PREFAUSGESTELLTAM           PIC X(10).
300902     10 PREFAUSGESTELLTORT.
300902        49 PREFAUSGESTELLTORT-LEN   PIC S9(4) COMP.
300902        49 PREFAUSGESTELLTORT-TEXT  PIC X(30).
030305     10 PREFGUELTIGBIS              PIC X(10).
      ******************************************************************
