      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0341.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0341.CPv  $
      *
      *   Rev 2.2   Feb 11 2004 12:51:00   t13ex27
      *AKI00403 - DB-Änderung Fix 57
      *
      *   Rev 2.1   Jun 10 2002 09:50:28   T13EX27
      *AKI00289 -  DB-Änderun für neue 'VP-Analyse'
      *
      *   Rev 2.0   Feb 27 1999 12:50:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:30   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERBVERTRAGSTEIL  TABLE
CCC004*    ( VVTVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VVTGUTHABPRODLFDNR             SMALLINT NOT NULL,
CCC004*      VVTLAUFENDENUMMER              SMALLINT NOT NULL,
CCC004*      VVTDARLEHENSSUMME              DECIMAL(11, 2),
CCC004*      VVTDARLSUMWKZ                  CHAR(1) NOT NULL,
CCC004*      VVTDARLEHENSZINSS              DECIMAL(5, 2),
CCC004*      VVTRATE                        DECIMAL(11, 2),
CCC004*      VVTRATEWKZ                     CHAR(1) NOT NULL,
CCC004*      VVTRESTSCHULD                  DECIMAL(11, 2),
CCC004*      VVTRESTSCHULDWKZ               CHAR(1) NOT NULL,
CCC004*      VVTFESTGESCHBIS                DATE,
CCC004*      VVTTILGUNGSWERT                DECIMAL(11, 2),
CCC004*      VVTTILGWERTWKZ                 CHAR(1) NOT NULL,
CCC004*      VVTTILGUNGSEINHEIT             SMALLINT,
CCC004*      VVTTILGUNGDYN                  DECIMAL(5, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VVTZWECK                       CHAR(40),
CCC004*      VVTARTID                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERBVERTRAGSTEIL-GESAMT.
        05  NI-VERBVERTRAGSTEIL-TAB.
210502*    10 NI-VERBVERTRAGSTEIL-ELE PIC S9(04) COMP OCCURS 16 TIMES.
210502*    10 NI-VERBVERTRAGSTEIL-ELE PIC S9(04) COMP OCCURS 17 TIMES.
100204     10 NI-VERBVERTRAGSTEIL-ELE PIC S9(04) COMP OCCURS 18 TIMES.
        05  NI-VERBVERTRAGSTEIL  REDEFINES  NI-VERBVERTRAGSTEIL-TAB.
           10 NI-VVTVERTRAGSID            PIC S9(4) COMP.
           10 NI-VVTGUTHABPRODLFDNR       PIC S9(4) COMP.
           10 NI-VVTLAUFENDENUMMER        PIC S9(4) COMP.
           10 NI-VVTDARLEHENSSUMME        PIC S9(4) COMP.
           10 NI-VVTDARLSUMWKZ            PIC S9(4) COMP.
           10 NI-VVTDARLEHENSZINSS        PIC S9(4) COMP.
           10 NI-VVTRATE                  PIC S9(4) COMP.
           10 NI-VVTRATEWKZ               PIC S9(4) COMP.
           10 NI-VVTRESTSCHULD            PIC S9(4) COMP.
           10 NI-VVTRESTSCHULDWKZ         PIC S9(4) COMP.
           10 NI-VVTFESTGESCHBIS          PIC S9(4) COMP.
           10 NI-VVTTILGUNGSWERT          PIC S9(4) COMP.
           10 NI-VVTTILGWERTWKZ           PIC S9(4) COMP.
           10 NI-VVTTILGUNGSEINHEIT       PIC S9(4) COMP.
           10 NI-VVTTILGUNGDYN            PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
210502     10 NI-VVTZWECK                 PIC S9(4) COMP.
100204     10 NI-VVTARTID                 PIC S9(4) COMP.
        05  DCL-VERBVERTRAGSTEIL.
           10 VVTVERTRAGSID               PIC X(13).
           10 VVTGUTHABPRODLFDNR          PIC S9(4) COMP.
           10 VVTLAUFENDENUMMER           PIC S9(4) COMP.
           10 VVTDARLEHENSSUMME           PIC S9(9)V9(2) COMP-3.
           10 VVTDARLSUMWKZ               PIC X(01).
           10 VVTDARLEHENSZINSS           PIC S9(3)V9(2) COMP-3.
           10 VVTRATE                     PIC S9(9)V9(2) COMP-3.
           10 VVTRATEWKZ                  PIC X(01).
           10 VVTRESTSCHULD               PIC S9(9)V9(2) COMP-3.
           10 VVTRESTSCHULDWKZ            PIC X(01).
           10 VVTFESTGESCHBIS             PIC X(10).
           10 VVTTILGUNGSWERT             PIC S9(9)V9(2) COMP-3.
           10 VVTTILGWERTWKZ              PIC X(01).
           10 VVTTILGUNGSEINHEIT          PIC S9(4) COMP.
           10 VVTTILGUNGDYN               PIC S9(3)V9(2) COMP-3.
           10 LETZTEAENDERUNG             PIC X(26).
210502     10 VVTZWECK                    PIC X(40).
100204     10 VVTARTID                    PIC S9(4) COMP.
      ******************************************************************
