      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0322.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0322.CPv  $
      *
      *   Rev 2.2   Jan 25 2012 11:43:36   t13ex14
      *FKI01362: Korrektur KFZKASKOART SST-Generierung zu ZTKI0322
      *
      *   Rev 2.1   24 Sep 2008 08:43:50   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.0   Feb 27 1999 12:50:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:58   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE KFZKASKO TABLE
CCC004*    ( KFKVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      KFKVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      KFKKASKOART                    CHAR(2) NOT NULL,
CCC004*      KFKKASKOBEITRAG                DECIMAL(11, 2),
CCC004*      KFKKASKOBEITRWKZ               CHAR(1) NOT NULL,
CCC004*      KFKBEITRSATZFZINPR             DECIMAL(5, 2),
CCC004*      KFKREGIONALKLASSE              CHAR(3),
CCC004*      KFKSCHADFREIHEITID             SMALLINT,
CCC004*      KFKSEBLSTBETEILTK              DECIMAL(11, 2),
CCC004*      KFKSBTEILKASKOWKZ              CHAR(1) NOT NULL,
CCC004*      KFKSELBSTBETEILVK              DECIMAL(11, 2),
CCC004*      KFKSBVOLLKASKOWKZ              CHAR(1) NOT NULL,
CCC004*      KFKSELBSTBETEILKZ              CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KFZKASKO                    *
      ******************************************************************
       01  KFZKASKO-GESAMT.
        05 NI-KFZKASKO-TAB.
           10 NI-KFZKASKO-ELE PIC S9(04) COMP OCCURS 14 TIMES.
        05 NI-KFZKASKO REDEFINES NI-KFZKASKO-TAB.
           10 NI-KFKVERTRAGSID      PIC S9(4) COMP.
           10 NI-KFKVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-KFKKASKOART        PIC S9(4) COMP.
           10 NI-KFKKASKOBEITRAG    PIC S9(4) COMP.
           10 NI-KFKKASKOBEITRWKZ   PIC S9(4) COMP.
           10 NI-KFKBEITRSATZFZINPR PIC S9(4) COMP.
           10 NI-KFKREGIONALKLASSE  PIC S9(4) COMP.
           10 NI-KFKSCHADFREIHEITID PIC S9(4) COMP.
           10 NI-KFKSEBLSTBETEILTK  PIC S9(4) COMP.
           10 NI-KFKSBTEILKASKOWKZ  PIC S9(4) COMP.
           10 NI-KFKSELBSTBETEILVK  PIC S9(4) COMP.
           10 NI-KFKSBVOLLKASKOWKZ  PIC S9(4) COMP.
           10 NI-KFKSELBSTBETEILKZ  PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-KFZKASKO.
           10 KFKVERTRAGSID        PIC X(13).
           10 KFKVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 KFKKASKOART          PIC X(2).
           10 KFKKASKOBEITRAG      PIC S9(9)V9(2) USAGE COMP-3.
           10 KFKKASKOBEITRWKZ     PIC X(1).
           10 KFKBEITRSATZFZINPR   PIC S9(3)V9(2) USAGE COMP-3.
           10 KFKREGIONALKLASSE    PIC X(3).
           10 KFKSCHADFREIHEITID   PIC S9(4) USAGE COMP.
           10 KFKSEBLSTBETEILTK    PIC S9(9)V9(2) USAGE COMP-3.
           10 KFKSBTEILKASKOWKZ    PIC X(1).
           10 KFKSELBSTBETEILVK    PIC S9(9)V9(2) USAGE COMP-3.
           10 KFKSBVOLLKASKOWKZ    PIC X(1).
           10 KFKSELBSTBETEILKZ    PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
