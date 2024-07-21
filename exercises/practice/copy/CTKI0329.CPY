      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0329.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0329.CPv  $
      *
      *   Rev 2.4   Sep 20 2017 15:22:36   t13ac20
      *AKI01503: Bereinigung AML-Tabellen
      *
      *   Rev 2.3   16 Dec 2008 11:30:04   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.2   Nov 15 2005 15:23:36   t13ex27
      *AKI00533: DB-Änderung für Fix 82 CTKI's
      *
      *   Rev 2.1   Apr 05 2000 13:56:18   T13AE38
      *AKI00095
      *
      *   Rev 2.0   Feb 27 1999 12:50:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:10   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE LEBENTEILLEISTUNG TABLE
CCC004*    ( LTLVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LTLVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      LTLLAUFENDENR                  SMALLINT NOT NULL,
CCC004*      LTLAUSZAHLBETRAG               DECIMAL(11, 2),
CCC004*      LTLAUSZAHLZEITPKT              DATE,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      LTLVERRENTKAPBETR              DECIMAL(11, 2),
CCC004*      LTLVERRENTKAPBWKZ              CHAR(1) NOT NULL,
CCC004*      LTLRENTEGARZEIT                SMALLINT,
CCC004*      LTLAUSZDECKKAPBETR             DECIMAL(11, 2),
CCC004*      LTLAUSZBETRAGBKZ               CHAR(1),
CCC004*      LTLZAHLZEITPBKZ                CHAR(1),
CCC004*      LTLVERRENTKAPBKZ               CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENTEILLEISTUNG           *
      ******************************************************************
       01  LEBENTEILLEISTUNG-GESAMT.
        05 NI-LEBENTEILLEISTUNG-TAB.
           10 NI-LEBENTEILLEISTUNG-ELE PIC S9(04) COMP OCCURS 13 TIMES.
        05 NI-LEBENTEILLEISTUNG REDEFINES NI-LEBENTEILLEISTUNG-TAB.
           10 NI-LTLVERTRAGSID      PIC S9(4) COMP.
           10 NI-LTLVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-LTLLAUFENDENR      PIC S9(4) COMP.
           10 NI-LTLAUSZAHLBETRAG   PIC S9(4) COMP.
           10 NI-LTLAUSZAHLZEITPKT  PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-LTLVERRENTKAPBETR  PIC S9(4) COMP.
           10 NI-LTLVERRENTKAPBWKZ  PIC S9(4) COMP.
           10 NI-LTLRENTEGARZEIT    PIC S9(4) COMP.
           10 NI-LTLAUSZDECKKAPBETR PIC S9(4) COMP.
           10 NI-LTLAUSZBETRAGBKZ   PIC S9(4) COMP.
           10 NI-LTLZAHLZEITPBKZ    PIC S9(4) COMP.
           10 NI-LTLVERRENTKAPBKZ   PIC S9(4) COMP.
        05 DCL-LEBENTEILLEISTUNG.
           10 LTLVERTRAGSID        PIC X(13).
           10 LTLVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 LTLLAUFENDENR        PIC S9(4) USAGE COMP.
           10 LTLAUSZAHLBETRAG     PIC S9(9)V9(2) USAGE COMP-3.
           10 LTLAUSZAHLZEITPKT    PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
           10 LTLVERRENTKAPBETR    PIC S9(9)V9(2) USAGE COMP-3.
           10 LTLVERRENTKAPBWKZ    PIC X(1).
           10 LTLRENTEGARZEIT      PIC S9(4) USAGE COMP.
           10 LTLAUSZDECKKAPBETR   PIC S9(9)V9(2) USAGE COMP-3.
           10 LTLAUSZBETRAGBKZ     PIC X(1).
           10 LTLZAHLZEITPBKZ      PIC X(1).
           10 LTLVERRENTKAPBKZ     PIC X(1).
      ************************** ENDE-DCLGEN *************************

