      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0362.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/CTKI0362.CPv  $
      *
      *   Rev 2.7   Dec 09 2019 13:47:10   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.6   Jan 11 2012 09:09:28   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.5   Nov 11 2011 14:22:24   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.4   Jul 21 2009 13:34:32   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.3   Feb 26 2009 09:18:46   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.2   Feb 23 2009 08:07:50   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.1   Feb 23 2009 07:46:24   t13ex14
      *AKI00642: Programmerweiterung Ermittl., Aufber., Aktual. Wag
      *nistexte
      *
      *   Rev 2.0   Feb 27 1999 12:50:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:30:08   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 10:00:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRAGSARTBEZ TABLE
CCC004*    ( VRTVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VRTVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      VRTLFDNR                       SMALLINT NOT NULL,
CCC004*      VRTVERTART_KTY                 SMALLINT NOT NULL,
CCC004*      VRTVERTART_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      VRTVERTRAGWAGNISKZ             CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VRTSELBSTBETEIL                DECIMAL(7, 0),
CCC004*      VRTSFRVAR                      CHAR(1),
CCC004*      VRTSFRSTUFE                    CHAR(1),
CCC004*      VRTRISKBTGBRUTTO               DECIMAL(11, 2),
CCC004*      VRTRISKBRUTTOWKZ               CHAR(1),
CCC004*      VRTRISKBTGNETTO                DECIMAL(11, 2),
CCC004*      VRTRISKNETTOWKZ                CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VERTRAGSARTBEZ              *
      ******************************************************************
       01  VERTRAGSARTBEZ-GESAMT.
        05 NI-VERTRAGSARTBEZ-TAB.
           10 NI-VERTRAGSARTBEZ-ELE PIC S9(04) COMP OCCURS 14 TIMES.
        05 NI-VERTRAGSARTBEZ REDEFINES NI-VERTRAGSARTBEZ-TAB.
           10 NI-VRTVERTRAGSID      PIC S9(4) COMP.
           10 NI-VRTVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-VRTLFDNR           PIC S9(4) COMP.
           10 NI-VRTVERTART-KTY     PIC S9(4) COMP.
           10 NI-VRTVERTART-KNR     PIC S9(4) COMP.
           10 NI-VRTVERTRAGWAGNISKZ PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VRTSELBSTBETEIL    PIC S9(4) COMP.
           10 NI-VRTSFRVAR          PIC S9(4) COMP.
           10 NI-VRTSFRSTUFE        PIC S9(4) COMP.
           10 NI-VRTRISKBTGBRUTTO   PIC S9(4) COMP.
           10 NI-VRTRISKBRUTTOWKZ   PIC S9(4) COMP.
           10 NI-VRTRISKBTGNETTO    PIC S9(4) COMP.
           10 NI-VRTRISKNETTOWKZ    PIC S9(4) COMP.
        05 DCL-VERTRAGSARTBEZ.
           10 VRTVERTRAGSID        PIC X(13).
           10 VRTVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 VRTLFDNR             PIC S9(4) USAGE COMP.
           10 VRTVERTART-KTY       PIC S9(4) USAGE COMP.
           10 VRTVERTART-KNR       PIC S9(15)V USAGE COMP-3.
           10 VRTVERTRAGWAGNISKZ   PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VRTSELBSTBETEIL      PIC S9(7)V USAGE COMP-3.
           10 VRTSFRVAR            PIC X(1).
           10 VRTSFRSTUFE          PIC X(1).
           10 VRTRISKBTGBRUTTO     PIC S9(9)V9(2) USAGE COMP-3.
           10 VRTRISKBRUTTOWKZ     PIC X(1).
           10 VRTRISKBTGNETTO      PIC S9(9)V9(2) USAGE COMP-3.
           10 VRTRISKNETTOWKZ      PIC X(1).
      ************************** ENDE-DCLGEN *************************

