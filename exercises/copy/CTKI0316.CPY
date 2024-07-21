      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28/06/10
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0316.CPY  $
      *    $Revision:   2.17  $
      *
      *    $Log:   L:/mf/CPY/CTKI0316.CPv  $
      *
      *   Rev 2.17   Jan 21 2021 17:11:58   u130685
      *AKI01821: Guthabenzahlung Programme
      *
      *   Rev 2.16   Jan 20 2021 13:59:20   u130685
      *AKI01821: Guthabenzahlung Programme
      *
      *   Rev 2.15   Nov 18 2019 12:48:04   t13ex39
      *AKI01733: FoDB Verarbeitung "urspr. Sparrate des Sparplan"
      * OZP
      *
      *   Rev 2.14   Jul 02 2012 10:36:30   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.13   Jun 29 2011 07:55:58   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.12   Jun 28 2010 10:18:10   t13ex48
      *AKI00970: DB-Änderungen für Rel. 5.12: DWS
      *
      *   Rev 2.0   Jun 28 2010 09:14:47   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GUTHABENZAHLUNG TABLE
CCC004*    ( FOZVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      FOZGUTHABPRODLFDNR             SMALLINT NOT NULL,
CCC004*      FOZFONDID                      SMALLINT NOT NULL,
CCC004*      FOZLFDNR                       SMALLINT NOT NULL,
CCC004*      FOZZAHLUNGSWEISEID             SMALLINT,
CCC004*      FOZPROZSATZDYNAMIS             DECIMAL(5, 2),
CCC004*      FOZZAHLUNGWKZ                  CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      FOZEINAUSDATUM                 SMALLINT,
CCC004*      FOZEINAUSTYP                   CHAR(1) NOT NULL,
CCC004*      FOZBUCHUNGSTEXTID              SMALLINT,
CCC004*      FOZZAHLUNG                     DECIMAL(11, 2),
CCC004*      FOZEINZAHFRISTEND              DATE,
CCC004*      FOZSPARPLANEROEF               DATE,
CCC004*      FOZSPARPLANSCHLUSS             DATE,
CCC004*      FOZKONTOINHABER                CHAR(27),
CCC004*      FOZBANKNAME                    CHAR(20),
CCC004*      FOZBANKBLZ                     CHAR(11),
CCC004*      FOZBANKKONTONR                 CHAR(18),
CCC004*      FOZDYNAMIK                     DECIMAL(3, 2),
CCC004*      FOZDATBEGINNAUS                DATE,
CCC004*      FOZDATENDEAUS                  DATE,
CCC004*      FOZKOSTENPROZ                  DECIMAL(3, 2),
CCC004*      FOZURSPRSPARLST                DECIMAL(8, 2),
CCC004*      FOZURSPRSPARLSTWKZ             CHAR(1),
CCC004*      FOZURSPARRATE                  DECIMAL(9, 2),
CCC004*      FOZURSPARRATEWKZ               CHAR(1),
CCC004*      FOZAAUFSCHLURSPR               DECIMAL(4, 2),
CCC004*      FOZAAUFSCHLAKTUELL             DECIMAL(4, 2),
CCC004*      FOZVERTRLAUFZEIT               SMALLINT,
CCC004*      FOZBEGDATDYNAM                 DATE,
CCC004*      FOZZLASTSCHRRUECK              SMALLINT,
CCC004*      FOZKUENDMERKMAL                CHAR(1),
CCC004*      FOZDATVERTRAUFLOE              DATE,
CCC004*      FOZVERTRAGSTYP                 CHAR(2),
CCC004*      FOZBONIFIKATION                DECIMAL(7, 4),
CCC004*      FOZURSPRVNR                    DECIMAL(12, 0),
CCC004*      FOZMAHNSTATUS                  CHAR(2),
CCC004*      FOZDAUERZULAGEKZ               CHAR(1),
CCC004*      FOZZULBERECHTKZ                CHAR(1),
CCC004*      FOZWEGKAPUEBERTR               CHAR(1),
CCC004*      FOZUEBERTRAGART                CHAR(1),
CCC004*      FOZDATKAPUEBERTR               DATE,
CCC004*      FOZDATGELDBEWEG                DATE,
CCC004*      FOZANZKINDERZUL                SMALLINT,
CCC004*      FOZGARANTIESUMME               DECIMAL(15, 2),
CCC004*      FOZDATBEGABLSTAB               DATE,
CCC004*      FOZDATBEGHOECHSI               DATE,
CCC004*      FOZDATLETHOECHST               DATE,
CCC004*      FOZBETRHOECHSTST               DECIMAL(15, 2),
CCC004*      FOZDATKUENDIGUNG               DATE,
CCC004*      FOZPIN                         INTEGER,
CCC004*      FOZABRKTO_KTY                  SMALLINT,
CCC004*      FOZABRKTO_KNR                  SMALLINT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GUTHABENZAHLUNG             *
      ******************************************************************
       01  GUTHABENZAHLUNG-GESAMT.
        05 NI-GUTHABENZAHLUNG-TAB.
           10 NI-GUTHABENZAHLUNG-ELE PIC S9(04) COMP OCCURS 54 TIMES.
        05 NI-GUTHABENZAHLUNG REDEFINES NI-GUTHABENZAHLUNG-TAB.
           10 NI-FOZVERTRAGSID      PIC S9(4) COMP.
           10 NI-FOZGUTHABPRODLFDNR PIC S9(4) COMP.
           10 NI-FOZFONDID          PIC S9(4) COMP.
           10 NI-FOZLFDNR           PIC S9(4) COMP.
           10 NI-FOZZAHLUNGSWEISEID PIC S9(4) COMP.
           10 NI-FOZPROZSATZDYNAMIS PIC S9(4) COMP.
           10 NI-FOZZAHLUNGWKZ      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-FOZEINAUSDATUM     PIC S9(4) COMP.
           10 NI-FOZEINAUSTYP       PIC S9(4) COMP.
           10 NI-FOZBUCHUNGSTEXTID  PIC S9(4) COMP.
           10 NI-FOZZAHLUNG         PIC S9(4) COMP.
           10 NI-FOZEINZAHFRISTEND  PIC S9(4) COMP.
           10 NI-FOZSPARPLANEROEF   PIC S9(4) COMP.
           10 NI-FOZSPARPLANSCHLUSS PIC S9(4) COMP.
           10 NI-FOZKONTOINHABER    PIC S9(4) COMP.
           10 NI-FOZBANKNAME        PIC S9(4) COMP.
           10 NI-FOZBANKBLZ         PIC S9(4) COMP.
           10 NI-FOZBANKKONTONR     PIC S9(4) COMP.
           10 NI-FOZDYNAMIK         PIC S9(4) COMP.
           10 NI-FOZDATBEGINNAUS    PIC S9(4) COMP.
           10 NI-FOZDATENDEAUS      PIC S9(4) COMP.
           10 NI-FOZKOSTENPROZ      PIC S9(4) COMP.
           10 NI-FOZURSPRSPARLST    PIC S9(4) COMP.
           10 NI-FOZURSPRSPARLSTWKZ PIC S9(4) COMP.
           10 NI-FOZURSPARRATE      PIC S9(4) COMP.
           10 NI-FOZURSPARRATEWKZ   PIC S9(4) COMP.
           10 NI-FOZAAUFSCHLURSPR   PIC S9(4) COMP.
           10 NI-FOZAAUFSCHLAKTUELL PIC S9(4) COMP.
           10 NI-FOZVERTRLAUFZEIT   PIC S9(4) COMP.
           10 NI-FOZBEGDATDYNAM     PIC S9(4) COMP.
           10 NI-FOZZLASTSCHRRUECK  PIC S9(4) COMP.
           10 NI-FOZKUENDMERKMAL    PIC S9(4) COMP.
           10 NI-FOZDATVERTRAUFLOE  PIC S9(4) COMP.
           10 NI-FOZVERTRAGSTYP     PIC S9(4) COMP.
           10 NI-FOZBONIFIKATION    PIC S9(4) COMP.
           10 NI-FOZURSPRVNR        PIC S9(4) COMP.
           10 NI-FOZMAHNSTATUS      PIC S9(4) COMP.
           10 NI-FOZDAUERZULAGEKZ   PIC S9(4) COMP.
           10 NI-FOZZULBERECHTKZ    PIC S9(4) COMP.
           10 NI-FOZWEGKAPUEBERTR   PIC S9(4) COMP.
           10 NI-FOZUEBERTRAGART    PIC S9(4) COMP.
           10 NI-FOZDATKAPUEBERTR   PIC S9(4) COMP.
           10 NI-FOZDATGELDBEWEG    PIC S9(4) COMP.
           10 NI-FOZANZKINDERZUL    PIC S9(4) COMP.
           10 NI-FOZGARANTIESUMME   PIC S9(4) COMP.
           10 NI-FOZDATBEGABLSTAB   PIC S9(4) COMP.
           10 NI-FOZDATBEGHOECHSI   PIC S9(4) COMP.
           10 NI-FOZDATLETHOECHST   PIC S9(4) COMP.
           10 NI-FOZBETRHOECHSTST   PIC S9(4) COMP.
           10 NI-FOZDATKUENDIGUNG   PIC S9(4) COMP.
           10 NI-FOZPIN             PIC S9(4) COMP.
           10 NI-FOZABRKTO-KTY      PIC S9(4) COMP.
           10 NI-FOZABRKTO-KNR      PIC S9(4) COMP.
        05 DCL-GUTHABENZAHLUNG.
           10 FOZVERTRAGSID        PIC X(13).
           10 FOZGUTHABPRODLFDNR   PIC S9(4) USAGE COMP.
           10 FOZFONDID            PIC S9(4) USAGE COMP.
           10 FOZLFDNR             PIC S9(4) USAGE COMP.
           10 FOZZAHLUNGSWEISEID   PIC S9(4) USAGE COMP.
           10 FOZPROZSATZDYNAMIS   PIC S9(3)V9(2) USAGE COMP-3.
           10 FOZZAHLUNGWKZ        PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 FOZEINAUSDATUM       PIC S9(4) USAGE COMP.
           10 FOZEINAUSTYP         PIC X(1).
           10 FOZBUCHUNGSTEXTID    PIC S9(4) USAGE COMP.
           10 FOZZAHLUNG           PIC S9(9)V9(2) USAGE COMP-3.
           10 FOZEINZAHFRISTEND    PIC X(10).
           10 FOZSPARPLANEROEF     PIC X(10).
           10 FOZSPARPLANSCHLUSS   PIC X(10).
           10 FOZKONTOINHABER      PIC X(27).
           10 FOZBANKNAME          PIC X(20).
           10 FOZBANKBLZ           PIC X(11).
           10 FOZBANKKONTONR       PIC X(18).
           10 FOZDYNAMIK           PIC S9(1)V9(2) USAGE COMP-3.
           10 FOZDATBEGINNAUS      PIC X(10).
           10 FOZDATENDEAUS        PIC X(10).
           10 FOZKOSTENPROZ        PIC S9(1)V9(2) USAGE COMP-3.
           10 FOZURSPRSPARLST      PIC S9(6)V9(2) USAGE COMP-3.
           10 FOZURSPRSPARLSTWKZ   PIC X(1).
           10 FOZURSPARRATE        PIC S9(7)V9(2) USAGE COMP-3.
           10 FOZURSPARRATEWKZ     PIC X(1).
           10 FOZAAUFSCHLURSPR     PIC S9(2)V9(2) USAGE COMP-3.
           10 FOZAAUFSCHLAKTUELL   PIC S9(2)V9(2) USAGE COMP-3.
           10 FOZVERTRLAUFZEIT     PIC S9(4) USAGE COMP.
           10 FOZBEGDATDYNAM       PIC X(10).
           10 FOZZLASTSCHRRUECK    PIC S9(4) USAGE COMP.
           10 FOZKUENDMERKMAL      PIC X(1).
           10 FOZDATVERTRAUFLOE    PIC X(10).
           10 FOZVERTRAGSTYP       PIC X(2).
           10 FOZBONIFIKATION      PIC S9(3)V9(4) USAGE COMP-3.
           10 FOZURSPRVNR          PIC S9(12)V USAGE COMP-3.
           10 FOZMAHNSTATUS        PIC X(2).
           10 FOZDAUERZULAGEKZ     PIC X(1).
           10 FOZZULBERECHTKZ      PIC X(1).
           10 FOZWEGKAPUEBERTR     PIC X(1).
           10 FOZUEBERTRAGART      PIC X(1).
           10 FOZDATKAPUEBERTR     PIC X(10).
           10 FOZDATGELDBEWEG      PIC X(10).
           10 FOZANZKINDERZUL      PIC S9(4) USAGE COMP.
           10 FOZGARANTIESUMME     PIC S9(13)V9(2) USAGE COMP-3.
           10 FOZDATBEGABLSTAB     PIC X(10).
           10 FOZDATBEGHOECHSI     PIC X(10).
           10 FOZDATLETHOECHST     PIC X(10).
           10 FOZBETRHOECHSTST     PIC S9(13)V9(2) USAGE COMP-3.
           10 FOZDATKUENDIGUNG     PIC X(10).
           10 FOZPIN               PIC S9(9) USAGE COMP.
           10 FOZABRKTO-KTY        PIC S9(4) USAGE COMP.
           10 FOZABRKTO-KNR        PIC S9(4) USAGE COMP.
      ************************** ENDE-DCLGEN *************************
