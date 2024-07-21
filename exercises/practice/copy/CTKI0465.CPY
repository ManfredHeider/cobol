      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20/11/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0465.CPY  $
      *    $Revision:   2.21  $
      *
      *    $Log:   L:/mf/CPY/CTKI0465.CPv  $
      *
      *   Rev 2.21   Nov 24 2020 08:48:06   u130685
      *AKI01805: Bankprodukt OZP/Copy
      *
      *   Rev 2.20   Apr 17 2020 13:38:20   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.19   Jan 22 2020 10:49:28   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen
      *
      *   Rev 2.18   Jun 19 2019 14:04:40   t13ex39
      *AKI01687: Deutsche Bank: Anzeige gebührenbefreites Konto im
      *Bestandsvertrag inkl. Erweiterung der Selektionen
      *
      *   Rev 2.17   Mar 18 2019 17:50:48   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.16   Nov 29 2018 09:53:44   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.15   Jan 25 2018 06:54:36   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.13   Aug 25 2016 09:52:46   t13ex15
      *AKI01449: Deutsche Bank Konto: Datum des Saldos ZTKI0465
      *
      *   Rev 2.11   Nov 06 2015 12:30:38   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens OZP ZTKI0465
      *
      *   Rev 2.10   Oct 21 2015 12:44:00   t13ex39
      *AKI01361: DeuBa Sparverträge  ZTKI0465
      *
      *   Rev 2.9   Sep 29 2015 12:43:12   t13ex29
      *AKI01361: DeuBa Sparverträge von DBP in BKP-Programme
      *
      *   Rev 2.8   Mar 17 2015 14:39:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.7   Jan 13 2015 13:19:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.6   Jan 16 2014 14:43:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.5   Jan 16 2014 13:25:52   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.4   Jan 16 2014 13:08:30   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.3   Jan 15 2014 15:27:30   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.2   Jan 15 2014 13:04:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.1   Jan 15 2014 12:55:22   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.0   Nov 20 2013 10:12:31   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BANKPRODUKT TABLE
CCC004*    ( BKPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BKPLFDNR                       SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      BKPPRODUKTID                   SMALLINT NOT NULL,
CCC004*      BKPKONTOART                    CHAR(3),
CCC004*      BKPTELREVERSKZ                 CHAR(1),
CCC004*      BKPONLREVERSKZ                 CHAR(1),
CCC004*      BKPKREDITART                   CHAR(6),
CCC004*      BKPDARLEHENSART                CHAR(1),
CCC004*      BKPBILANZSCHLKTY               SMALLINT,
CCC004*      BKPBILANZSCHLKNR               SMALLINT,
CCC004*      BKPKONDSCHL                    CHAR(2),
CCC004*      BKPZAHLWEISEKTY                SMALLINT NOT NULL,
CCC004*      BKPZAHLWEISEKNR                SMALLINT NOT NULL,
CCC004*      BKPDETAILKTY                   SMALLINT,
CCC004*      BKPDETAILKNR                   DECIMAL(15, 0),
CCC004*      BKPBLZKUNDEN                   CHAR(8),
CCC004*      BKPBIC                         CHAR(11),
CCC004*      BKPIBAN                        CHAR(34),
CCC004*      BKPABSCHLUSSVBNR               INTEGER,
CCC004*      BKPEIGENVERMKZ                 CHAR(1),
CCC004*      BKPWAEHRSCHL                   DECIMAL(5, 0),
CCC004*      BKPDATEROFFNUNG                DATE,
CCC004*      BKPDATSCHLIESS                 DATE,
CCC004*      BKPDATKREDITZUS                DATE,
CCC004*      BKPDATKREDITAUSZ               DATE,
CCC004*      BKPENDEZINSFEST                DATE,
CCC004*      BKPVERTRAGSENDE                DATE,
CCC004*      BKPDATFAELLIG                  DATE,
CCC004*      BKPEZRFREIJAHRE                DATE,
CCC004*      BKPDATBEGENDE                  DATE,
CCC004*      BKPDARLBETRAG                  DECIMAL(14, 2),
CCC004*      BKPDARLBETRAKT                 DECIMAL(14, 2),
CCC004*      BKPSALDOBETR                   DECIMAL(14, 2),
CCC004*      BKPLIMITEXT                    DECIMAL(14, 2),
CCC004*      BKPSOLLZINS                    DECIMAL(6, 4),
CCC004*      BKPHABENZINS                   DECIMAL(6, 4),
CCC004*      BKPUEBERZZINS                  DECIMAL(6, 4),
CCC004*      BKPRATENHOEHE                  DECIMAL(14, 2),
CCC004*      BKPANNUITAET                   DECIMAL(14, 2),
CCC004*      BKPTILGUNGSART                 CHAR(1),
CCC004*      BKPTILGUNGPROZ                 DECIMAL(6, 4),
CCC004*      BKPZINSANTEIL                  DECIMAL(14, 2),
CCC004*      BKPTILGUNGANF                  DECIMAL(14, 2),
CCC004*      BKPPROVISIONKZ                 CHAR(1),
CCC004*      BKPDATUMSALDO                  DATE,
CCC004*      BKPABRECHKONTO                 CHAR(10),
CCC004*      BKPANTEILRATE                  DECIMAL(14, 2),
CCC004*      BKPPIN                         INTEGER NOT NULL,
CCC004*      BKPRESTLAUFZEIT                SMALLINT,
CCC004*      BKPSICHPAKET                   CHAR(1),
CCC004*      BKPDATVOLLAUSZ                 DATE,
CCC004*      BKPKFWERSTR                    DECIMAL(14, 2),
CCC004*      BKPKFWFOLGER                   DECIMAL(14, 2),
CCC004*      BKPKFWKREDITB                  DECIMAL(14, 2),
CCC004*      BKPKFWSOLLZ                    DECIMAL(6, 4),
CCC004*      BKPKFWDATFEST                  DATE,
CCC004*      BKPKFWTILGUNG                  DECIMAL(6, 4),
CCC004*      BKPKOSTSCHLZKP                 CHAR(1),
CCC004*      BKPULTOSPARKZ                  CHAR(1),
CCC004*      BKPGESAMTLAUFZEIT              SMALLINT,
CCC004*      BKPVERSUMFANG1                 SMALLINT,
CCC004*      BKPVERSUMFANG2                 SMALLINT,
CCC004*      BKPESAFE                       CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BANKPRODUKT                 *
      ******************************************************************
       01  BANKPRODUKT-GESAMT.
        05 NI-BANKPRODUKT-TAB.
           10 NI-BANKPRODUKT-ELE PIC S9(04) COMP OCCURS 64 TIMES.
        05 NI-BANKPRODUKT REDEFINES NI-BANKPRODUKT-TAB.
           10 NI-BKPVERTRAGSID      PIC S9(4) COMP.
           10 NI-BKPLFDNR           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-BKPPRODUKTID       PIC S9(4) COMP.
           10 NI-BKPKONTOART        PIC S9(4) COMP.
           10 NI-BKPTELREVERSKZ     PIC S9(4) COMP.
           10 NI-BKPONLREVERSKZ     PIC S9(4) COMP.
           10 NI-BKPKREDITART       PIC S9(4) COMP.
           10 NI-BKPDARLEHENSART    PIC S9(4) COMP.
           10 NI-BKPBILANZSCHLKTY   PIC S9(4) COMP.
           10 NI-BKPBILANZSCHLKNR   PIC S9(4) COMP.
           10 NI-BKPKONDSCHL        PIC S9(4) COMP.
           10 NI-BKPZAHLWEISEKTY    PIC S9(4) COMP.
           10 NI-BKPZAHLWEISEKNR    PIC S9(4) COMP.
           10 NI-BKPDETAILKTY       PIC S9(4) COMP.
           10 NI-BKPDETAILKNR       PIC S9(4) COMP.
           10 NI-BKPBLZKUNDEN       PIC S9(4) COMP.
           10 NI-BKPBIC             PIC S9(4) COMP.
           10 NI-BKPIBAN            PIC S9(4) COMP.
           10 NI-BKPABSCHLUSSVBNR   PIC S9(4) COMP.
           10 NI-BKPEIGENVERMKZ     PIC S9(4) COMP.
           10 NI-BKPWAEHRSCHL       PIC S9(4) COMP.
           10 NI-BKPDATEROFFNUNG    PIC S9(4) COMP.
           10 NI-BKPDATSCHLIESS     PIC S9(4) COMP.
           10 NI-BKPDATKREDITZUS    PIC S9(4) COMP.
           10 NI-BKPDATKREDITAUSZ   PIC S9(4) COMP.
           10 NI-BKPENDEZINSFEST    PIC S9(4) COMP.
           10 NI-BKPVERTRAGSENDE    PIC S9(4) COMP.
           10 NI-BKPDATFAELLIG      PIC S9(4) COMP.
           10 NI-BKPEZRFREIJAHRE    PIC S9(4) COMP.
           10 NI-BKPDATBEGENDE      PIC S9(4) COMP.
           10 NI-BKPDARLBETRAG      PIC S9(4) COMP.
           10 NI-BKPDARLBETRAKT     PIC S9(4) COMP.
           10 NI-BKPSALDOBETR       PIC S9(4) COMP.
           10 NI-BKPLIMITEXT        PIC S9(4) COMP.
           10 NI-BKPSOLLZINS        PIC S9(4) COMP.
           10 NI-BKPHABENZINS       PIC S9(4) COMP.
           10 NI-BKPUEBERZZINS      PIC S9(4) COMP.
           10 NI-BKPRATENHOEHE      PIC S9(4) COMP.
           10 NI-BKPANNUITAET       PIC S9(4) COMP.
           10 NI-BKPTILGUNGSART     PIC S9(4) COMP.
           10 NI-BKPTILGUNGPROZ     PIC S9(4) COMP.
           10 NI-BKPZINSANTEIL      PIC S9(4) COMP.
           10 NI-BKPTILGUNGANF      PIC S9(4) COMP.
           10 NI-BKPPROVISIONKZ     PIC S9(4) COMP.
           10 NI-BKPDATUMSALDO      PIC S9(4) COMP.
           10 NI-BKPABRECHKONTO     PIC S9(4) COMP.
           10 NI-BKPANTEILRATE      PIC S9(4) COMP.
           10 NI-BKPPIN             PIC S9(4) COMP.
           10 NI-BKPRESTLAUFZEIT    PIC S9(4) COMP.
           10 NI-BKPSICHPAKET       PIC S9(4) COMP.
           10 NI-BKPDATVOLLAUSZ     PIC S9(4) COMP.
           10 NI-BKPKFWERSTR        PIC S9(4) COMP.
           10 NI-BKPKFWFOLGER       PIC S9(4) COMP.
           10 NI-BKPKFWKREDITB      PIC S9(4) COMP.
           10 NI-BKPKFWSOLLZ        PIC S9(4) COMP.
           10 NI-BKPKFWDATFEST      PIC S9(4) COMP.
           10 NI-BKPKFWTILGUNG      PIC S9(4) COMP.
           10 NI-BKPKOSTSCHLZKP     PIC S9(4) COMP.
           10 NI-BKPULTOSPARKZ      PIC S9(4) COMP.
           10 NI-BKPGESAMTLAUFZEIT  PIC S9(4) COMP.
           10 NI-BKPVERSUMFANG1     PIC S9(4) COMP.
           10 NI-BKPVERSUMFANG2     PIC S9(4) COMP.
           10 NI-BKPESAFE           PIC S9(4) COMP.
        05 DCL-BANKPRODUKT.
           10 BKPVERTRAGSID        PIC X(13).
           10 BKPLFDNR             PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 BKPPRODUKTID         PIC S9(4) USAGE COMP.
           10 BKPKONTOART          PIC X(3).
           10 BKPTELREVERSKZ       PIC X(1).
           10 BKPONLREVERSKZ       PIC X(1).
           10 BKPKREDITART         PIC X(6).
           10 BKPDARLEHENSART      PIC X(1).
           10 BKPBILANZSCHLKTY     PIC S9(4) USAGE COMP.
           10 BKPBILANZSCHLKNR     PIC S9(4) USAGE COMP.
           10 BKPKONDSCHL          PIC X(2).
           10 BKPZAHLWEISEKTY      PIC S9(4) USAGE COMP.
           10 BKPZAHLWEISEKNR      PIC S9(4) USAGE COMP.
           10 BKPDETAILKTY         PIC S9(4) USAGE COMP.
           10 BKPDETAILKNR         PIC S9(15)V USAGE COMP-3.
           10 BKPBLZKUNDEN         PIC X(8).
           10 BKPBIC               PIC X(11).
           10 BKPIBAN              PIC X(34).
           10 BKPABSCHLUSSVBNR     PIC S9(9) USAGE COMP.
           10 BKPEIGENVERMKZ       PIC X(1).
           10 BKPWAEHRSCHL         PIC S9(5)V USAGE COMP-3.
           10 BKPDATEROFFNUNG      PIC X(10).
           10 BKPDATSCHLIESS       PIC X(10).
           10 BKPDATKREDITZUS      PIC X(10).
           10 BKPDATKREDITAUSZ     PIC X(10).
           10 BKPENDEZINSFEST      PIC X(10).
           10 BKPVERTRAGSENDE      PIC X(10).
           10 BKPDATFAELLIG        PIC X(10).
           10 BKPEZRFREIJAHRE      PIC X(10).
           10 BKPDATBEGENDE        PIC X(10).
           10 BKPDARLBETRAG        PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPDARLBETRAKT       PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPSALDOBETR         PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPLIMITEXT          PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPSOLLZINS          PIC S9(2)V9(4) USAGE COMP-3.
           10 BKPHABENZINS         PIC S9(2)V9(4) USAGE COMP-3.
           10 BKPUEBERZZINS        PIC S9(2)V9(4) USAGE COMP-3.
           10 BKPRATENHOEHE        PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPANNUITAET         PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPTILGUNGSART       PIC X(1).
           10 BKPTILGUNGPROZ       PIC S9(2)V9(4) USAGE COMP-3.
           10 BKPZINSANTEIL        PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPTILGUNGANF        PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPPROVISIONKZ       PIC X(1).
           10 BKPDATUMSALDO        PIC X(10).
           10 BKPABRECHKONTO       PIC X(10).
           10 BKPANTEILRATE        PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPPIN               PIC S9(9) USAGE COMP.
           10 BKPRESTLAUFZEIT      PIC S9(4) USAGE COMP.
           10 BKPSICHPAKET         PIC X(1).
           10 BKPDATVOLLAUSZ       PIC X(10).
           10 BKPKFWERSTR          PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPKFWFOLGER         PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPKFWKREDITB        PIC S9(12)V9(2) USAGE COMP-3.
           10 BKPKFWSOLLZ          PIC S9(2)V9(4) USAGE COMP-3.
           10 BKPKFWDATFEST        PIC X(10).
           10 BKPKFWTILGUNG        PIC S9(2)V9(4) USAGE COMP-3.
           10 BKPKOSTSCHLZKP       PIC X(1).
           10 BKPULTOSPARKZ        PIC X(1).
           10 BKPGESAMTLAUFZEIT    PIC S9(4) USAGE COMP.
           10 BKPVERSUMFANG1       PIC S9(4) USAGE COMP.
           10 BKPVERSUMFANG2       PIC S9(4) USAGE COMP.
           10 BKPESAFE             PIC X(1).
      ************************** ENDE-DCLGEN *************************
