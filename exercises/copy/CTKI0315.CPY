      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0315.CPY  $
      *    $Revision:   2.21  $
      *
      *    $Log:   L:/mf/CPY/CTKI0315.CPv  $
      *
      *   Rev 2.21   Nov 24 2020 09:05:54   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.20   Feb 24 2020 11:42:36   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.19   Mar 09 2018 16:11:22   t13ex78
      *AKI01565: OZPs und Copies - TCKI0315
      *
      *   Rev 2.18   Jan 16 2018 09:42:36   t13ex78
      *AKI01562: OZPs und Copies
      *
      *   Rev 2.17   Dec 08 2015 12:52:20   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens - DWS
      *
      *   Rev 2.16   Nov 06 2015 12:28:58   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens  OZP ZTKI0315
      *
      *   Rev 2.15   Oct 19 2015 13:34:38   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *W H O
      *
      *   Rev 2.14   Nov 27 2014 09:45:50   t13ex39
      *AKI01327: Anpassung der Depotansicht Online KI Wertentwicklu
      *ng - DeuBa ZTKI0315
      *
      *   Rev 2.13   Sep 29 2010 07:44:56   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.12   Sep 23 2010 12:22:06   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.11   Nov 16 2009 11:57:52   t13ex27
      *AKI00897: DB-Änderung für Rel. 4.66
      *
      *   Rev 2.10   Aug 12 2009 12:06:34   t13ex27
      *AKI00843: DB-Änderungen für Rel. 4.60-DWS
      *
      *   Rev 2.9   Apr 03 2008 15:21:06   t13ex27
      *AKI00708: OnlineKI - Deutsche Bank Depot - angezeigte Daten
      *DB-Änderung
      *
      *   Rev 2.8   Sep 18 2006 10:48:14   t13ex27
      *AKI00624: DB-Änderung für Fix 93 DCLGENs
      *
      *   Rev 2.7   Sep 22 2004 13:07:38   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 CTKI's
      *
      *   Rev 2.6   Jul 10 2003 16:27:32   t13ex48
      *FKI00754.ue001 Falsche Feldlänge GHPPERFxx
      *
      *   Rev 2.5   Apr 02 2003 16:04:30   t13ex27
      *AKI00347 - DB-Änderung für Fix 38
      *
      *   Rev 2.4   Dec 16 2002 12:03:22   t13ex27
      *AKI00321 - DB-Änderung für Fixpack 31
      *
      *   Rev 2.3   Oct 15 2002 15:35:38   t13ex27
      *AKI00308 DB-Änderung für Fixpack 27
      *
      *   Rev 2.2   Jun 10 2002 09:50:26   T13EX27
      *AKI00289 -  DB-Änderun für neue 'VP-Analyse'
      *
      *   Rev 2.1   12 Sep 2000 16:21:46   T13EX27
      *AKI00149 DB-Änderung für Fixpack 16
      *
      *   Rev 2.0   Feb 27 1999 12:50:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:28:46   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GUTHABENPRODUKT TABLE
CCC004*    ( GHPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      GHPGUTHABPRODLFDNR             SMALLINT NOT NULL,
CCC004*      GHPPRODUKTID                   SMALLINT NOT NULL,
CCC004*      GHPANLAGENAME                  VARCHAR(15),
CCC004*      GHPSCHLAUSZGESCH               DECIMAL(11, 2),
CCC004*      GHPSCHLAUSZGWKZ                CHAR(1) NOT NULL,
CCC004*      GHPZINSSATZ                    DECIMAL(5, 2),
CCC004*      GHPINFOWPERREKZ                CHAR(1) NOT NULL,
CCC004*      GHPINFOSPERREDATUM             DATE,
CCC004*      GHPVERFUEGUNGID                CHAR(2),
CCC004*      GHPGUTHABENANT                 DECIMAL(9, 3),
CCC004*      GHPGUTHABENWERT                DECIMAL(11, 2),
CCC004*      GHPGUTHWERTWKZ                 CHAR(1) NOT NULL,
CCC004*      GHPGUTHABEN                    DECIMAL(11, 2),
CCC004*      GHPGUTHABENWKZ                 CHAR(1) NOT NULL,
CCC004*      GHPSUBTYPE                     CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      GHPRISIKOKLASSE                SMALLINT,
CCC004*      GHPEIGENAUFWAND                DECIMAL(11, 2),
CCC004*      GHPFAXREVERS                   CHAR(1) NOT NULL,
CCC004*      GHPTELEFONREVERS               CHAR(1) NOT NULL,
CCC004*      GHPONLINEREVERS                CHAR(1) NOT NULL,
CCC004*      GHPZWISCHENGEWINN              DECIMAL(11, 2),
CCC004*      GHPZWISCHENGEWWKZ              CHAR(1) NOT NULL,
CCC004*      GHPPERFLFDJAHR                 DECIMAL(18, 5),
CCC004*      GHPPERFGESAMT                  DECIMAL(18, 5),
CCC004*      GHPEIGENVERMITTELT             CHAR(1) NOT NULL,
CCC004*      GHPOFFNUNGSDATUM               DATE,
CCC004*      GHPSCHLIESSDATUM               DATE,
CCC004*      GHPANLAGEZIEL                  CHAR(1) NOT NULL,
CCC004*      GHPABSCHLUSSVBNR               INTEGER,
CCC004*      GHPDEPOTART                    CHAR(1),
CCC004*      GHPPERFDAT                     DATE,
CCC004*      GHPBRPERFLFJBEG                DATE,
CCC004*      GHPBRPERFLFJEND                DATE,
CCC004*      GHPBRPERFGESBEG                DATE,
CCC004*      GHPBRPERFGESEND                DATE,
CCC004*      GHPPORTFOLIONR                 CHAR(10),
CCC004*      GHPDETAILNAM_KTY               SMALLINT,
CCC004*      GHPDETAILNAM_KNR               DECIMAL(15, 0),
CCC004*      GHPNRGWINKLBBA                 DECIMAL(13, 2),
CCC004*      GHPNRGWINKLBBAVH               DECIMAL(8, 2),
CCC004*      GHPNRGWOHNEBBA                 DECIMAL(13, 2),
CCC004*      GHPNRGWOHNEBBAVH               DECIMAL(8, 2),
CCC004*      GHPRGWUDEPOT                   DECIMAL(11, 2),
CCC004*      GHPPROVISIONKZ                 CHAR(1),
CCC004*      GHPSONDERKONDI                 CHAR(1),
CCC004*      GHPWPHGDAT                     DATE,
CCC004*      GHPEMAILADRESSE                VARCHAR(128),
CCC004*      GHPBLZKUNDEN                   CHAR(8),
CCC004*      GHPESAFE                       CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GUTHABENPRODUKT             *
      ******************************************************************
       01  GUTHABENPRODUKT-GESAMT.
        05 NI-GUTHABENPRODUKT-TAB.
           10 NI-GUTHABENPRODUKT-ELE PIC S9(04) COMP OCCURS 51 TIMES.
        05 NI-GUTHABENPRODUKT REDEFINES NI-GUTHABENPRODUKT-TAB.
           10 NI-GHPVERTRAGSID      PIC S9(4) COMP.
           10 NI-GHPGUTHABPRODLFDNR PIC S9(4) COMP.
           10 NI-GHPPRODUKTID       PIC S9(4) COMP.
           10 NI-GHPANLAGENAME      PIC S9(4) COMP.
           10 NI-GHPSCHLAUSZGESCH   PIC S9(4) COMP.
           10 NI-GHPSCHLAUSZGWKZ    PIC S9(4) COMP.
           10 NI-GHPZINSSATZ        PIC S9(4) COMP.
           10 NI-GHPINFOWPERREKZ    PIC S9(4) COMP.
           10 NI-GHPINFOSPERREDATUM PIC S9(4) COMP.
           10 NI-GHPVERFUEGUNGID    PIC S9(4) COMP.
           10 NI-GHPGUTHABENANT     PIC S9(4) COMP.
           10 NI-GHPGUTHABENWERT    PIC S9(4) COMP.
           10 NI-GHPGUTHWERTWKZ     PIC S9(4) COMP.
           10 NI-GHPGUTHABEN        PIC S9(4) COMP.
           10 NI-GHPGUTHABENWKZ     PIC S9(4) COMP.
           10 NI-GHPSUBTYPE         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-GHPRISIKOKLASSE    PIC S9(4) COMP.
           10 NI-GHPEIGENAUFWAND    PIC S9(4) COMP.
           10 NI-GHPFAXREVERS       PIC S9(4) COMP.
           10 NI-GHPTELEFONREVERS   PIC S9(4) COMP.
           10 NI-GHPONLINEREVERS    PIC S9(4) COMP.
           10 NI-GHPZWISCHENGEWINN  PIC S9(4) COMP.
           10 NI-GHPZWISCHENGEWWKZ  PIC S9(4) COMP.
           10 NI-GHPPERFLFDJAHR     PIC S9(4) COMP.
           10 NI-GHPPERFGESAMT      PIC S9(4) COMP.
           10 NI-GHPEIGENVERMITTELT PIC S9(4) COMP.
           10 NI-GHPOFFNUNGSDATUM   PIC S9(4) COMP.
           10 NI-GHPSCHLIESSDATUM   PIC S9(4) COMP.
           10 NI-GHPANLAGEZIEL      PIC S9(4) COMP.
           10 NI-GHPABSCHLUSSVBNR   PIC S9(4) COMP.
           10 NI-GHPDEPOTART        PIC S9(4) COMP.
           10 NI-GHPPERFDAT         PIC S9(4) COMP.
           10 NI-GHPBRPERFLFJBEG    PIC S9(4) COMP.
           10 NI-GHPBRPERFLFJEND    PIC S9(4) COMP.
           10 NI-GHPBRPERFGESBEG    PIC S9(4) COMP.
           10 NI-GHPBRPERFGESEND    PIC S9(4) COMP.
           10 NI-GHPPORTFOLIONR     PIC S9(4) COMP.
           10 NI-GHPDETAILNAM-KTY   PIC S9(4) COMP.
           10 NI-GHPDETAILNAM-KNR   PIC S9(4) COMP.
           10 NI-GHPNRGWINKLBBA     PIC S9(4) COMP.
           10 NI-GHPNRGWINKLBBAVH   PIC S9(4) COMP.
           10 NI-GHPNRGWOHNEBBA     PIC S9(4) COMP.
           10 NI-GHPNRGWOHNEBBAVH   PIC S9(4) COMP.
           10 NI-GHPRGWUDEPOT       PIC S9(4) COMP.
           10 NI-GHPPROVISIONKZ     PIC S9(4) COMP.
           10 NI-GHPSONDERKONDI     PIC S9(4) COMP.
           10 NI-GHPWPHGDAT         PIC S9(4) COMP.
           10 NI-GHPEMAILADRESSE    PIC S9(4) COMP.
           10 NI-GHPBLZKUNDEN       PIC S9(4) COMP.
           10 NI-GHPESAFE           PIC S9(4) COMP.
        05 DCL-GUTHABENPRODUKT.
           10 GHPVERTRAGSID        PIC X(13).
           10 GHPGUTHABPRODLFDNR   PIC S9(4) USAGE COMP.
           10 GHPPRODUKTID         PIC S9(4) USAGE COMP.
           10 GHPANLAGENAME.
              49 GHPANLAGENAME-LEN
                 PIC S9(4) USAGE COMP.
              49 GHPANLAGENAME-TEXT
                 PIC X(15).
           10 GHPSCHLAUSZGESCH     PIC S9(9)V9(2) USAGE COMP-3.
           10 GHPSCHLAUSZGWKZ      PIC X(1).
           10 GHPZINSSATZ          PIC S9(3)V9(2) USAGE COMP-3.
           10 GHPINFOWPERREKZ      PIC X(1).
           10 GHPINFOSPERREDATUM   PIC X(10).
           10 GHPVERFUEGUNGID      PIC X(2).
           10 GHPGUTHABENANT       PIC S9(6)V9(3) USAGE COMP-3.
           10 GHPGUTHABENWERT      PIC S9(9)V9(2) USAGE COMP-3.
           10 GHPGUTHWERTWKZ       PIC X(1).
           10 GHPGUTHABEN          PIC S9(9)V9(2) USAGE COMP-3.
           10 GHPGUTHABENWKZ       PIC X(1).
           10 GHPSUBTYPE           PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 GHPRISIKOKLASSE      PIC S9(4) USAGE COMP.
           10 GHPEIGENAUFWAND      PIC S9(9)V9(2) USAGE COMP-3.
           10 GHPFAXREVERS         PIC X(1).
           10 GHPTELEFONREVERS     PIC X(1).
           10 GHPONLINEREVERS      PIC X(1).
           10 GHPZWISCHENGEWINN    PIC S9(9)V9(2) USAGE COMP-3.
           10 GHPZWISCHENGEWWKZ    PIC X(1).
           10 GHPPERFLFDJAHR       PIC S9(13)V9(5) USAGE COMP-3.
           10 GHPPERFGESAMT        PIC S9(13)V9(5) USAGE COMP-3.
           10 GHPEIGENVERMITTELT   PIC X(1).
           10 GHPOFFNUNGSDATUM     PIC X(10).
           10 GHPSCHLIESSDATUM     PIC X(10).
           10 GHPANLAGEZIEL        PIC X(1).
           10 GHPABSCHLUSSVBNR     PIC S9(9) USAGE COMP.
           10 GHPDEPOTART          PIC X(1).
           10 GHPPERFDAT           PIC X(10).
           10 GHPBRPERFLFJBEG      PIC X(10).
           10 GHPBRPERFLFJEND      PIC X(10).
           10 GHPBRPERFGESBEG      PIC X(10).
           10 GHPBRPERFGESEND      PIC X(10).
           10 GHPPORTFOLIONR       PIC X(10).
           10 GHPDETAILNAM-KTY     PIC S9(4) USAGE COMP.
           10 GHPDETAILNAM-KNR     PIC S9(15)V USAGE COMP-3.
           10 GHPNRGWINKLBBA       PIC S9(11)V9(2) USAGE COMP-3.
           10 GHPNRGWINKLBBAVH     PIC S9(6)V9(2) USAGE COMP-3.
           10 GHPNRGWOHNEBBA       PIC S9(11)V9(2) USAGE COMP-3.
           10 GHPNRGWOHNEBBAVH     PIC S9(6)V9(2) USAGE COMP-3.
           10 GHPRGWUDEPOT         PIC S9(9)V9(2) USAGE COMP-3.
           10 GHPPROVISIONKZ       PIC X(1).
           10 GHPSONDERKONDI       PIC X(1).
           10 GHPWPHGDAT           PIC X(10).
           10 GHPEMAILADRESSE.
              49 GHPEMAILADRESSE-LEN
                 PIC S9(4) USAGE COMP.
              49 GHPEMAILADRESSE-TEXT
                 PIC X(128).
           10 GHPBLZKUNDEN         PIC X(8).
           10 GHPESAFE             PIC X(1).
      ************************** ENDE-DCLGEN *************************
