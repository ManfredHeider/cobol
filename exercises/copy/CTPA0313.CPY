      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0313.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/CTPA0313.CPv  $
      *
      *   Rev 2.14   Mar 24 2020 06:54:44   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.13   May 14 2018 11:13:00   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.12   Mar 28 2017 08:22:24   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.11   Oct 02 2015 09:35:48   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.10   Oct 02 2015 09:13:06   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.9   May 04 2015 11:31:26   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.8   Mar 10 2014 16:53:28   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.7   Feb 28 2014 14:41:26   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.6   Jun 19 2012 09:32:50   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   May 21 2012 08:57:50   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   Dec 10 2007 14:50:06   t13ex21
      *APA00622: Einführung Allfinanz (MST)
      *
      *   Rev 2.3   Oct 12 2004 11:37:32   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.2   Oct 12 2004 08:17:48   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.1   Feb 17 2004 08:28:28   t13ex44
      *APA00218 - Auslagerung
      *
      *   Rev 2.0   Feb 04 2004 08:35:54   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0313)                                         *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0313))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0313 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PMBE_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMBE_VERSION                   SMALLINT NOT NULL,
CCC004*      PMBE_V_GUELTIG_AB              TIMESTAMP NOT NULL,
CCC004*      PMBE_V_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PMBE_V_KENNUNG                 CHAR(8) NOT NULL,
CCC004*      KNR_PMBE_ART                   SMALLINT NOT NULL,
CCC004*      PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_VERSION                   SMALLINT NOT NULL,
CCC004*      PMBE_RFOLGE_NR                 SMALLINT NOT NULL,
CCC004*      PMBE_PGFAELLIG_VON             DATE NOT NULL,
CCC004*      PMBE_PGFAELLIG_BIS             DATE NOT NULL,
CCC004*      PMBE_PG_ABR_DAT                DATE NOT NULL,
CCC004*      PMBE_PG_EINL_DAT               DATE NOT NULL,
CCC004*      PMBE_GSMT_PROV_DAT             DATE NOT NULL,
CCC004*      PMBE_KD_BETR                   INTEGER NOT NULL,
CCC004*      KNR_ZAHLWEISE                  SMALLINT NOT NULL,
CCC004*      PMBE_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PMBE_VSTAT_UNR                 SMALLINT NOT NULL,
CCC004*      PMBE_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PMBE_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PMBE_PG_DATEN                  VARCHAR(300) NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      PMBE_UNR                       SMALLINT NOT NULL,
CCC004*      PMBE_SCHICHT_NR                SMALLINT NOT NULL,
CCC004*      PMBE_INKST_DAT                 DATE NOT NULL,
CCC004*      KNR_INK_ART                    SMALLINT NOT NULL,
CCC004*      PMBE_SAM_NR                    INTEGER NOT NULL,
CCC004*      KNR_RAB_ST                     SMALLINT NOT NULL,
CCC004*      PMBE_VVG_KZ                    CHAR(1) NOT NULL,
CCC004*      KNR_DEPOT_KZ                   SMALLINT NOT NULL,
CCC004*      PMBE_PROV_SCHL                 CHAR(5) NOT NULL,
CCC004*      PMBE_BEW_DAT                   DATE NOT NULL,
CCC004*      PMBE_POL_DAT                   DATE NOT NULL,
CCC004*      PMBE_BESVV                     DECIMAL(1, 0) NOT NULL,
CCC004*      PMBE_PROV_BEGINN               DATE NOT NULL,
CCC004*      PMBE_PROV_ABLAUF               DATE NOT NULL,
CCC004*      PMBE_KZ_EIGENV                 CHAR(1) NOT NULL,
CCC004*      PMBE_RATENBEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMBE_PG_RECH_NR                CHAR(30) NOT NULL,
CCC004*      PMBE_HIST_UNR                  SMALLINT NOT NULL,
CCC004*      PMBE_VP_NR                     SMALLINT NOT NULL,
CCC004*      PMBE_GEL_KENNUNG               CHAR(6) NOT NULL,
CCC004*      PMBE_BEZUGSJAHR                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0313                           *
      ******************************************************************
       01  DCL-TBPA0313.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PMBE-ID              PIC S9(15)V USAGE COMP-3.
           10 PMBE-VERSION         PIC S9(4) USAGE COMP.
           10 PMBE-V-GUELTIG-AB    PIC X(26).
           10 PMBE-V-GUELTIG-BIS   PIC X(10).
           10 PMBE-V-KENNUNG       PIC X(8).
           10 KNR-PMBE-ART         PIC S9(4) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PMBE-RFOLGE-NR       PIC S9(4) USAGE COMP.
           10 PMBE-PGFAELLIG-VON   PIC X(10).
           10 PMBE-PGFAELLIG-BIS   PIC X(10).
           10 PMBE-PG-ABR-DAT      PIC X(10).
           10 PMBE-PG-EINL-DAT     PIC X(10).
           10 PMBE-GSMT-PROV-DAT   PIC X(10).
           10 PMBE-KD-BETR         PIC S9(9) USAGE COMP.
           10 KNR-ZAHLWEISE        PIC S9(4) USAGE COMP.
           10 PMBE-VSTAT           PIC X(4).
           10 PMBE-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PMBE-LAST-UPDATE     PIC X(26).
           10 PMBE-KENNUNG         PIC X(8).
           10 PMBE-PG-DATEN.
              49 PMBE-PG-DATEN-LEN  PIC S9(4) USAGE COMP.
              49 PMBE-PG-DATEN-TEXT  PIC X(300).
230212     10 KNR-MANDANT          PIC S9(4) USAGE COMP.
270214     10 PMBE-UNR             PIC S9(4) USAGE COMP.
270214     10 PMBE-SCHICHT-NR      PIC S9(4) USAGE COMP.
270214     10 PMBE-INKST-DAT       PIC X(10).
270214     10 KNR-INK-ART          PIC S9(4) USAGE COMP.
270214     10 PMBE-SAM-NR          PIC S9(9) USAGE COMP.
270214     10 KNR-RAB-ST           PIC S9(4) USAGE COMP.
270214     10 PMBE-VVG-KZ          PIC X(1).
270214     10 KNR-DEPOT-KZ         PIC S9(4) USAGE COMP.
270214     10 PMBE-PROV-SCHL       PIC X(5).
270214     10 PMBE-BEW-DAT         PIC X(10).
270214     10 PMBE-POL-DAT         PIC X(10).
270214     10 PMBE-BESVV           PIC S9(1)V USAGE COMP-3.
270214     10 PMBE-PROV-BEGINN     PIC X(10).
270214     10 PMBE-PROV-ABLAUF     PIC X(10).
270214     10 PMBE-KZ-EIGENV       PIC X(1).
270214     10 PMBE-RATENBEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
030317     10 PMBE-PG-RECH-NR      PIC X(30).
310315     10 PMBE-HIST-UNR        PIC S9(4) USAGE COMP.
021015     10 PMBE-VP-NR           PIC S9(4) USAGE COMP.
240418     10 PMBE-GEL-KENNUNG     PIC X(6).
191119     10 PMBE-BEZUGSJAHR      PIC S9(4) USAGE COMP.
      ******************************************************************

