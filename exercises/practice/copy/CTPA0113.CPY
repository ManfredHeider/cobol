      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0113.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/CTPA0113.CPv  $
      *
      *   Rev 2.15   Mar 24 2020 06:53:56   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.14   May 14 2018 11:12:36   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.13   Mar 28 2017 08:22:02   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - OZPs
      *
      *   Rev 2.12   Oct 02 2015 09:35:40   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.11   Oct 02 2015 09:12:58   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.10   May 04 2015 11:07:36   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.9   Mar 10 2014 16:52:44   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.8   Jul 26 2013 11:44:12   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.7   Jun 19 2012 09:32:36   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.6   May 21 2012 08:57:34   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   Dec 10 2007 14:36:22   t13ex21
      *APA00622: Einführung Allfinanz (MST)
      *
      *   Rev 2.4   Feb 17 2000 17:12:22   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.3   Jul 01 1999 09:18:14   T13EX21
      *APA00042 - Änderung Reihenfolge
      *PMBE-PG-Daten
      *
      *   Rev 2.2   Jun 04 1999 19:15:20   T13EX16
      *Umwandlungsfehler da Komma im DCLGEN fehlt
      *
      *   Rev 2.1   Jun 04 1999 15:19:02   T13EX21
      *
      *
      *   Rev 2.0   Feb 27 1999 14:26:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:37:04   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   04 Aug 1997 13:12:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00042!27.05.99!2.0 ! NEUERSTELLUNG             !          *
      *         !        !2.2 ! KOMMA IM DCLGEN FEHLT     !          *
      * APA00042!01.07.99!2.3 ! KORREKTUR REIHENFOLGE     !          *
      * APA00081!17.02.00!2.4 ! PGEB-GES IN SQL-STATEMENTS!          *
      * APA00622!10.12.07!2.5 ! EINFÜHRUNG ALLFINANZ (MST)!          *
      * APA01101!19.06.12!2.7 ! RESTRUKTURIERUNG          !          *
      * APA01539!26.07.13!2.8 ! ERWEITERT FÜR KALYPSO     ! RAHNER   *
      * APA01932!31.03.15!2.9 ! ERWEITERUNG FÜR LUCY      ! KÖPRÜCÜ  *
      *         !02.10.15!2.10! NEUES FELD PMBE-VP-NR     ! KÖPRÜCÜ  *
      * APA02353!24.02.17!2.13! ERWEITERUNG FÜR TELESON   ! BUCH     *
      * APA02579!24.04.18!2.14! GENERALI LEBEN            ! SCHEIDEL *
      * APA02884!19.11.19!2.15! NEUES FELD BEZUGSJAHR     ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBPA0113)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0113 TABLE
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
240217*      PMBE_PG_RECH_NR                CHAR(12) NOT NULL,
CCC004*      PMBE_PG_RECH_NR                CHAR(30) NOT NULL,
CCC004*      PMBE_HIST_UNR                  SMALLINT NOT NULL,
CCC004*      PMBE_VP_NR                     SMALLINT NOT NULL,
CCC004*      PMBE_GEL_KENNUNG               CHAR(6) NOT NULL,
CCC004*      PMBE_BEZUGSJAHR                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0113                           *
      ******************************************************************
       01  TBPA0113-GESAMT.
         05  DCL-TBPA0113.
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
              49 PMBE-PG-DATEN-TEXT PIC X(300).
           10 KNR-MANDANT          PIC S9(4) USAGE COMP.
           10 PMBE-UNR             PIC S9(4) USAGE COMP.
           10 PMBE-SCHICHT-NR      PIC S9(4) USAGE COMP.
           10 PMBE-INKST-DAT       PIC X(10).
           10 KNR-INK-ART          PIC S9(4) USAGE COMP.
           10 PMBE-SAM-NR          PIC S9(9) USAGE COMP.
           10 KNR-RAB-ST           PIC S9(4) USAGE COMP.
           10 PMBE-VVG-KZ          PIC X(1).
           10 KNR-DEPOT-KZ         PIC S9(4) USAGE COMP.
           10 PMBE-PROV-SCHL       PIC X(5).
           10 PMBE-BEW-DAT         PIC X(10).
           10 PMBE-POL-DAT         PIC X(10).
           10 PMBE-BESVV           PIC S9(1)V USAGE COMP-3.
           10 PMBE-PROV-BEGINN     PIC X(10).
           10 PMBE-PROV-ABLAUF     PIC X(10).
           10 PMBE-KZ-EIGENV       PIC X(1).
           10 PMBE-RATENBEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
240217*    10 PMBE-PG-RECH-NR      PIC X(12).
240217     10 PMBE-PG-RECH-NR      PIC X(30).
310315     10 PMBE-HIST-UNR        PIC S9(4) USAGE COMP.
021015     10 PMBE-VP-NR           PIC S9(4) USAGE COMP.
240418     10 PMBE-GEL-KENNUNG     PIC X(6).
191119     10 PMBE-BEZUGSJAHR      PIC S9(4) USAGE COMP.
      ******************************************************************

