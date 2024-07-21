      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0202.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRAN0202.CPv  $
      *
      *   Rev 2.11   Mar 31 2016 08:20:54   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.10   Mar 02 2016 19:14:18   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.9   Feb 17 2016 17:33:26   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.8   Jan 22 2016 08:02:18   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.7   Jul 25 2013 10:51:02   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.6   May 15 2012 10:39:22   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.5   Dec 14 2011 17:03:12   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.4   May 27 2009 16:35:42   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26
      *- Beitrag
      *sfeld öffnen -
      *
      *   Rev 2.2   Nov 24 2008 14:20:36   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:51:36   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 08 2008 10:34:20   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      ****************************************************************
      *    COPY-MEMBER: DRAN0202                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.0 !NEU                        !RAHNER    *
      * AAN00824!23.05.09!2.1 !BEITRAGSFELD ÖFFNEN        !RAHNER    *
      * AAN01033!14.12.11!2.2 !DATEN GELDWÄSCHE ECT.      !RAHNER    *
      * AAN01051!15.05.12!2.6 !INTERNE ID-NUMMERN GENERALI!RAHNER    *
      * AAN01073!        !2.7 !SEPA                       !RAHNER    *
      * AAN01230!29.09.15!2.8 !PDF                        !HEIDER    *
      * AAN01230!17.02.16!2.9 !RÜCKBAU - OHNE BLOB        !HEIDER    *
      * AAN01230!02.03.16!2.10!DATEINAME                  !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
           05  GENL-PC-DATEN.
      *
               10  GENL-PC-PGEB-GES     PIC S9(4) COMP.
               10  GENL-PC-HVANTNUM     PIC X(09).
               10  GENL-PC-PIN          PIC S9(9) COMP.
      *
      *        VERTRIEBSDATEN / INTERNE DATEN
      *            ABSCHLUSSVERMITTLER
               10  GENL-PC-AVERM        PIC 9(07)         COMP-3.
      *            PRÜFZIFFER AVERM
               10  GENL-PC-AVERM-PZ     PIC X(01).
      *            ABSCHLUSSVERMITTLER-ERFASSUNG
               10  GENL-PC-AVERM-E      PIC 9(07)         COMP-3.
      *            PRÜFZIFFER AVERM
               10  GENL-PC-AVERM-PZ-E   PIC X(01).
      *            VERTRIEBSGESELLSCHAFT VB
               10  GENL-PC-VB-GES       PIC 9(02).
      *
      *            VM
               10  GENL-PC-VMNR         PIC 9(07)         COMP-3.
      *            PRÜFZIFFER VM
               10  GENL-PC-VMNR-PZ      PIC X(01).
      *            VOR- UND ZUNAME VM
               10  GENL-PC-VM-VORNA     PIC X(26).
               10  GENL-PC-VM-ZUNA      PIC X(26).
      *            VM-ERFASSUNG
               10  GENL-PC-VMNR-E       PIC 9(07)         COMP-3.
      *            PRÜFZIFFER VM
               10  GENL-PC-VMPZ-E       PIC X(01).
      *            VOR- UND ZUNAME VM
               10  GENL-PC-VM-VORNA-E   PIC X(26).
               10  GENL-PC-VM-ZUNA-E    PIC X(26).
      *
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG
               10  GENL-PC-VB-GW        PIC 9(07)         COMP-3.
      *            PRÜFZIFFER
               10  GENL-PC-VB-GW-PZ     PIC X(01).
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG-ERFASSUNG
               10  GENL-PC-VB-GW-E      PIC 9(07)       COMP-3.
      *            PRÜFZIFFER
               10  GENL-PC-VB-GW-PZ-E   PIC X(01).
      *
      *            DVAG-DIREKTION
               10  GENL-PC-DIR          PIC 9(03).
 
      *            VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  GENL-PC-KD-BEST-ID      PIC S9(18)V    COMP-3.
               10  GENL-PC-VN-AKD-T        PIC X(26).
               10  GENL-PC-VN-AKD-T-E      PIC X(26).
               10  GENL-PC-VN-VORNA        PIC X(26).
               10  GENL-PC-VN-VORNA-E      PIC X(26).
               10  GENL-PC-VN-ZUNA         PIC X(26).
               10  GENL-PC-VN-ZUNA-E       PIC X(26).
               10  GENL-PC-VN-GBDAT        PIC X(10).
               10  GENL-PC-VN-SVNR         PIC S9(10)V    COMP-3.
               10  GENL-PC-VN-SVNR-E       PIC S9(10)V    COMP-3.
               10  GENL-PC-VN-STR          PIC X(26).
               10  GENL-PC-VN-STR-E        PIC X(26).
               10  GENL-PC-VN-PLZ          PIC X(05).
               10  GENL-PC-VN-PLZ-E        PIC X(05).
               10  GENL-PC-VN-ORT          PIC X(26).
               10  GENL-PC-VN-ORT-E        PIC X(26).
               10  GENL-PC-VN-LKZ          PIC X(03).
               10  GENL-PC-VN-LKZ-E        PIC X(03).
               10  GENL-PC-VN-KNR          PIC S9(15)V    COMP-3.
               10  GENL-PC-VN-KNR-E        PIC S9(15)V    COMP-3.
               10  GENL-PC-VN-BLZ          PIC S9(15)V    COMP-3.
               10  GENL-PC-VN-BLZ-E        PIC S9(15)V    COMP-3.
250713         10  GENL-PC-VN-IBAN         PIC X(34).
250713         10  GENL-PC-VN-IBAN-E       PIC X(34).
      *
141211*        DATEN IM RAHMEN GELDWÄSCHE/COMPILANCE
141211         10  GENL-PC-VN-STAA         PIC X(03).
141211         10  GENL-PC-VN-STAA-E       PIC X(03).
141211         10  GENL-PC-VN-BGRP         PIC X(40).
141211         10  GENL-PC-VN-BGRP-E       PIC X(40).
141211         10  GENL-PC-VN-BRAN         PIC X(40).
141211         10  GENL-PC-VN-BRAN-E       PIC X(40).
141211         10  GENL-PC-VN-PEP          PIC X(01).
141211         10  GENL-PC-VN-PEP-E        PIC X(01).
141211*
      *        DATEN ZUR HAUPTVERSICHERUNG
               10  GENL-PC-VD-KZ-ZUW       PIC X(1).
               10  GENL-PC-VD-KZ-BUZB      PIC X(1).
               10  GENL-PC-VD-KZ-BUZR      PIC X(1).
               10  GENL-PC-VD-KZ-PO        PIC X(1).
               10  GENL-PC-VD-KZ-UTZ       PIC X(1).
               10  GENL-PC-BEGINN          PIC X(10).
               10  GENL-PC-BEGINN-E        PIC X(10).
               10  GENL-PC-PZ-DAUER        PIC S9(04)     COMP-3.
               10  GENL-PC-PZ-DAUER-E      PIC S9(04)     COMP-3.
               10  GENL-PC-SUMME           PIC S9(09)V    COMP-3.
               10  GENL-PC-SUMME-E         PIC S9(09)V    COMP-3.
               10  GENL-PC-BEITRAG         PIC S9(07)V9(02)  COMP-3.
230509         10  GENL-PC-BEITRAG-E       PIC S9(07)V9(02)  COMP-3.
               10  GENL-PC-JAHR-NTP        PIC S9(11)V9(02)  COMP-3.
               10  GENL-PC-JAHR-NTP-E      PIC S9(11)V9(02)  COMP-3.
               10  GENL-PC-PRSUM           PIC S9(11)V    COMP-3.
               10  GENL-PC-PRSUM-E         PIC S9(11)V    COMP-3.
               10  GENL-PC-ZAHLW           PIC 9(02).
               10  GENL-PC-ZAHLW-E         PIC 9(02).
               10  GENL-PC-WAEHRUNG        PIC X(03).
               10  GENL-PC-AN-ART          PIC X(01).
               10  GENL-PC-AN-ART-E        PIC X(01).
               10  GENL-PC-AN-DAT          PIC X(10).
               10  GENL-PC-AN-DAT-E        PIC X(10).
      *
      *            DVAG-PRODUKTSCHLÜSSEL
               10  GENL-PC-VD-PRODKEY      PIC 9(04)         COMP-3.
      *
      *            DVAG-ABSCHLUSSEINHEITEN
               10  GENL-PC-VD-EINH         PIC S9(9)V9(02)   COMP-3.
               10  GENL-PC-VD-EINH-E       PIC S9(9)V9(02)   COMP-3.
      *
      *            VERSIONSDATEN
               10  GENL-PC-VERSION         PIC X(05).
      *
150512*            INTERNE ID-NUMMERN GENERALI
150512*            ANTRAGSNUMMER
150512         10  GENL-PC-GEN-ANT-NR      PIC S9(15)V    COMP-3.
150512*            BÜNDELANTRAGSNUMMER
150512         10  GENL-PC-GEN-B-ANT-NR    PIC S9(15)V    COMP-3.
150512*
      *            SONSTIGE DATEN
               10  GENL-PC-LAST-UPDATE     PIC X(26).
               10  GENL-PC-KENNUNG-A       PIC X(8).
      *
      *            BLACK-BOX (XML-DATEN)
               10 GENL-PC-XML.
                  15 GENL-PC-XML-TEXT      PIC X(30000).
      *
 
020316         10  GENL-PC-EA-DATEINAME    PIC X(50).
      *
      *       ENDE COPY-MEMBER
      *


