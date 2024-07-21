      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0195.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRAN0195.CPv  $
      *
      *   Rev 2.10   Jun 20 2017 12:23:52   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.9   Mar 31 2016 08:20:46   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.8   Mar 02 2016 19:14:10   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.7   Feb 17 2016 17:33:18   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.6   Jan 22 2016 08:02:10   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.5   Jul 25 2013 10:50:32   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.4   May 15 2012 10:38:48   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.3   Dec 14 2011 17:02:28   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.2   Nov 24 2008 14:19:54   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:50:56   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 08 2008 10:33:48   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0195  FUNKTION INSERT STANDARD-OZP
      *    FÜR GENL-TBAN0012
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01033!14.12.11!2.2 ! DATEN GELDWÄSCHE ECT.     ! RAHNER   *
      * AAN01051!15.05.12!2.4 !INTERNE ID-NUMMERN GENERALI! RAHNER   *
      * AAN01073!25.07.13!2.5 !SEPA                       ! RAHNER   *
      * AAN01230!29.09.15!2.6 !PDF                        ! HEIDER   *
      * AAN01230!17.02.16!2.7 !RÜCKBAU OHNE BLOB          ! HEIDER   *
      * AAN01230!02.03.16!2.8 !DATEINAME                  ! HEIDER   *
      * AAN01291!20.06.17!2.10!E-SIGN ÖSTERREICH          ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0195                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE GENL TBAN0012              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0195.
           05 PI-DRAN0195-GENL-KEY.
              10 PI-DRAN0195-PGEB-GES           PIC S9(4)    COMP.
              10 PI-DRAN0195-GENL-HVANTNUM      PIC X(9).
 
      *        VERTRIEBSDATEN / INTERNE DATEN
200617         10  PI-DRAN0195-GENL-PIN          PIC S9(09)  COMP-3.
      *            ABSCHLUSSVERMITTLER
               10  PI-DRAN0195-GENL-AVERM        PIC 9(07)   COMP-3.
      *            PRÜFZIFFER AVERM
               10  PI-DRAN0195-GENL-AVERMPZ      PIC X(01).
      *            ABSCHLUSSVERMITTLER-ERFASSUNG
               10  PI-DRAN0195-GENL-AVERM-E      PIC 9(07)   COMP-3.
      *            PRÜFZIFFER AVERM
               10  PI-DRAN0195-GENL-AVERMPZ-E    PIC X(01).
      *            VERTRIEBSGESELLSCHAFT VB
               10  PI-DRAN0195-GENL-VB-GES       PIC 9(02).
      *
      *            VM
               10  PI-DRAN0195-GENL-VMNR         PIC 9(07)   COMP-3.
      *            PRÜFZIFFER VM
               10  PI-DRAN0195-GENL-VMNR-PZ      PIC X(01).
      *            VOR- UND ZUNAME VM
               10  PI-DRAN0195-GENL-VM-VORNA     PIC X(26).
               10  PI-DRAN0195-GENL-VM-ZUNA      PIC X(26).
      *            VM-ERFASSUNG
               10  PI-DRAN0195-GENL-VMNR-E       PIC 9(07)   COMP-3.
      *            PRÜFZIFFER VM
               10  PI-DRAN0195-GENL-VMPZ-E       PIC X(01).
      *            VOR- UND ZUNAME VM
               10  PI-DRAN0195-GENL-VM-VORNA-E   PIC X(26).
               10  PI-DRAN0195-GENL-VM-ZUNA-E    PIC X(26).
      *
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG
               10  PI-DRAN0195-GENL-VB-GW        PIC 9(07)  COMP-3.
      *            PRÜFZIFFER
               10  PI-DRAN0195-GENL-VB-GW-PZ     PIC X(01).
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG-ERFASSUNG
               10  PI-DRAN0195-GENL-VB-GW-E      PIC 9(07)  COMP-3.
      *            PRÜFZIFFER
               10  PI-DRAN0195-GENL-VB-GW-PZ-E   PIC X(01).
      *
      *            DVAG-DIREKTION
               10  PI-DRAN0195-GENL-DIR            PIC 9(03).
 
      *            VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  PI-DRAN0195-GENL-KD-BEST-ID      PIC S9(18)V COMP-3.
               10  PI-DRAN0195-GENL-VN-AKD-T        PIC X(26).
               10  PI-DRAN0195-GENL-VN-AKAD-T-E     PIC X(26).
               10  PI-DRAN0195-GENL-VN-VORNA        PIC X(26).
               10  PI-DRAN0195-GENL-VN-VORNA-E      PIC X(26).
               10  PI-DRAN0195-GENL-VN-ZUNA         PIC X(26).
               10  PI-DRAN0195-GENL-VN-ZUNA-E       PIC X(26).
               10  PI-DRAN0195-GENL-VN-GBDAT        PIC X(10).
               10  PI-DRAN0195-GENL-VN-SVNR         PIC S9(10)V COMP-3.
               10  PI-DRAN0195-GENL-VN-SVNR-E       PIC S9(10)V COMP-3.
               10  PI-DRAN0195-GENL-VN-STR          PIC X(26).
               10  PI-DRAN0195-GENL-VN-STR-E        PIC X(26).
               10  PI-DRAN0195-GENL-VN-PLZ          PIC X(05).
               10  PI-DRAN0195-GENL-VN-PLZ-E        PIC X(05).
               10  PI-DRAN0195-GENL-VN-ORT          PIC X(26).
               10  PI-DRAN0195-GENL-VN-ORT-E        PIC X(26).
               10  PI-DRAN0195-GENL-VN-LKZ          PIC X(03).
               10  PI-DRAN0195-GENL-VN-LKZ-E        PIC X(03).
               10  PI-DRAN0195-GENL-VN-KNR          PIC S9(15)V COMP-3.
               10  PI-DRAN0195-GENL-VN-KNR-E        PIC S9(15)V COMP-3.
               10  PI-DRAN0195-GENL-VN-BLZ          PIC S9(15)V COMP-3.
               10  PI-DRAN0195-GENL-VN-BLZ-E        PIC S9(15)V COMP-3.
250713         10  PI-DRAN0195-GENL-VN-IBAN         PIC X(34).
250713         10  PI-DRAN0195-GENL-VN-IBAN-E       PIC X(34).
 
141211*        INFORMATIONEN IM RAHMEN GELDWÄSCHE COMPILANCE
141211         10  PI-DRAN0195-GENL-VN-STAAANG      PIC X(3).
141211         10  PI-DRAN0195-GENL-VN-STAAANG-E    PIC X(3).
141211         10  PI-DRAN0195-GENL-VN-BGRP         PIC X(40).
141211         10  PI-DRAN0195-GENL-VN-BGRP-E       PIC X(40).
141211         10  PI-DRAN0195-GENL-VN-BRAN         PIC X(40).
141211         10  PI-DRAN0195-GENL-VN-BRAN-E       PIC X(40).
141211         10  PI-DRAN0195-GENL-VN-PEP          PIC X(01).
141211         10  PI-DRAN0195-GENL-VN-PEP-E        PIC X(01).
      *
      *        DATEN ZUR HAUPTVERSICHERUNG
               10  PI-DRAN0195-GENL-VD-KZ-ZUW   PIC X(1).
               10  PI-DRAN0195-GENL-VD-KZ-BUZB  PIC X(1).
               10  PI-DRAN0195-GENL-VD-KZ-BUZR  PIC X(1).
               10  PI-DRAN0195-GENL-VD-KZ-PO    PIC X(1).
               10  PI-DRAN0195-GENL-VD-KZ-UTZ   PIC X(1).
               10  PI-DRAN0195-GENL-BEGINN      PIC X(10).
               10  PI-DRAN0195-GENL-BEGINN-E    PIC X(10).
               10  PI-DRAN0195-GENL-PZ-DAUER    PIC S9(04)   COMP-3.
               10  PI-DRAN0195-GENL-PZ-DAUER-E  PIC S9(04)   COMP-3.
               10  PI-DRAN0195-GENL-SUMME       PIC S9(09)V  COMP-3.
               10  PI-DRAN0195-GENL-SUMME-E     PIC S9(09)V  COMP-3.
               10  PI-DRAN0195-GENL-BEITRAG     PIC S9(11)V9(02) COMP-3.
               10  PI-DRAN0195-GENL-JAHR-NTP    PIC S9(11)V9(02) COMP-3.
               10  PI-DRAN0195-GENL-JAHR-NTP-E  PIC S9(11)V9(02) COMP-3.
               10  PI-DRAN0195-GENL-PRSUM       PIC S9(11)V9(02) COMP-3.
               10  PI-DRAN0195-GENL-PRSUM-E     PIC S9(11)V  COMP-3.
               10  PI-DRAN0195-GENL-ZAHLW       PIC 9(02).
               10  PI-DRAN0195-GENL-ZAHLW-E     PIC 9(02).
               10  PI-DRAN0195-GENL-WAEHRUNG    PIC X(03).
               10  PI-DRAN0195-GENL-AN-ART      PIC X(01).
               10  PI-DRAN0195-GENL-AN-ART-E    PIC X(01).
               10  PI-DRAN0195-GENL-AN-DAT      PIC X(10).
               10  PI-DRAN0195-GENL-AN-DAT-E    PIC X(01).
      *
      *            DVAG-PRODUKTSCHLÜSSEL
               10  PI-DRAN0195-GENL-VD-PRODKEY  PIC 9(04)    COMP-3.
      *
      *            DVAG-ABSCHLUSSEINHEITEN
               10  PI-DRAN0195-GENL-VD-EINH     PIC S9(9)V9(02) COMP-3.
               10  PI-DRAN0195-GENL-VD-EINH-E   PIC S9(9)V9(02) COMP-3.
      *
      *            VERSIONSDATEN
               10  PI-DRAN0195-GENL-VERSION     PIC X(05).
      *
      *            SONSTIGE DATEN
               10  PI-DRAN0195-GENL-LAST-UPDATE PIC X(26).
               10  PI-DRAN0195-GENL-KENNUNG-A   PIC X(08).
200617*        10  PI-DRAN0195-GENL-FILLER      PIC X(100).
200617         10  PI-DRAN0195-GENL-FILLER      PIC X(95).
      *
020316         10  PI-DRAN0195-GENL-EA-DATEINAME PIC X(050).
      *
150512*            GENERALI INTERNE VERTRAGSNUMMERN
150512*            GENERALI ANTRAGSNUMMER
150512         10  PI-DRAN0195-GENL-GEN-ANT-NR      PIC S9(15)V COMP-3.
150512*            GENERALI BÜNDELANTRAGSNUMMER
150512         10  PI-DRAN0195-GENL-GEN-B-ANT-NR    PIC S9(15)V COMP-3.
      *
      *            BLACK-BOX (XML-DATEN)
               10 PI-DRAN0195-GENL-XML.
                  15 PI-DRAN0195-GENL-XML-TEXT      PIC X(30000).
      *
       01  PO-DRAN0195.
           05 PO-DRAN0195-RC                        PIC  X(003).
             88 PO-DRAN0195-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0195-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      * ENDE COPY-MEMBER ***********************************************



