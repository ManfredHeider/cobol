      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0210.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0210.CPv  $
      *
      *   Rev 2.7   Jun 20 2017 12:24:06   t13ae23
      *AAN01291: Elektron. Antrag DVAG Wien Verarbeitung d. Header-
      *Datensätze
      *
      *   Rev 2.6   Jul 25 2013 10:51:36   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   May 15 2012 10:39:34   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.4   Dec 14 2011 17:27:14   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.3   Dec 14 2011 16:49:44   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 19 2008 11:28:12   t13ae34
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.1   Nov 24 2008 14:01:52   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2008 09:38:54   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0210  FUNKTION INSERT STANDARD-OZP
      *    FÜR GENS-TBAN0013
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00824!03.12.08!2.2 ! -P80 STATT -P90 ..        ! HEIDER   *
      * AAN01033!14.12.11!2.3 ! DATEN FÜR GELDWÄSCHE ECT. ! RAHNER   *
      * AAN01051!15.05.12!2.5 !INTERNE ID-NUMMERN GENERALI! RAHNER   *
      * AAN01073!25.07.13!2.6 !SEPA                       ! RAHNER   *
      * AAN01291!20.06.17!2.7 ! E-SIGN ÖSTERREICH         ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0210                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE GENS TBAN0013              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0210.
           05 PI-DRAN0210-GENS-KEY.
              10 PI-DRAN0210-PGEB-GES           PIC S9(4)    COMP.
              10 PI-DRAN0210-GENS-HVANTNUM      PIC X(9).

      *        VERTRIEBSDATEN / INTERNE DATEN
200617*            PIN
200617         10  PI-DRAN0210-GENS-PIN          PIC S9(09)  COMP-3.
      *            ABSCHLUSSVERMITTLER
               10  PI-DRAN0210-GENS-AVERM        PIC 9(07)   COMP-3.
      *            PRÜFZIFFER AVERM
               10  PI-DRAN0210-GENS-AVERMPZ      PIC X(01).
      *            ABSCHLUSSVERMITTLER-ERFASSUNG
               10  PI-DRAN0210-GENS-AVERM-E      PIC 9(07)   COMP-3.
      *            PRÜFZIFFER AVERM
               10  PI-DRAN0210-GENS-AVERMPZ-E    PIC X(01).
      *            VERTRIEBSGESELLSCHAFT VB
               10  PI-DRAN0210-GENS-VB-GES       PIC 9(02).
      *
      *            VM
               10  PI-DRAN0210-GENS-VMNR         PIC 9(07)   COMP-3.
      *            PRÜFZIFFER VM
               10  PI-DRAN0210-GENS-VMNR-PZ      PIC X(01).
      *            VOR- UND ZUNAME VM
               10  PI-DRAN0210-GENS-VM-VORNA     PIC X(26).
               10  PI-DRAN0210-GENS-VM-ZUNA      PIC X(26).
      *            VM-ERFASSUNG
               10  PI-DRAN0210-GENS-VMNR-E       PIC 9(07)   COMP-3.
      *            PRÜFZIFFER VM
               10  PI-DRAN0210-GENS-VMPZ-E       PIC X(01).
      *            VOR- UND ZUNAME VM
               10  PI-DRAN0210-GENS-VM-VORNA-E   PIC X(26).
               10  PI-DRAN0210-GENS-VM-ZUNA-E    PIC X(26).
      *
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG
               10  PI-DRAN0210-GENS-VB-GW        PIC 9(07)  COMP-3.
      *            PRÜFZIFFER
               10  PI-DRAN0210-GENS-VB-GW-PZ     PIC X(01).
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG-ERFASSUNG
               10  PI-DRAN0210-GENS-VB-GW-E      PIC 9(07)  COMP-3.
      *            PRÜFZIFFER
               10  PI-DRAN0210-GENS-VB-GW-PZ-E   PIC X(01).
      *
      *            DVAG-DIREKTION
               10  PI-DRAN0210-GENS-DIR            PIC 9(03).
      *
      *        VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  PI-DRAN0210-GENS-KD-BEST-ID      PIC S9(18)V COMP-3.
               10  PI-DRAN0210-GENS-VN-AKD-T        PIC X(26).
               10  PI-DRAN0210-GENS-VN-AKAD-T-E     PIC X(26).
               10  PI-DRAN0210-GENS-VN-VORNA        PIC X(26).
               10  PI-DRAN0210-GENS-VN-VORNA-E      PIC X(26).
               10  PI-DRAN0210-GENS-VN-ZUNA         PIC X(26).
               10  PI-DRAN0210-GENS-VN-ZUNA-E       PIC X(26).
               10  PI-DRAN0210-GENS-VN-SVNR         PIC S9(10)V COMP-3.
               10  PI-DRAN0210-GENS-VN-SVNR-E       PIC S9(10)V COMP-3.
               10  PI-DRAN0210-GENS-VN-STR          PIC X(26).
               10  PI-DRAN0210-GENS-VN-STR-E        PIC X(26).
               10  PI-DRAN0210-GENS-VN-PLZ          PIC X(05).
               10  PI-DRAN0210-GENS-VN-PLZ-E        PIC X(05).
               10  PI-DRAN0210-GENS-VN-ORT          PIC X(26).
               10  PI-DRAN0210-GENS-VN-ORT-E        PIC X(26).
               10  PI-DRAN0210-GENS-VN-LKZ          PIC X(03).
               10  PI-DRAN0210-GENS-VN-LKZ-E        PIC X(03).
               10  PI-DRAN0210-GENS-VN-KNR          PIC S9(15)V COMP-3.
               10  PI-DRAN0210-GENS-VN-KNR-E        PIC S9(15)V COMP-3.
               10  PI-DRAN0210-GENS-VN-BLZ          PIC S9(15)V COMP-3.
               10  PI-DRAN0210-GENS-VN-BLZ-E        PIC S9(15)V COMP-3.
250713         10  PI-DRAN0210-GENS-VN-IBAN         PIC X(34).
250713         10  PI-DRAN0210-GENS-VN-IBAN-E       PIC X(34).
               10  PI-DRAN0210-GENS-VN-KFZ-KZ       PIC X(10).
               10  PI-DRAN0210-GENS-VN-KFZ-KZ-E     PIC X(10).
               10  PI-DRAN0210-GENS-VN-FGST-NR      PIC X(26).
               10  PI-DRAN0210-GENS-VN-FGST-NR-E    PIC X(26).
               10  PI-DRAN0210-GENS-VN-KFZ-B-NR     PIC X(20).
               10  PI-DRAN0210-GENS-VN-KFZ-B-NR-E   PIC X(20).
      *
141211*        INFORMATIONEN IM RAHMEN GELDWÄSCHE, COMPLIANCE
141211         10  PI-DRAN0210-GENS-VN-STAAANG      PIC X(3).
141211         10  PI-DRAN0210-GENS-VN-STAAANG-E    PIC X(3).
141211         10  PI-DRAN0210-GENS-VN-GEBDAT       PIC X(10).
141211         10  PI-DRAN0210-GENS-VN-GEBDAT-E     PIC X(10).
      *
      *        DATEN ZUR VERSICHERUNG
               10  PI-DRAN0210-GENS-BEGINN          PIC X(10).
               10  PI-DRAN0210-GENS-BEGINN-E        PIC X(10).
               10  PI-DRAN0210-GENS-ENDE            PIC X(10).
               10  PI-DRAN0210-GENS-ENDE-E          PIC X(10).
               10  PI-DRAN0210-GENS-ZAHLW           PIC 9(02).
               10  PI-DRAN0210-GENS-ZAHLW-E         PIC 9(02).
               10  PI-DRAN0210-GENS-WAEHRUNG        PIC X(03).
               10  PI-DRAN0210-GENS-AN-ART          PIC X(01).
               10  PI-DRAN0210-GENS-AN-ART-E        PIC X(01).
               10  PI-DRAN0210-GENS-AN-DAT          PIC X(10).
AN2191         10  PI-DRAN0210-GENS-AN-DAT-E        PIC X(10).
AN2191*        10  PI-DRAN0210-GENS-AN-DAT-E        PIC X(01).
      *
      *        DVAG PRODUKTE MIT JEW. BETRÄGEN
               10 PI-DRAN0210-VN-PRODKEY1O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P1O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P1O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P1O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P1O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY2O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P2O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P2O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P2O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P2O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY3O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P3O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P3O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P3O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P3O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY4O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P4O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P4O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P4O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P4O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY5O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P5O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P5O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P5O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P5O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY6O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P6O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P6O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P6O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P6O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY7O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P7O      PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-BTP-P7O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P7O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P7O       PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-PRODKEY8O          PIC S9(04) COMP-3.
               10 PI-DRAN0210-VN-BTP-P8O      PIC S9(06)V9(02) COMP-3.
031208*        10 PI-DRAN0210-VN-BTP-P9O-E    PIC S9(06)V9(02) COMP-3.
031208         10 PI-DRAN0210-VN-BTP-P8O-E    PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-JNTP-P8O     PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0210-VN-EH-P8O       PIC S9(06)V9(02) COMP-3.
      *
      *            VERSIONSDATEN
               10  PI-DRAN0210-GENS-VERSION         PIC X(05).
      *
      *            SONSTIGE DATEN
               10  PI-DRAN0210-GENS-LAST-UPDATE     PIC X(26).
               10  PI-DRAN0210-GENS-KENNUNG-A       PIC X(8).

150512*            INTERNE ID-NUMMERN GENERALI
150512*            ANTRAGSNUMMER
150512         10  PI-DRAN0210-GENS-GEN-ANT-NR      PIC S9(15)V COMP-3.
150512*            BÜNDELANTRAGSNUMMER
150512         10  PI-DRAN0210-GENS-GEN-B-ANT-NR    PIC S9(15)V COMP-3.

      *            SONSTIGE DATEN
141211*        10  PI-DRAN0210-GENS-FILLER          PIC X(100).
150512*        10  PI-DRAN0210-GENS-FILLER          PIC X(74).
200617*        10  PI-DRAN0210-GENS-FILLER          PIC X(58).
200617         10  PI-DRAN0210-GENS-FILLER          PIC X(53).
      *
      *            BLACK-BOX (XML-DATEN)
               10 PI-DRAN0210-GENS-XML.
                  15 PI-DRAN0210-GENS-XML-TEXT      PIC X(30000).
      *
       01  PO-DRAN0210.
           05 PO-DRAN0210-RC                        PIC  X(003).
             88 PO-DRAN0210-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0210-RC-DUPLICATE-VALUE            VALUE 'DVA'.


