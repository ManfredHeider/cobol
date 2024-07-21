      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0214.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0214.CPv  $
      *
      *   Rev 2.5   Jul 25 2013 10:51:58   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.4   May 15 2012 10:39:48   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.3   Dec 14 2011 17:27:44   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:50:12   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:02:18   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2008 09:39:14   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      ****************************************************************
      *    COPY-MEMBER: DRAN0214                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.0 !NEU                        !RAHNER    *
      * AAN01033!14.12.11!2.2 !DATEN GELDWÄSCHE ECT.      !RAHNER    *
      * AAN01051!15.05.12!2.4 !INTERNE ID-NUMMERN GENERALI!RAHNER    *
      * AAN01073!        !2.5 !SEPA                       !RAHNER    *
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
           05  GENS-PC-DATEN.
      *
               10  GENS-PC-PGEB-GES     PIC S9(4) COMP.
               10  GENS-PC-HVANTNUM     PIC X(09).
               10  GENS-PC-PIN          PIC S9(9) COMP.
      *
      *        VERTRIEBSDATEN / INTERNE DATEN
      *            ABSCHLUSSVERMITTLER
               10  GENS-PC-AVERM        PIC 9(07)         COMP-3.
      *            PRÜFZIFFER AVERM
               10  GENS-PC-AVERM-PZ     PIC X(01).
      *            ABSCHLUSSVERMITTLER-ERFASSUNG
               10  GENS-PC-AVERM-E      PIC 9(07)         COMP-3.
      *            PRÜFZIFFER AVERM
               10  GENS-PC-AVERM-PZ-E   PIC X(01).
      *            VERTRIEBSGESELLSCHAFT VB
               10  GENS-PC-VB-GES       PIC 9(02).
      *
      *            VM
               10  GENS-PC-VMNR         PIC 9(07)         COMP-3.
      *            PRÜFZIFFER VM
               10  GENS-PC-VMNR-PZ      PIC X(01).
      *            VOR- UND ZUNAME VM
               10  GENS-PC-VM-VORNA     PIC X(26).
               10  GENS-PC-VM-ZUNA      PIC X(26).
      *            VM-ERFASSUNG
               10  GENS-PC-VMNR-E       PIC 9(07)         COMP-3.
      *            PRÜFZIFFER VM
               10  GENS-PC-VMPZ-E       PIC X(01).
      *            VOR- UND ZUNAME VM
               10  GENS-PC-VM-VORNA-E   PIC X(26).
               10  GENS-PC-VM-ZUNA-E    PIC X(26).
      *
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG
               10  GENS-PC-VB-GW        PIC 9(07)         COMP-3.
      *            PRÜFZIFFER
               10  GENS-PC-VB-GW-PZ     PIC X(01).
      *            GGF. NOTWENDIGER VB MIT GEWERBEBERECHTIGUNG-ERFASSUNG
               10  GENS-PC-VB-GW-E      PIC 9(07)       COMP-3.
      *            PRÜFZIFFER
               10  GENS-PC-VB-GW-PZ-E   PIC X(01).
      *
      *            DVAG-DIREKTION
               10  GENS-PC-DIR          PIC 9(03).

      *            VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  GENS-PC-KD-BEST-ID      PIC S9(18)V    COMP-3.
               10  GENS-PC-VN-AKD-T        PIC X(26).
               10  GENS-PC-VN-AKD-T-E      PIC X(26).
               10  GENS-PC-VN-VORNA        PIC X(26).
               10  GENS-PC-VN-VORNA-E      PIC X(26).
               10  GENS-PC-VN-ZUNA         PIC X(26).
               10  GENS-PC-VN-ZUNA-E       PIC X(26).
               10  GENS-PC-VN-SVNR         PIC S9(10)V    COMP-3.
               10  GENS-PC-VN-SVNR-E       PIC S9(10)V    COMP-3.
               10  GENS-PC-VN-STR          PIC X(26).
               10  GENS-PC-VN-STR-E        PIC X(26).
               10  GENS-PC-VN-PLZ          PIC X(05).
               10  GENS-PC-VN-PLZ-E        PIC X(05).
               10  GENS-PC-VN-ORT          PIC X(26).
               10  GENS-PC-VN-ORT-E        PIC X(26).
               10  GENS-PC-VN-LKZ          PIC X(03).
               10  GENS-PC-VN-LKZ-E        PIC X(03).
               10  GENS-PC-VN-KNR          PIC S9(15)V    COMP-3.
               10  GENS-PC-VN-KNR-E        PIC S9(15)V    COMP-3.
               10  GENS-PC-VN-BLZ          PIC S9(15)V    COMP-3.
               10  GENS-PC-VN-BLZ-E        PIC S9(15)V    COMP-3.
               10  GENS-PC-VN-KFZ-KZ       PIC X(10).
               10  GENS-PC-VN-KFZ-KZ-E     PIC X(10).
               10  GENS-PC-VN-FGST-NR      PIC X(26).
               10  GENS-PC-VN-FGST-NR-E    PIC X(26).
               10  GENS-PC-VN-KFZ-B-NR     PIC X(20).
               10  GENS-PC-VN-KFZ-B-NR-E   PIC X(20).
250713         10  GENS-PC-VN-IBAN         PIC X(34).
250713         10  GENS-PC-VN-IBAN-E       PIC X(34).

141211*        INFORMATIONEN IM RAHMEN GELDWÄSCHE, COMPLIANCE
141211         10  GENS-PC-VN-STAAANG      PIC X(3).
141211         10  GENS-PC-VN-STAAANG-E    PIC X(3).
141211         10  GENS-PC-VN-GEBDAT       PIC X(10).
141211         10  GENS-PC-VN-GEBDAT-E     PIC X(10).

      *        DATEN ZUR HAUPTVERSICHERUNG
               10  GENS-PC-BEGINN          PIC X(10).
               10  GENS-PC-BEGINN-E        PIC X(10).
               10  GENS-PC-ENDE            PIC X(10).
               10  GENS-PC-ENDE-E          PIC X(10).
               10  GENS-PC-ZAHLW           PIC 9(02).
               10  GENS-PC-ZAHLW-E         PIC 9(02).
               10  GENS-PC-WAEHRUNG        PIC X(03).
               10  GENS-PC-AN-ART          PIC X(01).
               10  GENS-PC-AN-ART-E        PIC X(01).
               10  GENS-PC-AN-DAT          PIC X(10).
               10  GENS-PC-AN-DAT-E        PIC X(10).
      *
      *        DVAG PRODUKTE MIT JEW. BETRÄGEN
               10 GENS-PC-VN-PRODKEY1     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P1       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P1-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P1      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P1        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY2     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P2       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P2-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P2      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P2        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY3     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P3       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P3-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P3      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P3        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY4     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P4       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P4-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P4      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P4        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY5     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P5       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P5-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P5      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P5        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY6     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P6       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P6-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P6      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P6        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY7     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P7       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P7-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P7      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P7        PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-PRODKEY8     PIC S9(04)       COMP-3.
               10 GENS-PC-VN-BTP-P8       PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-BTP-P8-E     PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-JNTP-P8      PIC S9(06)V9(02) COMP-3.
               10 GENS-PC-VN-EH-P8        PIC S9(06)V9(02) COMP-3.
      *
      *            VERSIONSDATEN
               10  GENS-PC-VERSION        PIC X(05).
      *
150512*            INTERNE ID-NUMMERN GENERALI
150512*            ANTRAGSNUMMER
150512         10  GENS-PC-GEN-ANT-NR     PIC S9(15)V      COMP-3.
150512*            BÜNDELANTRAGSNUMMER
150512         10  GENS-PC-GEN-B-ANT-NR   PIC S9(15)V      COMP-3.
150512*
      *            SONSTIGE DATEN
               10  GENS-PC-LAST-UPDATE    PIC X(26).
               10  GENS-PC-KENNUNG-A      PIC X(8).
      *
      *            BLACK-BOX (XML-DATEN)
               10 GENS-PC-XML.
                  15 GENS-PC-XML-TEXT     PIC X(30000).

