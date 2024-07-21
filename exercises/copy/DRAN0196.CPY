      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0196.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0196.CPv  $
      *
      *   Rev 2.6   Jul 25 2013 10:50:36   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   Dec 14 2011 17:02:36   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.4   May 27 2009 16:35:14   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26 
      *- Beitrag
      *sfeld öffnen -
      *
      *   Rev 2.2   Nov 24 2008 14:20:00   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:51:02   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 08 2008 10:33:56   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0196  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00824!23.05.09!2.2 ! BEITRAGSFELD ÖFFNEN       ! RAHNER   *
      * AAN01033!14.12.11!2.5 ! DATEN GELDWÄSCHE ECT.     ! RAHNER   *
      * AAN01073!25.07.13!2.6 ! SEPA                      ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0196                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE GENL TBAN0012              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0196.
           05 PI-DRAN0196-GENL-KEY.
               10 PI-DRAN0196-PGEB-GES          PIC S9(4) COMP.
               10 PI-DRAN0196-GENL-HVANTNUM     PIC X(9).
               10 PI-DRAN0196-GENL-VDABVERM     PIC S9(9) COMP.
      *
           05 PI-DRAN0196-GENL-DATEN.
               10 PI-DRAN0196-GENL-PIN          PIC S9(9) COMP.
               10 PI-DRAN0196-GENL-KENNUNG-A    PIC X(8).
      *
      *           AVERM-ERFASSUNG
               10 PI-DRAN0196-GENL-AVERM-E      PIC 9(07) COMP-3.
      *           PRÜFZIFFER AVERM
               10 PI-DRAN0196-GENL-AVERMPZ-E    PIC X(01).
      *           VM-ERFASSUNG
               10 PI-DRAN0196-GENL-VMNR-E       PIC 9(07) COMP-3.
      *           PRÜFZIFFER VM
               10 PI-DRAN0196-GENL-VMPZ-E       PIC X(01).
      *           VOR- UND ZUNAME VM
               10 PI-DRAN0196-GENL-VM-VORNA-E   PIC X(26).
               10 PI-DRAN0196-GENL-VM-ZUNA-E    PIC X(26).
      *           GGF. NOTWENDIGER VB MIT GE
               10 PI-DRAN0196-GENL-VB-GW-E      PIC 9(07) COMP-3.
      *           PRÜFZIFFER
               10 PI-DRAN0196-GENL-VB-GW-PZ-E   PIC X(01).
      *           VERSICHERUNGSNEHMER/ANTRAG
               10 PI-DRAN0196-GENL-VN-AKD-T-E   PIC X(26).
               10 PI-DRAN0196-GENL-VN-VORNA-E   PIC X(26).
               10 PI-DRAN0196-GENL-VN-ZUNA-E    PIC X(26).
               10 PI-DRAN0196-GENL-VN-SVNR-E    PIC S9(10)V  COMP-3.
               10 PI-DRAN0196-GENL-VN-STR-E     PIC X(26).
               10 PI-DRAN0196-GENL-VN-PLZ-E     PIC X(05).
               10 PI-DRAN0196-GENL-VN-ORT-E     PIC X(26).
               10 PI-DRAN0196-GENL-VN-LKZ-E     PIC X(03).
               10 PI-DRAN0196-GENL-VN-KNR-E     PIC S9(15)V  COMP-3.
               10 PI-DRAN0196-GENL-VN-BLZ-E     PIC S9(15)V  COMP-3.
250713         10 PI-DRAN0196-GENL-VN-IBAN-E    PIC X(34).
      *           DATEN IM RAHMEN GELDWÄSCHE/COMPILANCE
141211         10 PI-DRAN0196-GENL-VN-STAA-E    PIC X(03).
141211         10 PI-DRAN0196-GENL-VN-BGRP-E    PIC X(40).
141211         10 PI-DRAN0196-GENL-VN-BRAN-E    PIC X(40).
141211         10 PI-DRAN0196-GENL-VN-PEP-E     PIC X(01).

      *            DATEN ZUR HAUPTVERSICHERU
               10 PI-DRAN0196-GENL-BEGINN-E     PIC X(10).
               10 PI-DRAN0196-GENL-PZ-DAUER-E   PIC S9(04)   COMP-3.
               10 PI-DRAN0196-GENL-SUMME-E      PIC S9(09)V  COMP-3.
               10 PI-DRAN0196-GENL-JAHR-NTP-E   PIC S9(11)V9(02) COMP-3.
               10 PI-DRAN0196-GENL-PRSUM-E      PIC S9(11)V      COMP-3.
230509         10 PI-DRAN0196-GENL-BEITRAG-E    PIC S9(11)V9(02) COMP-3.
               10 PI-DRAN0196-GENL-ZAHLW-E      PIC 9(02).
               10 PI-DRAN0196-GENL-AN-ART-E     PIC X(01).
               10 PI-DRAN0196-GENL-AN-DAT-E     PIC X(10).
      *           DVAG-ABSCHLUSSEINHEITEN
               10 PI-DRAN0196-GENL-VD-EINH-E    PIC S9(9)V9(02) COMP-3.
      *
       01  PO-DRAN0196.
           05 PO-DRAN0196-RC                    PIC  X(003).
             88 PO-DRAN0196-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0196-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0196-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
