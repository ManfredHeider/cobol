      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0193.CPY  $
      *    $Revision:   1.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0193.CPv  $
      *
      *   Rev 1.6   Jul 25 2013 10:50:26   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 1.5   Dec 14 2011 17:02:20   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 1.4   May 27 2009 16:35:10   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26 
      *- Beitrag
      *sfeld öffnen -
      *
      *   Rev 1.2   Nov 24 2008 14:19:42   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 1.1   Nov 24 2008 11:50:46   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 1.0   Jul 03 2008 10:14:16   i13ex34
      *Initial revision.
      *
      *   Rev 2.0   03 Jul 2008 11:24:16   t13ae23
      *Archiverstellung
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0193                                     *
      *    FUNKTION   : UPDATE PIN IN GENL-BESTAND                   *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 !NEU                        ! RAHNER   *
      * AAN00824!23.05.09!2.2 !BEITRAGSFELD ÖFFNEN        ! RAHNER   *
      * AAN01033!14.12.11!2.3 !DATEN GELDWÄSCHE ECT       ! RAHNER   *
      * AAN01073!25.07.13!2.5 !SEPA                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0193                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0193.
           05 PI-DRAN0193-GENL-KEY.
               10 PI-DRAN0193-PGEB-GES        PIC X(2).
               10 PI-DRAN0193-GENL-HVANTNUM   PIC X(9).
               10 PI-DRAN0193-GENL-AVERM-E    PIC S9(9) COMP.
               10 PI-DRAN0193-GENL-AVERM-PZ-E PIC X(1).
          05 PI-DRAN0193-DATEN.
               10 PI-DRAN0193-GENL-PIN        PIC S9(9) COMP.
               10 PI-DRAN0193-GENL-VMNR-E     PIC S9(9) COMP.
               10 PI-DRAN0193-GENL-VMPZ-E     PIC X(1).
               10 PI-DRAN0193-GENL-VM-VORNA-E PIC X(26).
               10 PI-DRAN0193-GENL-VM-ZUNA-E  PIC X(26).
               10 PI-DRAN0193-GENL-VB-GW-E    PIC S9(9) COMP.
               10 PI-DRAN0193-GENL-VB-GW-PZ-E PIC X(1).
               10 PI-DRAN0193-GENL-VN-AKD-T-E PIC X(26).
               10 PI-DRAN0193-GENL-VN-VORNA-E PIC X(26).
               10 PI-DRAN0193-GENL-VN-ZUNA-E  PIC X(26).
               10 PI-DRAN0193-GENL-VN-SVNR-E  PIC S9(10) COMP-3.
               10 PI-DRAN0193-GENL-VN-STR-E   PIC X(26).
               10 PI-DRAN0193-GENL-VN-PLZ-E   PIC X(5).
               10 PI-DRAN0193-GENL-VN-ORT-E   PIC X(26).
               10 PI-DRAN0193-GENL-VN-LKZ-E   PIC X(3).
               10 PI-DRAN0193-GENL-VN-KNR-E   PIC S9(15) COMP-3.
               10 PI-DRAN0193-GENL-VN-BLZ-E   PIC S9(15) COMP-3.
250713         10 PI-DRAN0193-GENL-VN-IBAN-E  PIC X(34).
141211         10 PI-DRAN0193-GENL-VN-STAA-E  PIC X(3).
141211         10 PI-DRAN0193-GENL-VN-BGRP-E  PIC X(40).
141211         10 PI-DRAN0193-GENL-VN-BRAN-E  PIC X(40).
141211         10 PI-DRAN0193-GENL-VN-PEP-E   PIC X(1).
               10 PI-DRAN0193-GENL-BEGINN-E   PIC X(10).
               10 PI-DRAN0193-GENL-PZ-DAUER-E PIC X(2).
               10 PI-DRAN0193-GENL-SUMME-E    PIC S9(9)V9(2) COMP.
               10 PI-DRAN0193-GENL-JAHR-NTP-E PIC S9(9)V9(2) COMP.
               10 PI-DRAN0193-GENL-PRSUM-E    PIC S9(9)      COMP.
230509         10 PI-DRAN0193-GENL-BEITRAG-E  PIC S9(11)V9(02) COMP-3.
               10 PI-DRAN0193-GENL-ZAHLW-E    PIC X(1).
               10 PI-DRAN0193-GENL-AN-ART-E   PIC X(1).
               10 PI-DRAN0193-GENL-AN-DAT-E   PIC X(10).
               10 PI-DRAN0193-GENL-VD-EINH-E  PIC S9(6)V9(2).
      *
       01 PO-DRAN0193.
          05 PO-DRAN0193-RC                   PIC X(003).
             88 PO-DRAN0193-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0193-RC-NOK                 VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************
