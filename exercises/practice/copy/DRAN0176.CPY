      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0176.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0176.CPv  $
      *
      *   Rev 2.6   Jul 25 2013 10:50:10   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   May 15 2012 10:38:36   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.4   Dec 14 2011 17:02:06   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.3   Feb 10 2009 14:39:28   t13ae23
      *FAN00873: Fehler bei Verarbeitung KontoNr > 10 Stellen Ges. 
      *16 u. 26
      *
      *   Rev 2.2   Nov 24 2008 14:19:24   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:50:28   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 03 2008 10:48:40   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0176                                     *
      *    FUNKTION   : ERM. GENL-BESTAND ÜBER GES, VORSCHLAGSNUMMER *
      *                 -> AUFBEREITET FÜR GENERALI LEBEN (BAN0039)  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 !NEU                         !RAHNER   *
      * FAN00873!10.02.09!2.2 !TARIFBEZ AUF LÄNGE 15 ERW.  !RAHNER   *
      * AAN01033!14.12.11!2.4 !DATEN GELDWÄSCHE ECT.       !RAHNER   *
      * AAN01051!15.05.12!2.5 !INTERNE ID-NUMMERN GENERALI !RAHNER   *
      * AAN01073!25.07.13!2.6 !SEPA                        !RAHNER   *
      *         !        !    !                            !         *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0176                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0176.
           05 PI-DRAN0176-GENL-KEY.
               10 PI-DRAN0176-PGEB-GES        PIC  X(2).
               10 PI-DRAN0176-GENL-HVANTNUM   PIC  X(9).
      *           OPTIONAL
               10 PI-DRAN0176-GENL-AVERM      PIC  X(7).
               10 PI-DRAN0176-GENL-AVERM-PZ   PIC  X(1).
      *
       01 PO-DRAN0176.
          05 PO-DRAN0176-RC                   PIC  X(003).
             88 PO-DRAN0176-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0176-RC-NOK                 VALUE 'NOK'.
             88 PO-DRAN0176-RC-KEY-NOT-UNIQUE      VALUE 'KNU'.
          05 PO-DRAN0176-DATEN.
             10 PO-DRAN0176-BAN0039.
                15 PO-DRAN0176-VBNR1O       PIC X(7).
                15 PO-DRAN0176-VBNR-PZ1O    PIC X(1).
      *
                15 PO-DRAN0176-VN-AKD-T1O   PIC X(26).
                15 PO-DRAN0176-VN-VORNA1O   PIC X(26).
                15 PO-DRAN0176-VN-ZUNA1O    PIC X(26).
                15 PO-DRAN0176-VN-GBDAT1O   PIC X(6).
                15 PO-DRAN0176-VN-STR1O     PIC X(26).
                15 PO-DRAN0176-VN-PLZ1O     PIC X(5).
                15 PO-DRAN0176-VN-ORT1O     PIC X(21).
                15 PO-DRAN0176-VN-LKZ1O     PIC X(3).
                15 PO-DRAN0176-VN-KNR1O     PIC X(15).
                15 PO-DRAN0176-VN-BLZ1O     PIC X(15).
250713          15 PO-DRAN0176-VN-IBAN1O    PIC X(34).
                15 PO-DRAN0176-VN-SVNR1O    PIC X(10).
141211          15 PO-DRAN0176-VN-STAA1O    PIC X(3).
141211          15 PO-DRAN0176-VN-BGRP1O    PIC X(40).
141211          15 PO-DRAN0176-VN-BRAN1O    PIC X(40).
141211          15 PO-DRAN0176-VN-PEP1O     PIC X(3).
                15 PO-DRAN0176-VD-PRODKEY1O PIC X(4) .
100209*         15 PO-DRAN0176-VD-TARIF1O   PIC X(10).
100209          15 PO-DRAN0176-VD-TARIF1O   PIC X(15).
                15 PO-DRAN0176-VD-KZ-ZUW1O  PIC X(1).
                15 PO-DRAN0176-VD-KZ-BUZR1O PIC X(1).
                15 PO-DRAN0176-VD-KZ-BUZB1O PIC X(1).
                15 PO-DRAN0176-VD-KZ-UTZ1O  PIC X(1).
                15 PO-DRAN0176-VD-KZ-PO1O   PIC X(1).
                15 PO-DRAN0176-BEGINN1O     PIC X(6).
                15 PO-DRAN0176-PZ-DAUER1O   PIC X(2) .
                15 PO-DRAN0176-SUMME1O      PIC X(9) .
                15 PO-DRAN0176-BEITRAG1O    PIC X(9).
                15 PO-DRAN0176-JAHR-NTP1O   PIC X(9).
                15 PO-DRAN0176-PRSUM1O      PIC X(9).
                15 PO-DRAN0176-ZAHLW1O      PIC X(2) .
                15 PO-DRAN0176-WAEHRUNG1O   PIC X(3).
                15 PO-DRAN0176-VD-EINH1O    PIC X(7).
                15 PO-DRAN0176-AN-ART1O     PIC X(1).
                15 PO-DRAN0176-AN-DAT1O     PIC X(6).

             10 PO-DRAN0176-SONSTIGE.
                15 PO-DRAN0176-GENL-PIN     PIC S9(9) COMP.

      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0202.
      *
      ******** ENDE COPY-MEMBER **************************************
