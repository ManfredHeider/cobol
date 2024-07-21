      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0177.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0177.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:50:16   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:02:14   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.2   Nov 24 2008 14:19:30   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:50:34   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   21 Nov 2006 09:46:42   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0177                                     *
      *    FUNKTION   : ERM. GENL-BESTAND ÜBER GES, PIN              *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN00824!24.11.08!2.1 !NEU                         !HEIDER   *
      * AAN01033!14.12.11!2.3 ! DATEN GELDWÄSCHE ECT.      ! RAHNER  *
      * AAN01073!25.07.13!2.4 ! SEPA                       ! RAHNER  *
      *         !        !    !                            !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0177                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0177.
           05 PI-DRAN0177-GENL-KEY.
               10 PI-DRAN0177-PGEB-GES         PIC  X(2).
               10 PI-DRAN0177-GENL-PIN         PIC  S9(9) COMP.
               10 PI-DRAN0177-GENL-AVERM       PIC  S9(9) COMP.
      *
       01 PO-DRAN0177.
          05 PO-DRAN0177-RC                    PIC  X(003).
             88 PO-DRAN0177-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0177-RC-NOK                  VALUE 'NOK'.
          05 PO-DRAN0177-DATEN.
             10 PO-DRAN0177-BAN0039O.
                15 PO-DRAN0177-VBNR1O       PIC X(7).
                15 PO-DRAN0177-VBNR-PZ1O    PIC X(1).
                15 PO-DRAN0177-VN-AKD-T1O   PIC X(26).
                15 PO-DRAN0177-VN-VORNA1O   PIC X(26).
                15 PO-DRAN0177-VN-ZUNA1O    PIC X(26).
                15 PO-DRAN0177-VN-GBDAT1O   PIC X(6).
                15 PO-DRAN0177-VN-STR1O     PIC X(26).
                15 PO-DRAN0177-VN-PLZ1O     PIC X(5).
                15 PO-DRAN0177-VN-ORT1O     PIC X(21).
                15 PO-DRAN0177-VN-LKZ1O     PIC X(3).
                15 PO-DRAN0177-VN-SVNR1O    PIC X(10).
141211          15 PO-DRAN0177-VN-STAA1O    PIC X(3).
141211          15 PO-DRAN0177-VN-BGRP1O    PIC X(40).
141211          15 PO-DRAN0177-VN-BRAN1O    PIC X(40).
141211          15 PO-DRAN0177-VN-PEP1O     PIC X(1).
                15 PO-DRAN0177-VN-KNR1O     PIC X(15).
                15 PO-DRAN0177-VN-BLZ1O     PIC X(15).
250713          15 PO-DRAN0177-VN-IBAN1O    PIC X(34).
                15 PO-DRAN0177-VD-PRODKEY1O PIC X(4).
                15 PO-DRAN0177-VD-TARIF1O   PIC X(10).
                15 PO-DRAN0177-VD-KZ-ZUW1O  PIC X(1).
                15 PO-DRAN0177-VD-KZ-BUZR1O PIC X(1).
                15 PO-DRAN0177-VD-KZ-BUZB1O PIC X(1).
                15 PO-DRAN0177-VD-KZ-UTZ1O  PIC X(1).
                15 PO-DRAN0177-VD-KZ-PO1O   PIC X(1).
                15 PO-DRAN0177-BEGINN1O     PIC X(6).
                15 PO-DRAN0177-PZ-DAUER1O   PIC X(2).
                15 PO-DRAN0177-SUMME1O      PIC X(9).
                15 PO-DRAN0177-BEITRAG1O    PIC X(11).
                15 PO-DRAN0177-JAHR-NTP1O   PIC X(11).
                15 PO-DRAN0177-PRSUM1O      PIC X(11).
                15 PO-DRAN0177-ZAHLW1O      PIC X(2).
                15 PO-DRAN0177-WAEHRUNG1O   PIC X(3).
                15 PO-DRAN0177-VD-EINH1O    PIC X(7).
                15 PO-DRAN0177-AN-ART1O     PIC X(1).
                15 PO-DRAN0177-AN-DAT1O     PIC X(6).
      *
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0202.
      *
      ******** ENDE COPY-MEMBER **************************************
