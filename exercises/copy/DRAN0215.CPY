      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0215.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0215.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:52:02   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:27:50   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:50:18   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:02:22   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 17 2008 14:44:20   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0215                                     *
      *    FUNKTION   : ERM. GENS-BESTAND ÜBER GES, PIN              *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 !NEU                         !RAHNER   *
      * AAN01033!14.12.11!2.2 !DATEN GELDWÄSCHE ECT.       !RAHNER   *
      * AAN01073!25.07.13!2.4 !SEPA                        !RAHNER   *
      *         !        !    !                            !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0215                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0215.
           05 PI-DRAN0215-GENS-KEY.
               10 PI-DRAN0215-PGEB-GES         PIC  X(2).
               10 PI-DRAN0215-GENS-PIN         PIC  S9(9) COMP.
               10 PI-DRAN0215-GENS-AVERM       PIC  S9(9) COMP.
      *
       01 PO-DRAN0215.
          05 PO-DRAN0215-RC                    PIC  X(003).
             88 PO-DRAN0215-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0215-RC-NOK                  VALUE 'NOK'.
          05 PO-DRAN0215-DATEN.
             10 PO-DRAN0215-BAN0038.
                15 PO-DRAN0215-VBNR1O       PIC X(7).
                15 PO-DRAN0215-VBNR-PZ1O    PIC X(1).
      *
                15 PO-DRAN0215-VN-AKD-T1O   PIC X(26).
                15 PO-DRAN0215-VN-VORNA1O   PIC X(26).
                15 PO-DRAN0215-VN-ZUNA1O    PIC X(26).
                15 PO-DRAN0215-VN-STR1O     PIC X(26).
                15 PO-DRAN0215-VN-PLZ1O     PIC X(5).
                15 PO-DRAN0215-VN-ORT1O     PIC X(21).
                15 PO-DRAN0215-VN-LKZ1O     PIC X(3).
                15 PO-DRAN0215-VN-KNR1O     PIC X(15).
                15 PO-DRAN0215-VN-BLZ1O     PIC X(15).
250713          15 PO-DRAN0215-VN-IBAN1O    PIC X(34).
                15 PO-DRAN0215-VN-SVNR1O    PIC X(10).
141211          15 PO-DRAN0215-VN-STAAANG1O PIC X(10).
141211          15 PO-DRAN0215-VN-GEBDAT1O  PIC X(10).
      *
                15 PO-DRAN0215-PROD-TAB     OCCURS 8
                                       INDEXED BY I-DRAN0215-PROD,
                                                  I-DRAN0215-PROD-2.
                   20 PO-DRAN0215-PRODKEYO      PIC X(2).
                   20 PO-DRAN0215-BTP-PRODKEYO  PIC X(8).
                   20 PO-DRAN0215-BTP-PRODKEYO-E  PIC X(8).
                   20 PO-DRAN0215-JNTP-PRODKEYO PIC S9(6)V9(2) COMP-3.
                   20 PO-DRAN0215-EH-PRODKEYO   PIC S9(9)V9(2) COMP-3.
      *
                15 PO-DRAN0215-BEGINN1O     PIC X(6).
                15 PO-DRAN0215-ENDE1O       PIC X(6).
                15 PO-DRAN0215-ZAHLW1O      PIC X(2) .
                15 PO-DRAN0215-WAEHRUNG1O   PIC X(3).
                15 PO-DRAN0215-EINH1O       PIC X(7).
      *
                15 PO-DRAN0215-KFZ-KZ1O     PIC X(10).
                15 PO-DRAN0215-FGST-NR1O    PIC X(26).
                15 PO-DRAN0215-KFZ-B-NR1O   PIC X(20).
      *
                15 PO-DRAN0215-AN-ART1O     PIC X(1).
                15 PO-DRAN0215-AN-DAT1O     PIC X(6).
      *
             10 PO-DRAN0215-SONSTIGE.
                15 PO-DRAN0215-GENS-PIN     PIC S9(9) COMP.

      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0214.
      *
      ******** ENDE COPY-MEMBER **************************************
