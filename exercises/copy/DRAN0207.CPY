      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0207.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0207.CPv  $
      *
      *   Rev 2.6   Jan 15 2019 12:58:54   t13ae23
      *AAN01410: Darstellung einer gepackten Zahl enthält Unterstri
      *ch "_" hier : Ges. 16 und 26
      *
      *   Rev 2.5   Jul 25 2013 10:51:24   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.4   May 15 2012 10:39:28   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.3   Dec 14 2011 17:26:58   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:49:30   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:01:32   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2008 09:38:38   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0207                                     *
      *    FUNKTION   : ERM. GENS-BESTAND ÜBER GES, VORSCHLAGSNUMMER *
      *                 -> AUFBEREITET FÜR GENERALI SACH  (BAN0038)  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 !NEU                         !RAHNER   *
      * AAN01033!14.12.11!2.2 ! DATEN GELDWÄSCHE ECT.      !RAHNER   *
      * AAN01051!15.05.12!2.4 !INTERNE ID-NUMMERN GENERALI !RAHNER   *
      * AAN01073!25.07.13!2.5 !SEPA                        !RAHNER   *
      * AAN01410!15.01.19!2.6 !NUR PIC X IN -BAN0038       !RAHNER   *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *         !        !    !                            !         *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0207                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0207.
           05 PI-DRAN0207-GENS-KEY.
               10 PI-DRAN0207-PGEB-GES        PIC  X(2).
               10 PI-DRAN0207-GENS-HVANTNUM   PIC  X(9).
      *           OPTIONAL
               10 PI-DRAN0207-GENS-AVERM      PIC  X(7).
               10 PI-DRAN0207-GENS-AVERM-PZ   PIC  X(1).
      *
       01 PO-DRAN0207.
          05 PO-DRAN0207-RC                   PIC  X(003).
             88 PO-DRAN0207-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0207-RC-NOK                 VALUE 'NOK'.
             88 PO-DRAN0207-RC-KEY-NOT-UNIQUE      VALUE 'KNU'.
          05 PO-DRAN0207-DATEN.
             10 PO-DRAN0207-BAN0038.
                15 PO-DRAN0207-VBNR1O       PIC X(7).
                15 PO-DRAN0207-VBNR-PZ1O    PIC X(1).
                15 PO-DRAN0207-VN-AKD-T1O   PIC X(26).
                15 PO-DRAN0207-VN-VORNA1O   PIC X(26).
                15 PO-DRAN0207-VN-ZUNA1O    PIC X(26).
                15 PO-DRAN0207-VN-STR1O     PIC X(26).
                15 PO-DRAN0207-VN-PLZ1O     PIC X(5).
                15 PO-DRAN0207-VN-ORT1O     PIC X(21).
                15 PO-DRAN0207-VN-LKZ1O     PIC X(3).
                15 PO-DRAN0207-VN-KNR1O     PIC X(15).
                15 PO-DRAN0207-VN-BLZ1O     PIC X(15).
250713          15 PO-DRAN0207-VN-IBAN1O    PIC X(34).
                15 PO-DRAN0207-VN-SVNR1O    PIC X(10).
141211          15 PO-DRAN0207-VN-STAAANG1O PIC X(3).
141211          15 PO-DRAN0207-VN-GEBDAT1O  PIC X(10).
                15 PO-DRAN0207-BEGINN1O     PIC X(6).
                15 PO-DRAN0207-ENDE1O       PIC X(6).
                15 PO-DRAN0207-ZAHLW1O      PIC X(2) .
                15 PO-DRAN0207-WAEHRUNG1O   PIC X(3).
                15 PO-DRAN0207-VN-EINH1O    PIC X(7).
                15 PO-DRAN0207-KFZ-KZ1O     PIC X(10).
                15 PO-DRAN0207-FGST-NR1O    PIC X(26).
                15 PO-DRAN0207-KFZ-B-NR1O   PIC X(20).
                15 PO-DRAN0207-AN-ART1O     PIC X(1).
                15 PO-DRAN0207-AN-DAT1O     PIC X(6).
      *
150119*         15 PO-DRAN0207-PROD-TAB     OCCURS 8
150119*                                INDEXED BY I-DRAN0207-PROD,
150119*                                           I-DRAN0207-PROD-2.
150119*            20 PO-DRAN0207-PRODKEYO      PIC X(2).
150119*            20 PO-DRAN0207-BTP-PRODKEYO  PIC X(8).
150119*            20 PO-DRAN0207-BTP-PRODKEYO-E  PIC X(8).
150119*            20 PO-DRAN0207-JNTP-PRODKEYO PIC S9(9)V9(2) COMP-3.
150119*            20 PO-DRAN0207-EH-PRODKEYO   PIC S9(9)V9(2) COMP-3.
      *
150119       10 PO-DRAN0207-PROD-TABELLE.
150119          15 PO-DRAN0207-PROD-TAB     OCCURS 8
150119                                 INDEXED BY I-DRAN0207-PROD,
150119                                            I-DRAN0207-PROD-2.
                   20 PO-DRAN0207-PRODKEYO        PIC X(2).
                   20 PO-DRAN0207-BTP-PRODKEYO    PIC X(8).
                   20 PO-DRAN0207-BTP-PRODKEYO-E  PIC X(8).
150119             20 PO-DRAN0207-JNTP-PRODKEYO   PIC S9(9)V9(2) COMP-3.
150119             20 PO-DRAN0207-EH-PRODKEYO     PIC S9(9)V9(2) COMP-3.

             10 PO-DRAN0207-SONSTIGE.
                15 PO-DRAN0207-GENS-PIN     PIC S9(9) COMP.

      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0214.
      *
      ******** ENDE COPY-MEMBER **************************************


