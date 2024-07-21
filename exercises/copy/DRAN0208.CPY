      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0208.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0208.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:51:30   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:27:06   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:49:36   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:01:40   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2008 09:38:44   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0208                                     *
      *    FUNKTION   : UPDATE PIN IN GENS-BESTAND                   *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 !NEU                        ! RAHNER   *
      * AAN01033!14.12.11!2.2 !DATEN GELDWÄSCHE ECT.      ! RAHNER   *
      * AAN01073!25.07.13!2.4 !SEPA                       ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0208                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0208.
           05 PI-DRAN0208-GENS-KEY.
               10 PI-DRAN0208-PGEB-GES        PIC X(2).
               10 PI-DRAN0208-GENS-HVANTNUM   PIC X(9).
               10 PI-DRAN0208-GENS-AVERM-E    PIC S9(9) COMP.
               10 PI-DRAN0208-GENS-AVERM-PZ-E PIC X(1).
          05 PI-DRAN0208-DATEN.
               10 PI-DRAN0208-GENS-PIN        PIC S9(9) COMP.
               10 PI-DRAN0208-GENS-VMNR-E     PIC S9(9) COMP.
               10 PI-DRAN0208-GENS-VMPZ-E     PIC X(1).
               10 PI-DRAN0208-GENS-VM-VORNA-E PIC X(26).
               10 PI-DRAN0208-GENS-VM-ZUNA-E  PIC X(26).
               10 PI-DRAN0208-GENS-VB-GW-E    PIC S9(9) COMP.
               10 PI-DRAN0208-GENS-VB-GW-PZ-E PIC X(1).
               10 PI-DRAN0208-GENS-VN-AKD-T-E PIC X(26).
               10 PI-DRAN0208-GENS-VN-VORNA-E PIC X(26).
               10 PI-DRAN0208-GENS-VN-ZUNA-E  PIC X(26).
               10 PI-DRAN0208-GENS-VN-SVNR-E  PIC S9(10) COMP-3.
               10 PI-DRAN0208-GENS-VN-STR-E   PIC X(26).
               10 PI-DRAN0208-GENS-VN-PLZ-E   PIC X(5).
               10 PI-DRAN0208-GENS-VN-ORT-E   PIC X(26).
               10 PI-DRAN0208-GENS-VN-LKZ-E   PIC X(3).
141211         10 PI-DRAN0208-GENS-VN-STAAANG-E PIC X(3).
141211         10 PI-DRAN0208-GENS-VN-GEBDAT-E  PIC X(10).
               10 PI-DRAN0208-GENS-BTP-P1-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P2-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P3-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P4-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P5-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P6-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P7-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-BTP-P8-E   PIC S9(06)V9(02) COMP-3.
               10 PI-DRAN0208-GENS-VN-KNR-E   PIC S9(15) COMP-3.
               10 PI-DRAN0208-GENS-VN-BLZ-E   PIC S9(15) COMP-3.
250713         10 PI-DRAN0208-GENS-VN-IBAN-E  PIC X(34).
               10 PI-DRAN0208-GENS-BEGINN-E   PIC X(10).
               10 PI-DRAN0208-GENS-ENDE-E     PIC X(10).
               10 PI-DRAN0208-GENS-ZAHLW-E    PIC X(1).
               10 PI-DRAN0208-GENS-KFZ-KZ-E   PIC X(10).
               10 PI-DRAN0208-GENS-FGST-NR-E  PIC X(26).
               10 PI-DRAN0208-GENS-KFZ-B-NR-E PIC X(20).
               10 PI-DRAN0208-GENS-AN-ART-E   PIC X(1).
               10 PI-DRAN0208-GENS-AN-DAT-E   PIC X(10).
      *
       01 PO-DRAN0208.
          05 PO-DRAN0208-RC                   PIC X(003).
             88 PO-DRAN0208-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0208-RC-NOK                 VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************

