      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0216.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0216.CPv  $
      *
      *   Rev 2.6   Jul 25 2013 10:52:08   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   Dec 14 2011 17:27:58   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.4   Dec 14 2011 16:50:26   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.3   Jun 15 2010 10:03:48   t13ae23
      *AAN00926: 2. Stufe Platzhalterersetzen elektr. Antrag Ges. 1
      *6 u. 26
      *
      *   Rev 2.2   Jun 30 2009 11:48:30   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26 
      *Teil - An
      *trag Batch 
      *
      *   Rev 2.1   Nov 24 2008 14:02:28   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 18 2008 10:20:38   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0216                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> REPLACE  <<<       TABELLE GENS TBAN0013              *
      *                                                              *
      *                                                              *
      *                                                              *
      *****************************************************************
      * LOGBUCH                                                       *
      *                                                               *
      * AUFTRAG ! DATUM  ! VER !AENDERUNG                 ! NAME      *
      * --------+--------+--------------------------------+-----------*
      * AAN00824!25.11.08!2.1  !NEU                       ! RAHNER
      * AAN00824!30.06.09!2.2  !EINFÜGEN PIN-PZ           ! RAHNER
      * AAN00926!15.06.10!2.3  !PLATZHALTER ERSETZEN TEIL2! RAHNER
      * AAN01073!25.07.13!2.6  !SEPA                      ! RAHNER
      *         !        !     !                          !
      *****************************************************************
       01  PI-DRAN0216.
           05 PI-DRAN0216-GENS-DATEN.
               10 PI-DRAN0216-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0216-GENS-HVANTNUM    PIC X(9).
               10 PI-DRAN0216-GENS-PIN         PIC S9(9) COMP.
300609         10 PI-DRAN0216-GENS-PIN-PZ      PIC X(2).
               10 PI-DRAN0216-GENS-AVERM       PIC S9(7).
               10 PI-DRAN0216-GENS-AVERM-PZ    PIC X(1).
               10 PI-DRAN0216-GENS-VN-VORNAME  PIC X(26).
               10 PI-DRAN0216-GENS-VN-ZUNAME   PIC X(26).
               10 PI-DRAN0216-GENS-VN-AKAD-T   PIC X(26).
               10 PI-DRAN0216-GENS-VN-SVNR     PIC S9(10) COMP-3.
               10 PI-DRAN0216-GENS-VN-PLZ      PIC X(5).
               10 PI-DRAN0216-GENS-VN-ORT      PIC X(26).
               10 PI-DRAN0216-GENS-VN-STRASSE  PIC X(26).
               10 PI-DRAN0216-GENS-VN-KNR      PIC S9(15) COMP-3.
               10 PI-DRAN0216-GENS-VN-BLZ      PIC S9(15) COMP-3.
250713         10 PI-DRAN0216-GENS-VN-IBAN     PIC X(34).
               10 PI-DRAN0216-GENS-KFZ-KZ      PIC X(10).
               10 PI-DRAN0216-GENS-FGST-NR     PIC X(26).
               10 PI-DRAN0216-GENS-KFZ-B-NR    PIC X(20).
150610         10 PI-DRAN0216-GENS-DIR         PIC 9(3).
      *
       01  PO-DRAN0216.
           05 PO-DRAN0216-RC                   PIC  X(003).
             88 PO-DRAN0216-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0216-RC-NOT-FOUND            VALUE 'NFD'.
      *
           05 PO-DRAN0216-GENS-DATEN.
               10 PO-DRAN0216-GENS-BLACKBOX    PIC X(30000).

