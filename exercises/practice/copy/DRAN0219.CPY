      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0219.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0219.CPv  $
      *
      *   Rev 2.6   Jul 25 2013 10:52:24   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   Dec 14 2011 17:28:20   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.4   Dec 14 2011 16:50:48   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.3   Jun 15 2010 10:03:54   t13ae23
      *AAN00926: 2. Stufe Platzhalterersetzen elektr. Antrag Ges. 1
      *6 u. 26
      *
      *   Rev 2.2   Jun 30 2009 11:48:36   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26 
      *Teil - An
      *trag Batch 
      *
      *   Rev 2.1   Nov 24 2008 14:02:48   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 18 2008 10:20:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0219                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> REPLACE  <<<       TABELLE GENS TBAN0013              *
      *                                                              *
      *                                                               *
      *                                                               *
      *****************************************************************
      * LOGBUCH                                                       *
      *                                                               *
      * AUFTRAG ! DATUM  ! VER !AENDERUNG                 ! NAME      *
      * --------+--------+--------------------------------+-----------*
      * AAN00824!25.11.08!2.1  !NEU                       ! RAHNER
      * AAN00824!30.06.09!2.2  !NEUE VORGABEN BLACKBOX    ! RAHNER
      * AAN00926!15.06.10!2.3  !PLATZHALTER ERSETZEN TEIL2! RAHNER
      * AAN01073!25.07.13!2.6  !SEPA                      ! RAHNER
      *         !        !     !                          !
      *         !        !     !                          !
      *****************************************************************
       01  PI-DRAN0219.
           05 PI-DRAN0219-GENS-DATEN.
               10 PI-DRAN0219-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0219-GENS-HVANTNUM    PIC X(9).
300609         10 PI-DRAN0219-GENS-PIN-NUM     PIC S9(9) COMP.
300609*        10 PI-DRAN0219-GENS-PIN         PIC S9(9) COMP.
300609         10 PI-DRAN0219-GENS-PIN         PIC X(9).
300609         10 PI-DRAN0219-GENS-PIN-X REDEFINES
300609            PI-DRAN0219-GENS-PIN.
300609            15 PI-DRAN0219-PIN           PIC X(7).
300609            15 PI-DRAN0219-PIN-PZ        PIC X(2).
300609*        10 PI-DRAN0219-GENS-AVERM       PIC S9(7).
300609*        10 PI-DRAN0219-GENS-AVERM-PZ    PIC X(1).
300609         10 PI-DRAN0219-GENS-AVERM       PIC X(8).
300609         10 PI-DRAN0219-GENS-AVERM-X REDEFINES
300609            PI-DRAN0219-GENS-AVERM.
300609            15 PI-DRAN0219-AVERM         PIC X(7).
300609            15 PI-DRAN0219-AVERM-PZ      PIC X(1).
               10 PI-DRAN0219-GENS-VN-VORNAME  PIC X(26).
               10 PI-DRAN0219-GENS-VN-ZUNAME   PIC X(26).
               10 PI-DRAN0219-GENS-VN-AKAD-T   PIC X(26).
150610*        10 PI-DRAN0219-GENS-VN-SVNR     PIC X(10).
150610         10 PI-DRAN0219-GENS-VN-SVNR     PIC X(4).
               10 PI-DRAN0219-GENS-VN-PLZ      PIC X(5).
               10 PI-DRAN0219-GENS-VN-ORT      PIC X(26).
               10 PI-DRAN0219-GENS-VN-STRASSE  PIC X(26).
               10 PI-DRAN0219-GENS-VN-KNR      PIC X(15).
               10 PI-DRAN0219-GENS-VN-BLZ      PIC X(15).
150610*        10 PI-DRAN0219-GENS-VN-KFZ-KZ   PIC X(10).
150610         10 PI-DRAN0219-GENS-VN-KFZ-KZ   PIC X(9).
150610*        10 PI-DRAN0219-GENS-VN-FGST-NR  PIC X(26).
150610         10 PI-DRAN0219-GENS-VN-FGST-NR  PIC X(20).
150610*        10 PI-DRAN0219-GENS-VN-KFZ-B-NR PIC X(20).
150610         10 PI-DRAN0219-GENS-VN-KFZ-B-NR PIC X(11).
150610         10 PI-DRAN0219-GENS-DIR         PIC 9(3).
250713         10 PI-DRAN0219-GENS-VN-IBAN     PIC X(34).
      *
       01  PO-DRAN0219.
           05 PO-DRAN0219-RC                   PIC  X(003).
             88 PO-DRAN0219-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0219-RC-NOT-FOUND            VALUE 'NFD'.
      *
           05 PO-DRAN0219-GENS-DATEN.
               10 PO-DRAN0219-GENS-BLACKBOX    PIC X(30000).
