      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0203.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAN0203.CPv  $
      *
      *   Rev 2.9   Mar 31 2016 08:21:04   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.8   Mar 16 2016 12:05:26   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.7   Mar 09 2016 11:54:46   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.6   Jul 25 2013 10:51:08   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.5   Dec 14 2011 17:03:18   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.4   Jun 15 2010 10:03:32   t13ae23
      *AAN00926: 2. Stufe Platzhalterersetzen elektr. Antrag Ges. 1
      *6 u. 26
      *
      *   Rev 2.3   Jun 30 2009 11:48:24   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26
      *Teil - An
      *trag Batch
      *
      *   Rev 2.2   Nov 24 2008 14:20:42   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:51:42   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Jul 08 2008 10:34:24   i13ex34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRAN0203                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *    >>> REPLACE  <<<       TABELLE GENL TBAN0012               *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      * LOGBUCH                                                       *
      *                                                               *
      * AUFTRAG ! DATUM  ! VER !AENDERUNG                 ! NAME      *
      * --------+--------+--------------------------------+-----------*
      * AAN00824!24.11.08!2.1  !NEU                       ! RAHNER    *
      * AAN00824!30.06.09!2.2  !ANPASSUNGEN BLACKBOX      ! RAHNER    *
      * AAN00926!15.06.10!2.3  !PLATZHALTER ERSETZEN TEIL2! RAHNER    *
      * AAN01073!25.07.13!2.6  !SEPA                      ! RAHNER    *
      *         !        !     !                          !           *
      *****************************************************************
       01  PI-DRAN0203.
           05 PI-DRAN0203-GENLDATEN.
               10 PI-DRAN0203-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0203-PIN-NUM          PIC S9(09) COMP.
300609         10 PI-DRAN0203-GENL-PIN         PIC X(7) JUST.
               10 PI-DRAN0203-GENL-AVERM       PIC X(7).
               10 PI-DRAN0203-GENL-VN-VORNAME  PIC X(26).
               10 PI-DRAN0203-GENL-VN-ZUNAME   PIC X(26).
               10 PI-DRAN0203-GENL-VN-AKAD-T   PIC X(26).
150610         10 PI-DRAN0203-GENL-VN-SVNR     PIC X(4).
               10 PI-DRAN0203-GENL-VN-PLZ      PIC X(5).
               10 PI-DRAN0203-GENL-VN-ORT      PIC X(26).
               10 PI-DRAN0203-GENL-VN-STRASSE  PIC X(26).
               10 PI-DRAN0203-GENL-VN-KNR      PIC X(15).
               10 PI-DRAN0203-GENL-VN-BLZ      PIC X(15).
250713         10 PI-DRAN0203-GENL-VN-IBAN     PIC X(34).
150610         10 PI-DRAN0203-GENL-DIR         PIC 9(3).
      *
       01  PO-DRAN0203.
           05 PO-DRAN0203-RC                   PIC  X(003).
             88 PO-DRAN0203-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0203-RC-NOT-FOUND            VALUE 'NFD'.
      *
           05 PO-DRAN0203-GENL-DATEN.
               10 PO-DRAN0203-GENL-BLACKBOX    PIC X(30000).


