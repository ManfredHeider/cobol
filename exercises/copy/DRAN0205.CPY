      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0205.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRAN0205.CPv  $
      *
      *   Rev 2.10   Sep 30 2019 14:12:16   t13ae23
      *AAN01438: DVAG-Wien: Generali (16 und 26) teilmasch. Antrag 
      *- Antragsdatensatzerweiterung (Release 8.16) - Neuer Platzha
      *
      *
      *   Rev 2.9   Aug 27 2019 10:55:06   t13ae23
      *AAN01438: DVAG-Wien: Generali (16 und 26) teilmasch. Antrag
      *- Antragsdatensatzerweiterung (Release 8.16) - Neuer Platzha
      *
      *
      *   Rev 2.8   Mar 31 2016 08:21:12   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.7   Mar 16 2016 12:05:34   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.6   Mar 09 2016 11:54:54   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.5   Jul 25 2013 10:51:20   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.4   Dec 14 2011 17:03:34   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 26
      *
      *   Rev 2.3   Jun 15 2010 10:03:40   t13ae23
      *AAN00926: 2. Stufe Platzhalterersetzen elektr. Antrag Ges. 1
      *6 u. 26
      *
      *   Rev 2.2   Nov 24 2008 14:20:54   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.1   Nov 24 2008 11:51:54   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 26  Teil - Ant
      *rag Dialog-
      *
      *   Rev 2.0   Sep 04 2008 09:54:20   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0205                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> REPLACE  <<<       TABELLE GENL TBAN0012              *
      *                                                              *
      *                                                               *
      *                                                               *
      *****************************************************************
      * LOGBUCH                                                       *
      *                                                               *
      * AUFTRAG ! DATUM  ! VER !AENDERUNG                 ! NAME      *
      * --------+--------+--------------------------------+-----------*
      * AAN00824!24.11.08!2.1  !NEU                       ! RAHNER    *
      * AAN00926!16.06.10!2.2  !PLATZHALTER ERSETZEN TEIL2! RAHNER    *
      * AAN01073!25.07.13!2.5  !SEPA                      ! RAHNER    *
      * AAN01230!08.03.16!2.6  !PDF                       ! HEIDER    *
      * AAN01438!30.09.19!2.91 !VERSION                   ! RAHNER    *
      *         !        !     !                          !           *
      *         !        !     !                          !           *
      *****************************************************************
       01  PI-DRAN0205.
           05 PI-DRAN0205-GENL-DATEN.
               10 PI-DRAN0205-PGEB-GES         PIC S9(4).
               10 PI-DRAN0205-GENL-HVANTNUM    PIC X(9).
               10 PI-DRAN0205-GENL-PIN         PIC S9(9) COMP.
               10 PI-DRAN0205-GENL-AVERM       PIC S9(7).
               10 PI-DRAN0205-GENL-AVERM-PZ    PIC X(1).
               10 PI-DRAN0205-GENL-VN-VORNAME  PIC X(26).
               10 PI-DRAN0205-GENL-VN-ZUNAME   PIC X(26).
               10 PI-DRAN0205-GENL-VN-AKAD-T   PIC X(26).
               10 PI-DRAN0205-GENL-VN-SVNR     PIC S9(10) COMP-3.
               10 PI-DRAN0205-GENL-VN-PLZ      PIC X(5).
               10 PI-DRAN0205-GENL-VN-ORT      PIC X(26).
               10 PI-DRAN0205-GENL-VN-STRASSE  PIC X(26).
               10 PI-DRAN0205-GENL-VN-KNR      PIC S9(15) COMP-3.
               10 PI-DRAN0205-GENL-VN-BLZ      PIC S9(15) COMP-3.
150610         10 PI-DRAN0205-GENL-DIR         PIC 9(3).
250713         10 PI-DRAN0205-GENL-VN-IBAN     PIC X(34).
080316         10 PI-DRAN0205-GENL-EA-DATEINAME PIC X(50).
300919         10 PI-DRAN0205-GENL-VERSION     PIC S9(4).
      *
       01  PO-DRAN0205.
           05 PO-DRAN0205-RC                   PIC  X(003).
             88 PO-DRAN0205-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0205-RC-NOT-FOUND            VALUE 'NFD'.
      *
           05 PO-DRAN0205-GENL-DATEN.
               10 PO-DRAN0205-GENL-BLACKBOX    PIC X(30000).




