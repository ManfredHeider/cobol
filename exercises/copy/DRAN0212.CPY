      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0212.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0212.CPv  $
      *
      *   Rev 2.5   Jul 25 2013 10:51:46   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.4   May 15 2012 10:39:40   t13ae23
      *AAN01051: Neu Generali  "RISK Care" mit R6.16
      *
      *   Rev 2.3   Dec 14 2011 17:27:28   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:49:58   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:02:04   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2008 09:39:04   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0212  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00828!25.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01033!14.12.11!2.2 ! DATEN GELDWÄSCHE ECT      ! RAHNER   *
      * AAN01051!15.05.12!2.4 !INTERNE ID-NUMMERN GENERALI! RAHNER   *
      * AAN01073!25.07.13!2.5 !SEPA                       ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0212                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE GENS TBAN0012              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0212.
           05 PI-DRAN0212-GENS-KEY.
               10 PI-DRAN0212-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0212-GENS-HVANTNUM    PIC X(9).
      *
       01  PO-DRAN0212.
           05 PO-DRAN0212-RC                   PIC  X(003).
             88 PO-DRAN0212-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0212-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0212-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0212-GENS-DATEN.
               10 PO-DRAN0212-GENS-PIN         PIC S9(9) COMP.
               10 PO-DRAN0212-GENS-LAST-UPDATE PIC X(26).
               10 PO-DRAN0212-GENS-KENNUNG-A   PIC X(8).
      *
      *    GENS-DATEN AUS PC-DATEN
           COPY DRAN0214.
      ****** ENDE COPY-MEMBER ****************************************

