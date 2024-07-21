      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2138
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2138 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0213.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0213.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:51:52   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:27:36   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:50:06   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:02:10   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 16 2138 09:39:10   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0213  FUNKTION SELECT ÜBER GES, PIN      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01033!14.12.11!2.3 ! DATEN GELDWÄSCHE ECT.     ! RAHNER   *
      * AAN01073!25.07.13!2.4 ! SEPA                      ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0213                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE GENS TBAN0013              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0213.
           05 PI-DRAN0213-GENS-KEY.
               10 PI-DRAN0213-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0213-GENS-PIN         PIC S9(9) COMP.
      *
       01  PO-DRAN0213.
           05 PO-DRAN0213-RC                   PIC  X(003).
             88 PO-DRAN0213-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0213-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0213-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0213-GENS-DATEN.
               10 PO-DRAN0213-GENS-LAST-UPDATE PIC X(26).
               10 PO-DRAN0213-GENS-KENNUNG-A   PIC X(8).

      *    GENS-DATEN AUS PC-DATEN
           COPY DRAN0214.

      ****** ENDE COPY-MEMBER ****************************************

