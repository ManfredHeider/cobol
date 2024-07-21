      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0218.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0218.CPv  $
      *
      *   Rev 2.4   Jul 25 2013 10:52:18   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Österreich
      *GENERAL
      *I-LEBEN+SACH
      *
      *   Rev 2.3   Dec 14 2011 17:28:12   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.2   Dec 14 2011 16:50:38   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz für 16
      *
      *   Rev 2.1   Nov 24 2008 14:02:42   t13ae23
      *AAN00824: Elektronischer Antrag GENERALI Ges. 16
      *Teil - Ant
      *rag Dialog -
      *
      *   Rev 2.0   Sep 18 2008 10:20:52   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0217  FUNKTION SELECT ÜBER GES, PIN, AVERM
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00824!25.11.08!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN01033!14.12.11!2.3 ! ERWEITERUNG DRAN0214      ! RAHNER   *
      * AAN01073!25.07.13!2.4 ! SEPA                      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0217                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE GENS TBAN0013              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0218.
           05 PI-DRAN0218-GENS-KEY.
               10 PI-DRAN0218-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0218-GENS-PIN         PIC S9(9) COMP.
               10 PI-DRAN0218-GENS-AVERM       PIC S9(9) COMP.
      *
       01  PO-DRAN0218.
           05 PO-DRAN0218-RC                   PIC  X(003).
             88 PO-DRAN0218-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0218-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0218-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0218-GENSB-DATEN.
               10 PO-DRAN0218-GENS-LAST-UPDATE PIC X(26).
               10 PO-DRAN0218-GENS-KENNUNG-A   PIC X(8).

      *    GENS-DATEN AUS PC-DATEN
           COPY DRAN0214.

      ****** ENDE COPY-MEMBER ****************************************

