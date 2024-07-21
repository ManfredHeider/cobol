      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0187.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0187.CPv  $
      *
      *   Rev 2.6   Dec 14 2011 16:59:14   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.5   Feb 13 2007 19:08:06   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   Feb 13 2007 18:05:02   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen - MSC
      *
      *   Rev 2.3   Feb 13 2007 16:44:46   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen - MSC
      *
      *   Rev 2.2   12 Feb 2007 11:46:28   t13ex44
      *AAN00655: Erweiterung des PS auf 3 Stellen - MSC
      *
      *   Rev 2.0   05 Feb 2007 11:12:18   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0187  FUNKTION SELECT ÜBER GES, PIN      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!05.02.07!2.1 ! NEUERSTELLUNG             ! SCHEIDEL *
      * AAN01033!        !2.5 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0187                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE ANTA TBAN0008              *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0187.
          05 PGEB-GES                  PIC S9(004)   BINARY.
          05 ANTA-PIN                  PIC S9(009)   BINARY.
      *
       01 PO-DRAN0187.
          05 PO-DRAN0187-RC            PIC  X(003).
             88 PO-DRAN0187-RC-OK           VALUE 'OK '.
             88 PO-DRAN0187-RC-NOT-FOUND    VALUE 'NFD'.
      *
          05 PO-DRAN0187-ANTRAG-DATEN.
             15 PO-DRAN0187-ANTRAG-KOPF.
                COPY DRAN0141.
             15 ANTA-LAST-UPDATE                 PIC  X(026).
             15 ANTA-KENNUNG                     PIC  X(008).
             15 PO-DRAN0187-ANTRAG-PE-STRUKTUR.
                COPY DRAN0142.
      *
