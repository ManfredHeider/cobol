      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0333.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0333.CPv  $
      *
      *   Rev 2.3   Oct 04 2018 12:42:32   t13ae34
      *AAN01390: Anpassung Etikettendruck Antrag
      *
      *   Rev 2.2   Oct 02 2018 17:46:36   t13ae34
      *AAN01390: Anpassung Etikettendruck Antrag
      *
      *   Rev 2.1   Oct 01 2018 18:07:32   t13ae34
      *AAN01390: Anpassung Etikettendruck Antrag
      *
      *   Rev 2.0   Sep 28 2018 11:59:54   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0333  FUNKTION SELECT MAXPIN             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01390!28.09.18!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0333                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  MAX  <<<  TABELLE ANTS TBAN0010         *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0333.
          05 PI-DRAN0333-PGEB-GES             PIC S9(4) COMP.
      *
       01 PO-DRAN0333.
          05 PO-DRAN0333-RC                   PIC X(003).
             88 PO-DRAN0333-RC-OK                 VALUE 'OK '.
      *
          05 PO-DRAN0333-ANTS-DATEN.
             10 PO-DRAN0333-MAX-PIN           PIC S9(9) COMP.
      ****** ENDE COPY-MEMBER ****************************************



