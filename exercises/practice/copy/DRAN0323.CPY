      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0323.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0323.CPv  $
      *
      *   Rev 2.1   Apr 20 2018 14:45:50   t13ae34
      *FAN01127: Erstellzeitpunkt wird in TBAN0008 verändert.
      *
      *   Rev 2.0   Mar 06 2018 16:29:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0323  FUNKTION INSERT STANDARD-OZP
      *                 FÜR TBAN0008
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN01127!20.04.18!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0323                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> INSERT MIT ERSTELLZEITPUNT <<<  TABELLE ANTA TBAN0008 *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0323.
          05 PI-DRAN0323-DATEN.
             10 PI-DRAN0323-ANTRAG-DATEN.
                15 PI-DRAN0323-ANTRAG-KOPF.
                   COPY DRAN0141.
                15 PI-DRAN0323-ANTRAG-PE-STRUKTUR.
                   COPY DRAN0142.

       01 PO-DRAN0323.
          05 PO-DRAN0323-RC                         PIC  X(003).
             88 PO-DRAN0323-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0323-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ***************************************

