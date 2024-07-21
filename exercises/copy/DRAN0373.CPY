      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.10.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0373.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0373.CPv  $
      *
      *   Rev 2.1   Dec 08 2020 16:21:32   t13ae34
      *AAN01492: Projekt Testoptimierung im eingereichten Geschäft 
      *2020
      *
      *   Rev 2.0   Oct 30 2020 09:01:32   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0373  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01492!30.10.20!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0373                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE ANVH-GEDRUCKT = N >>> ANVH TBAN0018           *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0373.
          05 PI-DRAN0373-DATEN.
             10 PI-DRAN0373-TAGE                 PIC S9(04) COMP.
      *
       01 PO-DRAN0373.
          05 PO-DRAN0373-RC                      PIC  X(3).
             88 PO-DRAN0373-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0373-RC-NOT-FOUND              VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

