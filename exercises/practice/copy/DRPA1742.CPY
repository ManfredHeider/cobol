      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1742.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1742.CPv  $
      *
      *   Rev 2.1   Dec 17 2013 11:17:12   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG
      *
      *   Rev 2.0   Mar 19 2013 13:31:58   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1742                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF << BEWEGUNGSSCHLÜSSEL PRÜFEN >>            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                   *
      *    DATUM:    19.03.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!19.03.13!2.1 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1742                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1742.
          05 PI-DRPA1742-DATEN.
             10 PI-DRPA1742-BEW-SCHLUESSEL          PIC  9(006).
      *
       01 PO-DRPA1742.
          05 PO-DRPA1742-RC                         PIC  X(003).
             88 PO-DRPA1742-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1742-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1742-DATEN.
             10 PO-DRPA1742-BEW-SCHLUESSEL          PIC  9(006).
      *
      ******* ENDE COPY-MEMBER **************************************
