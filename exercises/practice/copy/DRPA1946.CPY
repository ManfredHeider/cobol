      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  01.07.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1946.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1946.CPv  $
      *
      *   Rev 2.1   Jul 03 2020 07:10:38   t13ex67
      *FPA01775: Abend beim Aufruf einer EVS-Notiz 
      *
      *   Rev 2.0   Jul 01 2020 08:39:52   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1946
      *    --------------------------------------------------------
      *    FUNKTIONSAUFRUF <<LESEN EVS-NOTIZ     >>
      *    --------------------------------------------------------
      *    ERSTELLT: STORCH
      *    DATUM:    01.07.2020
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA01775!01.07.20!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1946                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1946.
          05 FILLER                                 PIC  X(001).
      *
       01 PO-DRPA1946.
          05 PO-DRPA1946-RC                         PIC  X(003).
             88 PO-DRPA1946-RC-OK                         VALUE 'OK '.
          05 PO-DRPA1946-DATEN.
             10 PO-DRPA1946-NZPA0011                PIC  X(23200).
      *
      ******* ENDE COPY-MEMBER **************************************
