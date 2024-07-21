      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1886.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1886.CPv  $
      *
      *   Rev 2.1   Apr 04 2018 15:36:26   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.0   Mar 28 2018 10:20:10   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1886 FUNKTION UWPA0046                   *
      *                 GEV- PM FORMAL PRUEFEN/NORMIEREN             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    26.03.2018                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!26.03.18! 01 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1886                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1886.
          05 PI-DRPA1886-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1886-DATEN.
             10 PI-DRPA1886-PEG-DATEN-GEI.
                COPY DRPA1887.
          05 PI-DRPA1886-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA1886.
          05 PO-DRPA1886-RC                         PIC  X(003).
             88 PO-DRPA1886-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1886-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *

