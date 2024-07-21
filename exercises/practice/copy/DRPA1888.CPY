      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1888.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1888.CPv  $
      *
      *   Rev 2.1   May 22 2018 09:48:22   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.0   Mar 29 2018 13:32:46   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1888 FUNKTION UWPA0046                   *
      *                 GEL- PM FORMAL PRUEFEN/NORMIEREN             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    29.03.2018                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!29.03.18! 01 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1888                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1888.
          05 PI-DRPA1888-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1888-DATEN.
             10 PI-DRPA1888-PEG-DATEN-GEL.
                COPY DRPA1889.
          05 PI-DRPA1888-KNR-MANDANT                PIC S9(004) BINARY.
      *
       01 PO-DRPA1888.
          05 PO-DRPA1888-RC                         PIC  X(003).
             88 PO-DRPA1888-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1888-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
      *
      ******* ENDE COPY-MEMBER **************************************

