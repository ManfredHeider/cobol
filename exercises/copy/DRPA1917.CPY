      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1917.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1917.CPv  $
      *
      *   Rev 2.1   Oct 15 2019 07:49:54   t13ex67
      *APA02846: PlanetHome- Anbindung der Plattform zur Immobilien
      *vermittlung durch den VB
      *
      *   Rev 2.0   Jun 04 2019 09:19:46   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * **************************************************************
      *    COPY-MEMBER: DRPA1917 FUNKTION UWPA0015                   *
      *                 PLANET IMMO - PM FORMAL PRUEFEN/NORMIEREN    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    04.06.2019                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02846!04.06.19!2.1 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1917                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1917.
             10 PI-DRPA1917-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1917-PEG-DAT-PLANETIMMO.
                COPY DRPA1918.
             10 PI-DRPA1917-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1917.
          05 PO-DRPA1917-RC                 PIC  X(003).
             88 PO-DRPA1917-RC-OK                VALUE 'OK '.
             88 PO-DRPA1917-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************


