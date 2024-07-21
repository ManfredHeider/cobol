      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1666.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1666.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:36:36   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Dec 10 2007 13:45:38   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Jul 17 2007 07:41:18   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1666 FUNKTION GES. 91-PM FORMAL PRUEFEN  *
      *                 UND NORMIEREN                                *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!17.07.07!2.1 ! NEU                       ! STORCH   *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1666                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1666.
             10 PI-DRPA1666-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1666-PEG-DATEN-GES91.
                COPY DRPA1623.
080408       10 PI-DRPA1666-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1666.
          05 PO-DRPA1666-RC                 PIC  X(003).
             88 PO-DRPA1666-RC-OK                VALUE 'OK '.
             88 PO-DRPA1666-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
