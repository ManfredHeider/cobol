      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1672.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1672.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:36:46   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   Dec 10 2007 14:04:06   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   10 Oct 2007 12:20:18   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1672 FUNKTION GENINVEST PM FORMAL PRUEFEN*
      *                 UND NORMIEREN                                *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!10.10.07!2.1 ! NEU                       ! STORCH   *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1672                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1672.
             10 PI-DRPA1672-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1672-PEG-DATEN-GENINV.
                COPY DRPA1623.
080408       10 PI-DRPA1672-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1672.
          05 PO-DRPA1672-RC                 PIC  X(003).
             88 PO-DRPA1672-RC-OK                VALUE 'OK '.
             88 PO-DRPA1672-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
