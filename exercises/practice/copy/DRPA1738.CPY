      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1738.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1738.CPv  $
      *
      *   Rev 2.2   Jan 14 2015 10:57:38   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.1   Jan 10 2014 13:25:14   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.0   Mar 06 2013 10:46:32   t13ex72
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1738                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO LEBEN >>                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOETTCHER                                        *
      *    DATUM:    06.03.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!06.03.13! 1.0! NEU                       ! BÖTTCHER *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1738                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1738.
          05 PI-DRPA1738-DATEN.
             10 PI-DRPA1738-PVER-ZUSTAND       PIC 9(002).
             10 PI-DRPA1738-ANTRAG-ZUSTAND     PIC 9(002).
             10 PI-DRPA1738-PIN-VNR-AKZ        PIC S9(004) BINARY.
             10 PI-DRPA1738-KNR-MANDANT        PIC S9(004) BINARY.
      *
       01 PO-DRPA1738.
          05 PO-DRPA1738-RC                    PIC  X(003).
             88 PO-DRPA1738-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1738-RC-MELDUNGSFEHLER        VALUE 'MFE'.
             88 PO-DRPA1738-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
 
          05 PO-DRPA1738-DATEN.
             10 PO-DRPA1738-VNR-ALT            PIC  X(015).
             10 PO-DRPA1738-PVER-ZUSTAND       PIC  9(002).
             10 PO-DRPA1738-STRUK-KDBETR       PIC  X(001).
             10 PO-DRPA1738-KD-BETR            PIC  9(007).
      ******* ENDE COPY-MEMBER **************************************

