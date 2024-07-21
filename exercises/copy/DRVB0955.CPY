      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0955.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0955.CPv  $
      *
      *   Rev 2.1   Oct 29 2009 13:20:02   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.0   Sep 18 2009 12:12:54   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0955                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0033                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    18.09.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVB01014!18.09.09!2.1 ! NEU                       ! Walb      *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0955 - ERMITTELN Anzahl ED zu vorgeg.         *
      *                        ED-Besitzer und Austrittsdatum         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0955.
           05  PI-DRVB0955-DATEN.
               10  PI-DRVB0955-MA-ST-NR          PIC S9(09) COMP.
               10  PI-DRVB0955-ZEITPUNKT         PIC  X(10).
      *
       01  PO-DRVB0955.
           05  PO-DRVB0955-RC                    PIC  X(03).
               88  PO-DRVB0955-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0955-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0955-DATEN.
               10  PO-DRVB0955-ANZAHL            PIC S9(04) COMP.
      ****
      **** ENDE COPY DRVB0955 ****************************************
      ****
