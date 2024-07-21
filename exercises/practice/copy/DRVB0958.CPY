      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0958.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0958.CPv  $
      *
      *   Rev 2.1   Oct 29 2009 13:20:18   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.0   Oct 05 2009 11:08:36   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0958                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0033                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    05.10.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVB01014!05.10.09!2.1 ! NEU                       ! Walb      *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0958 - ERMITTELN Anzahl ED zu vorgeg.         *
      *                        ED-Zweig    und Austrittsdatum         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0958.
           05  PI-DRVB0958-DATEN.
               10  PI-DRVB0958-ED-ZWEIG-NR       PIC S9(09) COMP.
               10  PI-DRVB0958-ZEITPUNKT         PIC  X(10).
      *
       01  PO-DRVB0958.
           05  PO-DRVB0958-RC                    PIC  X(03).
               88  PO-DRVB0958-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0958-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0958-DATEN.
               10  PO-DRVB0958-ANZAHL-ZWEIG      PIC S9(04) COMP.
      ****
      **** ENDE COPY DRVB0958 ****************************************
      ****
