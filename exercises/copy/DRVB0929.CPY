      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0929.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0929.CPv  $
      *
      *   Rev 2.1   Apr 07 2009 10:48:04   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Feb 25 2009 14:21:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0929                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0015                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.02.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!25.02.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0929 - ERMITTELN EDS FÜR D-CHANCENPROFIL      *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0929.
           05  PI-DRVB0929-DATEN.
               10  PI-DRVB0929-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0929-MA-ST-NR          PIC S9(009) COMP.
               10  PI-DRVB0929-ED-DAT-VON        PIC  X(010).
               10  PI-DRVB0929-ED-DAT-BIS        PIC  X(010).
      *
       01  PO-DRVB0929.
           05  PO-DRVB0929-RC                    PIC  X(003).
               88  PO-DRVB0929-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0929-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0929-DATEN.
               10  PO-DRVB0929-EDD-DAT-VON       PIC  X(026).
               10  PO-DRVB0929-ED-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0929-EDD-BEZUGS-NR     PIC S9(009) COMP.
      ****
      **** ENDE COPY DRVB0929 ****************************************
      ****
