      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0936.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0936.CPv  $
      *
      *   Rev 2.1   May 27 2009 14:16:08   t13ex43
      *AAS00815: PGM - Neukundenbonus
      *
      *   Rev 2.0   May 05 2009 14:32:50   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0936                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0032                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    05.05.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00815!05.05.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0936 - ERMITTELN DG-DATEN FÜR VB-NUMMER       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0936.
           05  PI-DRVB0936-DATEN.
               10  PI-DRVB0936-MA-ST-NR          PIC S9(009) COMP.
      *
       01  PO-DRVB0936.
           05  PO-DRVB0936-RC                    PIC  X(003).
               88  PO-DRVB0936-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0936-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0936-DATEN.
               10  PO-DRVB0936-DG-BEZ            PIC  X(010).
               10  PO-DRVB0936-DIR-NR            PIC S9(004) COMP.
               10  PO-DRVB0936-DL-MA-ST-NR       PIC S9(009) COMP.
               10  PO-DRVB0936-MAN-NAME          PIC  X(040).
               10  PO-DRVB0936-MAN-VORNAME       PIC  X(020).
               10  PO-DRVB0936-VERTRBER          PIC  X(010).
               10  PO-DRVB0936-BERVOR            PIC  X(040).
      ****
      **** ENDE COPY DRVB0936 ****************************************
      ****
