      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.10.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0803.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0803.CPv  $
      *
      *   Rev 2.1   Nov 16 2005 10:25:26   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Oct 31 2005 18:03:32   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0803 (ERSTELLT FÜR ADMIRALS-CLUB)        *
      *    FUNKTION ERMITTELT FÜR EINEN KANDIDATEN ALLE DIREKT       *
      *    HERAUS GEWACHSENEN ERSTDIREKTIONEN, DIE AKTUELL           *
      *    NOCH TÄTIG SIND.                                          *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    04.11.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!04.11.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0803                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0803.
          05 PI-DRVB0803-PROGRAMM                PIC  X(008).
          05 PI-DRVB0803-UFKT-CODE               PIC  9(002).
          05 PI-DRVB0803-MA-ST-NR                PIC  S9(009)    COMP.
      *
       01 PO-DRVB0803.
          05 PO-DRVB0803-RC                      PIC  X(003).
             88 PO-DRVB0803-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0803-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0803-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0803-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0803-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0803-DATEN.
             10 PO-DRVB0803-BETR-NR              PIC  S9(009)    COMP.
             10 PO-DRVB0803-UNTST-NR             PIC  S9(009)    COMP.
             10 PO-DRVB0803-STUFE                PIC  S9(004)    COMP.
             10 PO-DRVB0803-DAT-VON              PIC   X(010).
      *
      ******** Ende Copy-Member DRVB0803 *****************************
