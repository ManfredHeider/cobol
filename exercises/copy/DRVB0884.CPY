      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0884.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0884.CPv  $
      *
      *   Rev 2.1   10 Jan 2008 10:41:32   t13ae30
      *AAS00728: Kapitänsclub und Admiralsclub Ermittlung / Prüfung
      *
      *   Rev 2.0   14 Dec 2007 10:30:36   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0884                                     *
      *    FUNKTION ERMITTELT FÜR EINEN KANDIDATEN ALLE DIREKT       *
      *    UNTERSTELLTEN AKTIVEN RF-T (RFT-DAT-BIS IS NULL)          *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    14.12.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00728!14.12.07! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0884                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0884.
          05 PI-DRVB0884-PROGRAMM                PIC  X(008).
          05 PI-DRVB0884-UFKT-CODE               PIC  9(002).
          05 PI-DRVB0884-MA-ST-NR                PIC  S9(009)    COMP.
      *
       01 PO-DRVB0884.
          05 PO-DRVB0884-RC                      PIC  X(003).
             88 PO-DRVB0884-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0884-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0884-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0884-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0884-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0884-DATEN.
             10 PO-DRVB0884-BETR-NR              PIC  S9(009)    COMP.
             10 PO-DRVB0884-UNTST-NR             PIC  S9(009)    COMP.
      *
      ******** Ende Copy-Member DRVB0884 *****************************
