      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1698.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1698.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:02:26   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Feb 09 2010 14:13:06   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1698                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST PPIA SORTIERUNG AKT.DAT,ME-KZ  >>   *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01000!15.06.10!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1698                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1698.
          05 PI-DRPA1698-PROGRAMM               PIC  X(008).
          05 PI-DRPA1698-UFKT                   PIC  9(002).
      *
       01 PO-DRPA1698.
          05 PO-DRPA1698-RC                     PIC  X(003).
             88 PO-DRPA1698-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1698-RC-EOF                   VALUE 'EOF'.
             88 PO-DRPA1698-RC-LEER                  VALUE 'LER'.
          05 PO-DRPA1698-DATEN.
             15 PGEB-GES                        PIC S9(004) COMP.
             15 PPIA-VNR                        PIC X(015).
             15 PPIA-PIN                        PIC S9(009) COMP.
             15 PPIA-DTA-KZ                     PIC X(001).
             15 PPIA-KENNUNG                    PIC X(008).
             15 PPIA-LAST-UPDATE                PIC X(010).
