      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1523.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1523.CPv  $
      *
      *   Rev 2.1   Feb 18 2002 14:25:46   t13ex43
      *FPA00352 : Korrekturbuchung für halbe Einheiten
      *
      *   Rev 2.0   04 Jul 2001 11:11:10   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1523                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF BEREITSTELLEN UMBUCHUNGEN EINH-KORREKTUR   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    11.02.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00352!11.02.02!2.01! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1523                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1523.
          05 PI-DRPA1523-PROGRAMM            PIC  X(08).
          05 PI-DRPA1523-UFKT                PIC  9(02).
          05 PI-DRPA1523-SCHALTER            PIC  9.
          05 PI-DRPA1523-AUSWAHLKRITERIEN.
             10 PI-DRPA1523-PGEB-GES         PIC S9(04) USAGE COMP.
             10 PI-DRPA1523-BUCHUNGSART      PIC S9(04) USAGE COMP.
             10 PI-DRPA1523-ZEITRAUM-VON     PIC  X(10).
             10 PI-DRPA1523-ZEITRAUM-BIS     PIC  X(10).
      *
       01 PO-DRPA1523.
          05 PO-DRPA1523-RC                  PIC  X(03).
             88 PO-DRPA1523-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1523-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA1523-RC-LEER               VALUE 'LER'.
          05 PO-DRPA1523-ANZAHL-ERG-ZEILEN   PIC  9(05).

          05 PO-DRPA1523-ERGEBNISZEILE.
               10 PO-DRPA1523-PGEB-GES       PIC S9(04) USAGE COMP.
               10 PO-DRPA1523-PRINTE         PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1523-PDPT-ID        PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1523-PMPT-ID        PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1523-PMPT-VERSION   PIC S9(04) COMP.
               10 PO-DRPA1523-PVER-VNR       PIC  X(15).
               10 PO-DRPA1523-PVER-AKT-PIN   PIC  9(07).
      *
