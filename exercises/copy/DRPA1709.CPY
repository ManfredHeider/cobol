      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1709.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1709.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:02:42   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Mar 26 2010 15:46:44   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1709                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST PPIA MELDEKENNZEICHEN          >>   *
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
      *    FÜR DIE FUNKTION DRPA1709                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
       01 PI-DRPA1709.
          05 PI-DRPA1709-PROGRAMM               PIC  X(008).
          05 PI-DRPA1709-UFKT                   PIC  9(002).
          05 PI-DRPA1709-PPIA-PGEB-GES          PIC S9(004) COMP.
      *
       01 PO-DRPA1709.
          05 PO-DRPA1709-RC                     PIC  X(003).
             88 PO-DRPA1709-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1709-RC-EOF                   VALUE 'EOF'.
             88 PO-DRPA1709-RC-LEER                  VALUE 'LER'.
          05 PO-DRPA1709-DATEN.
             15 PPIA-VNR                        PIC X(015).
             15 PPIA-PIN                        PIC S9(009) COMP.
