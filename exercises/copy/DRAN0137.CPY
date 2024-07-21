      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0137.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0137.CPv  $
      *
      *   Rev 2.2   22 Dec 2006 20:32:32   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 11:02:18   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 14 2006 16:32:42   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0137                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ANTP SORTIERUNG LAUFENDE NR.   >>   *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!24.09.06!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0137                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRAN0137.
          05 PI-DRAN0137-PROGRAMM               PIC  X(008).
          05 PI-DRAN0137-UFKT                   PIC  9(002).
          05 ANTP-KEY.
             10 PGEB-GES                        PIC S9(4)   COMP.
             10 ANTP-NR                         PIC S9(15)V COMP-3.
      *
       01 PO-DRAN0137.
          05 PO-DRAN0137-RC                     PIC  X(003).
             88 PO-DRAN0137-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0137-RC-EOF                   VALUE 'EOF'.
             88 PO-DRAN0137-RC-LEER                  VALUE 'LER'.
          05 PO-DRAN0137-DATEN.
             15 ANTP-LAST-UPDATE                PIC  X(026).
             15 ANTP-KENNUNG                    PIC  X(008).
             15 PO-DRAN0137-ANTRAG-PRODUKT.
                COPY DRAN0143.
      *
      ******* ENDE COPY-MEMBER **************************************
