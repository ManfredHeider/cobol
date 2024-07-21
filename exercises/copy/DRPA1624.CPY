      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1624.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1624.CPv  $
      *
      *   Rev 2.1   Nov 03 2004 11:53:50   t13ex25
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 14 2004 09:03:36   t13ex25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1624                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF << LIST PVKB'S ZUM VERTRAG >>              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                 *
      *    DATUM:    15.10.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00295!15.10.04!2.01 ! NEUERSTELLUNG            ! JÄGER    *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1624                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1624.
          05 PI-DRPA1624-UFKT                  PIC  9(02).
          05 PI-DRPA1624-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1624-AUSWAHL-KRITERIEN.
             10 PI-DRPA1624-PGEB-GES           PIC X(002).
             10 PI-DRPA1624-VNR                PIC X(015).
      *
       01 PO-DRPA1624.
          05 PO-DRPA1624-RC                    PIC  X(003).
             88 PO-DRPA1624-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1624-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA1624-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA1624-ERGEBNISZEILE.
             10 PVKB-GUELTIG-AB          PIC  X(026).
             10 PVKB-KD-BETREUER         PIC S9(009) COMP.
             10 PVKB-F-GUELTIG-AB        PIC  X(010).
             10 PVKB-DTA-KZ              PIC  X.
             10 PVKB-KENNUNG             PIC  X(008).
             10 PVKB-LAST-UPDATE         PIC  X(026).
      ***   END OF COPY
