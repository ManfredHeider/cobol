      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1408.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1408.CPv  $
      *
      *   Rev 2.1   21 Sep 2000 16:06:42   T13EX21
      *APA00106 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:34   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1408                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF << LIST PIN'S ZUM VERTRAG >>               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    24.07.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00106!24.07.00!2.01 ! NEUERSTELLUNG            ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1408                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1408.
          05 PI-DRPA1408-UFKT                  PIC  9(02).
          05 PI-DRPA1408-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1408-AUSWAHL-KRITERIEN.
             10 PI-DRPA1408-PGEB-GES           PIC X(002).
      *
       01 PO-DRPA1408.
          05 PO-DRPA1408-RC                    PIC  X(003).
             88 PO-DRPA1408-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1408-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA1408-RC-LEER                 VALUE 'LER'.

          05 PO-DRPA1408-ERGEBNISZEILE.
             COPY DRPA1411.
      *
