      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1390.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1390.CPv  $
      *
      *   Rev 2.1   06 Jun 2000 11:48:08   T13EX21
      *APA00097 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:58:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1390                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PINZ >>                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    05.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00097!05.06.00!2.01 ! NEUERSTELLUNG            ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1390                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1390.
          05 PI-DRPA1390-PROGRAMM            PIC  X(08).
          05 PI-DRPA1390-UFKT-CODE           PIC  9(02).
          05 PI-DRPA1390-DATEN.
             10 PI-DRPA1390-PRINTE           PIC S9(15) PACKED-DECIMAL.
      *
       01 PO-DRPA1390.
          05 PO-DRPA1390-RC                  PIC  X(03).
             88 PO-DRPA1390-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1390-RC-LEER               VALUE 'LER'.
             88 PO-DRPA1390-RC-EOF                VALUE 'EOF'.
          05 PO-DRPA1390-DATEN.
             10 PO-DRPA1390-PGEB-GES           PIC S9(04) COMP.
             10 PO-DRPA1390-PINZ-PIN           PIC S9(09) COMP.
             10 PO-DRPA1390-PRINTE             PIC S9(15)V COMP-3.
             10 PO-DRPA1390-PINZ-PG-VBNR       PIC S9(09) COMP.
             10 PO-DRPA1390-PINZ-GUELTIG-AB    PIC  X(10).
             10 PO-DRPA1390-PINZ-LAST-UPDATE   PIC  X(26).
             10 PO-DRPA1390-PINZ-KENNUNG       PIC  X(08).
