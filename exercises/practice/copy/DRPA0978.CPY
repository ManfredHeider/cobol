      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0978.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0978.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:37:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:20:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0978                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PVPT (SORTIERT) >>               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    30.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.06.98!01.01!NEU                       ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0978                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0978.
          05 PI-DRPA0978-PROGRAMM            PIC  X(008).
          05 PI-DRPA0978-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0978-DATEN.
             10 PI-DRPA0978-PVER-PRINTE      PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0978.
          05 PO-DRPA0978-RC                  PIC  X(003).
             88 PO-DRPA0978-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0978-RC-LEER               VALUE 'LER'.
             88 PO-DRPA0978-RC-EOF                VALUE 'EOF'.
          05 PO-DRPA0978-DATEN.
             COPY DRPA0410.
