      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1317.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1317.CPv  $
      *
      *   Rev 2.1   Feb 22 2000 08:43:00   T13AE18
      *APA00080
      *
      *   Rev 2.0   Nov 29 1999 13:55:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1317  FUNKTION LIST-OZP FÜR TABELLE PGDF *
      *                                                 TBPA0027     *
      *        ERMITTELN DER PG-ABHÄNGIGEN DIALOGFELDER              *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    06.12.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!06.12.99! 01 ! NEUERSTELLUNG             !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1317                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1317.
          05 PI-DRPA1317-PROGRAMM            PIC  X(008).
          05 PI-DRPA1317-UFKT                PIC  9(002).
      *
       01 PO-DRPA1317.
          05 PO-DRPA1317-RC                  PIC  X(003).
             88 PO-DRPA1317-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1317-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA1317-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA1317-ERGEBNISZEILE.
             COPY DRPA1311.
      *

      ***** Ende-Copy-Member *****************************************
