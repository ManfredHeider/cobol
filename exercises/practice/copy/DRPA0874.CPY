      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0874.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0874.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:23:32   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:48:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER DRPA0874 :                                     *
      *   FUNKTION DRPA0874 - LIST VERTRAGSHISTORIE                  *
      *     DES DIALOGMODULS UGPA0001 (VERTRAGSDATEN)                *
      *   -------------------------------------------------------    *
      *   ERSTELLT: STORCH                                           *
      *   DATUM:    02.07.1998                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.98! 1.1!NEU                        ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0874                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA0877 )                *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0874.
          05 PI-DRPA0874-UFKT                    PIC  9(02).
          05 PI-DRPA0874-PRINTE                  PIC S9(015) COMP-3.

          05 PI-DRPA0874-AUSWAHL-KRITERIEN.
             10 PI-DRPA0874-PGEB-GES             PIC 9(002).
             10 PI-DRPA0874-VNR                  PIC X(015).

       01 PO-DRPA0874.
          05  PO-DRPA0874-RC                     PIC  X(003).
              88 PO-DRPA0874-RC-OK                        VALUE 'OK '.
              88 PO-DRPA0874-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA0874-RC-LEER                      VALUE 'LER'.

          05 PO-DRPA0874-ERGEBNISZEILE.
             COPY DRPA0987.
