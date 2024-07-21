      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0875.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0875.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:23:34   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:48:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER DRPA0875 :                                     *
      *   FUNKTION DRPA0875 - LIST VERTRAGSPRODUKTE                  *
      *     DES DIALOGMODULS UGPA0001 (VERTRAGSDATEN)                *
      *   -------------------------------------------------------    *
      *   - BENUTZT BISHER ALS SCHNITTSTELLE ZWISCHEN DEM BUFFER-OZP *
      *     ZYPA0014 ( DIALOG PO12 - PROVISIONSBESTAND - AUSKUNFT    *
      *     VERTRAGSPRODUKTE ) UND UGPA0001                          *
      *   -------------------------------------------------------    *
      *   ERSTELLT: STORCH                                           *
      *   DATUM:    08.06.1998                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.06.98! 1.1!NEU                        ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0875                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRPA0877 )                *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0875.
          05 PI-DRPA0875-UFKT                    PIC  9(02).
          05 PI-DRPA0875-PRINTE                  PIC S9(015) COMP-3.

          05 PI-DRPA0875-AUSWAHL-KRITERIEN.
             10 PI-DRPA0875-PGEB-GES             PIC 9(002).
             10 PI-DRPA0875-VNR                  PIC X(015).

       01 PO-DRPA0875.
          05  PO-DRPA0875-RC                     PIC  X(003).
              88 PO-DRPA0875-RC-OK                        VALUE 'OK '.
              88 PO-DRPA0875-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA0875-RC-LEER                      VALUE 'LER'.

          05 PO-DRPA0875-ERGEBNISZEILE.
             COPY DRPA0971.
