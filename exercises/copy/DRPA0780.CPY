      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0780.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0780.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:19:54   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   12 Dec 1997 09:47:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0780                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST zu PMPE über DTA-ID und PGEB>>      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                 *
      *    DATUM:    06.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.01.98!01.01! NEU                      ! J. Jäger *
      *         !        !     !                          !          *
      * AFPA0012!07.09.98!01.02! NEU: PMPG-ID             ! WOLFF    *
      * APA00012!24.09.98!01.03! NEU: PMPE-VNR            ! MUND     *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0780                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0780.
          05 PI-DRPA0780-PROGRAMM            PIC  X(008).
          05 PI-DRPA0780-UFKT                PIC  9(002).
          05 PI-DRPA0780-SCHALTER            PIC  9.
          05 PI-DRPA0780-DATEN.
             10 PI-DRPA0780-PDTA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0780-PGEB-GES         PIC S9(004) BINARY.
070998       10 PI-DRPA0780-PMPG-ID          PIC S9(015) PACKED-DECIMAL.
240998       10 PI-DRPA0780-PMPE-VNR         PIC  X(015).
      *
       01 PO-DRPA0780.
          05 PO-DRPA0780-RC                  PIC  X(003).
             88 PO-DRPA0780-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0780-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0780-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0780-DATEN.
             COPY DRPA0289.
