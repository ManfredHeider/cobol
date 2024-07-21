      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0925.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0925.CPv  $
      *
      *   Rev 2.1   Nov 23 1999 16:32:24   T13EX21
      *FPA00112 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:14:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0925  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00112!22.11.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0925                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0925.
          05 PI-DRPA0925-PROGRAMM           PIC  X(008).
          05 PI-DRPA0925-UFKT               PIC  9(002).
          05 PI-DRPA0925-DATEN.
             10 PI-DRPA0925-PMPG-ID         PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0925.
          05 PO-DRPA0925-RC                 PIC  X(003).
             88 PO-DRPA0925-RC-OK           VALUE 'OK '.
             88 PO-DRPA0925-RC-LEER         VALUE 'LER'.
             88 PO-DRPA0925-RC-EOF          VALUE 'EOF'.

          05 PO-DRPA0925-DATEN.
             10 PO-DRPA0925-PMPG-ID             PIC S9(015)
                                                    PACKED-DECIMAL.
             10 PO-DRPA0925-PMPG-VERSION        PIC S9(004) BINARY.
             10 PO-DRPA0925-PDTA-ID             PIC S9(015)
                                                    PACKED-DECIMAL.
             10 PO-DRPA0925-PMPG-LFD-NR-VNR     PIC S9(009) BINARY.
             10 PO-DRPA0925-PMPG-VSTAT          PIC  X(004).
             10 PO-DRPA0925-PMPGH-LAST-UPDATE   PIC  X(026).
             10 PO-DRPA0925-PMPGH-KENNUNG       PIC  X(008).
             10 PO-DRPA0925-PMPG-INHALT.
                15 PO-DRPA0925-PMPG-INHALT-LEN  PIC S9(004) BINARY.
                15 PO-DRPA0925-PMPG-INHALT-TEXT PIC  X(3900).
      *
      ***** ENDE-COPY-MEMBER *****************************************
