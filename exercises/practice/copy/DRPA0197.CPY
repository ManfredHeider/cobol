      * T13EX13   Stand: 10/02/97    15:38:52
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0197.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0197.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Aug 1998 11:56:38   T13EX16
      *APA00032 - PEW-Release 2.0
      *
      *   Rev 1.0   Feb 19 1997 12:08:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0197  FUNKTION LIST-OZP FÜR TABELLE PEWA *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0006!30.01.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * AFPA0006!07.03.97! 02 ! ermitteln pewa-pin        ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0197                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0197.
          05 PI-DRPA0197-PROGRAMM           PIC  X(008).
          05 PI-DRPA0197-UFKT-CODE          PIC  9(002).
          05 PI-DRPA0197-AUSWAHL-KRITERIEN.
             10 PI-DRPA0197-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA0197-PG-WV-DAT-AB    PIC  X(010).
             10 PI-DRPA0197-PG-WV-DAT-BIS   PIC  X(010).
      *
       01 PO-DRPA0197.
          05 PO-DRPA0197-RC                 PIC  X(003).
             88 PO-DRPA0197-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0197-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0197-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0197-DATEN.
             10 PO-DRPA0197-PEWA-ID          PIC S9(015) PACKED-DECIMAL.
970307       10 PO-DRPA0197-PEWA-PIN         PIC S9(009) BINARY.
             10 PO-DRPA0197-PEWA-VSTAT       PIC  X(004).
             10 PO-DRPA0197-PEWA-VSTAT-UNR   PIC S9(004) BINARY.
             10 PO-DRPA0197-PEWA-PG-MELD-DAT PIC  X(010).
             10 PO-DRPA0197-PDTA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0197-PEWA-LAST-UPDATE PIC  X(026).
      *
      ***** Ende-Copy-Member *****************************************
