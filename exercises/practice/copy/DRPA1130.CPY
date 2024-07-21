      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1130.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1130.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:30:00   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 26 1999 13:52:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:59:02   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1130  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    12.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.11.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00012!24.11.98!1.02! erweitert um kennung und  ! bohlinger*
      *         !        !    ! last-update               !          *
      * APA02908!17.02.20!2.1 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1130                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST PVOR <<                                         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1130.
          05 PI-DRPA1130-PROGRAMM            PIC  X(008).
          05 PI-DRPA1130-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1130-DATEN.
170220       10 PI-DRPA1130-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1130-PRINTE           PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA1130.
          05 PO-DRPA1130-RC                  PIC  X(003).
             88 PO-DRPA1130-RC-OK             VALUE 'OK '.
             88 PO-DRPA1130-RC-LEER           VALUE 'LER'.
             88 PO-DRPA1130-RC-EOF            VALUE 'EOF'.
          05 PO-DRPA1130-DATEN.
             10 PO-DRPA1130-PVOR-LAST-UPDATE PIC  X(026).
             10 PO-DRPA1130-PVOR-KENNUNG     PIC  X(010).
          05 PO-DRPA1130-PVOR-DATEN.
170220       10 PO-DRPA1130-PGEB-GES         PIC S9(004) BINARY.
             10 PO-DRPA1130-PRINTE-VERTRAG   PIC S9(015) PACKED-DECIMAL.
170220       10 PO-DRPA1130-PGEB-GES-VORVER  PIC S9(004) BINARY.
             10 PO-DRPA1130-PRINTE-VORVERTRAG
                                             PIC S9(015) PACKED-DECIMAL.
      *
      ***** ENDE-COPY-MEMBER *****************************************

