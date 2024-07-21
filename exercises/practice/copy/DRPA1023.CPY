      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1023.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1023.CPv  $
      *
      *   Rev 2.1   May 27 1999 10:37:28   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 13:59:30   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1023                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF LIST ZU PVPT                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    21.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!21.07.98!01.01!NEU                       ! D.WILHELM*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1023                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1023.
          05 PI-DRPA1023-PROGRAMM            PIC  X(008).
          05 PI-DRPA1023-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1023-DATEN.
             10 PI-DRPA1023-PIN              PIC S9(009) USAGE COMP.
      *
       01 PO-DRPA1023.
          05 PO-DRPA1023-RC                  PIC  X(003).
             88 PO-DRPA1023-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1023-RC-LEER               VALUE 'LER'.
             88 PO-DRPA1023-RC-EOF                VALUE 'EOF'.
          05 PO-DRPA1023-ANZAHL-ERG-ZEILEN   PIC  9(05).
          05 PO-DRPA1023-DATEN.
             10 PO-DRPA1023-PRINTE           PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1023-PROD-PROD-SCHL   PIC S9(009) USAGE COMP.
             10 PO-DRPA1023-PVER-VERSION     PIC S9(004) USAGE COMP.
