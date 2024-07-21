      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0625.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0625.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:26:30   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 27 1999 13:07:16   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:55:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0625                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PVPT >>                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jürgen Jäger-Sokoluk                             *
      *    DATUM:    13.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.11.97!01.01!NEU                       ! Jäger    *
      * APA02908!17.02.20!2.1  ! GES-UEBERGR EVS          ! SCHEIDEL *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0625                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0625.
          05 PI-DRPA0625-PROGRAMM            PIC  X(008).
          05 PI-DRPA0625-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0625-DATEN.
             10 PI-DRPA0625-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0625-PVER-PRINTE      PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0625.
          05 PO-DRPA0625-RC                  PIC  X(003).
             88 PO-DRPA0625-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0625-RC-LEER               VALUE 'LER'.
             88 PO-DRPA0625-RC-EOF                VALUE 'EOF'.
          05 PO-DRPA0625-DATEN.
             COPY DRPA0410.

