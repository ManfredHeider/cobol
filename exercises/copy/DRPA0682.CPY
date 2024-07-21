      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0682.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0682.cpv  $
      *
      *   Rev 2.0   May 21 1999 13:07:36   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 16:59:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0682 FUNKTION Suche PNAP-Objekt zum Ver- *
      *                                   trag mit größter Fälligkeit*
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                *
      *    DATUM:    15.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01!NEU                       ! Jäger    *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0682                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0682.
          05 PI-DRPA0682-DATEN.
             10 PI-DRPA0682-PRINTE           PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0682.
          05 PO-DRPA0682-RC                  PIC  X(003).
             88 PO-DRPA0682-RC-OK                 VALUE 'OK '.
          05 PO-DRPA0682-PNAP.
             COPY DRPA0407.
      *
      ******** Ende Copy-Member **************************************
