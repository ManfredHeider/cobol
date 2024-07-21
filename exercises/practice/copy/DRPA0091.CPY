      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0091.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0091.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:57:08   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0091  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !20.08.96! 01 ! Neuerstellung             ! Prömpeler*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0091                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       PVSG TBPA0016                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0091.
          05 PI-DRPA0091-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0091-PVSG-KEY.
             10 PI-DRPA0091-PVSG-VSTAT-GRP    PIC S9(004) BINARY.
             10 PI-DRPA0091-PVST-VSTAT        PIC  X(004).
          05 PI-DRPA0091-PVSG-DATEN.
             10 PI-DRPA0091-KTY-VSTAT-GRP     PIC S9(004) BINARY.
      *
       01 PO-DRPA0091.
          05 PO-DRPA0091-RC                   PIC  X(003).
             88 PO-DRPA0091-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0091-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0091-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************
