      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0087.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0087.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:56:44   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0087  Funktion Update Standard-OZP       *
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
      *         !13.02.97!    ! Anpassen VSTAT            ! bork     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0087                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       PVST TBPA0015                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0087.
          05 PI-DRPA0087-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0087-PVST-KEY.
130297       10 PI-DRPA0087-PVST-VSTAT        PIC  X(004).
          05 PI-DRPA0087-PVST-DATEN.
130297       10 PI-DRPA0087-PVST-TEXT         PIC  X(050).
      *
       01 PO-DRPA0087.
          05 PO-DRPA0087-RC                   PIC  X(003).
             88 PO-DRPA0087-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0087-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0087-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************
