      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0089.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0089.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:56:54   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0089  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !20.08.96! 01 ! Neuerstellung             ! Prömpeler*
      *         !13.02.97!    ! anpassen VSTAT            ! Bork     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0089                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PVST TBPA0015                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0089.
          05 PI-DRPA0089-PVST-KEY.
130297       10 PI-DRPA0089-PVST-VSTAT        PIC  X(004).
      *
       01 PO-DRPA0089.
          05 PO-DRPA0089-RC                   PIC  X(003).
             88 PO-DRPA0089-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0089-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0089-PVST-DATEN.
130297       10 PO-DRPA0089-PVST-TEXT         PIC  X(050).
          05 PO-DRPA0089-PVST-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0089-PVST-KENNUNG         PIC  X(008).
      *
      ****** Ende Copy-Member *****************************************
