      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0086.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0086.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:56:34   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0086  Funktion Insert Standard-OZP       *
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
      *         !13.02.97!    ! anpassen Vstat            ! bork     *
      *         !20.02.97!    ! Hinzufügen Tabellenfelder ! Storch   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0086                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PVST TBPA0015              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0086.
          05 PI-DRPA0086-PVST-KEY.
130297       10 PI-DRPA0086-PVST-VSTAT        PIC  X(004).
          05 PI-DRPA0086-PVST-DATEN.
200297       10 PI-DRPA0086-PVST-KMPTZ        PIC  S9(4) USAGE COMP.
200297       10 PI-DRPA0086-PVST-VSTAT-NEU    PIC  X(004).
130297       10 PI-DRPA0086-PVST-TEXT         PIC  X(050).
      *
       01 PO-DRPA0086.
          05 PO-DRPA0086-RC                   PIC  X(003).
             88 PO-DRPA0086-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0086-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ***** Ende-Copy-Member *****************************************
