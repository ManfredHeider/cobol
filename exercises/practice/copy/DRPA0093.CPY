      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0093.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0093.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:57:18   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0093  Funktion Select Standard-OZP       *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0093                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PVSG TBPA0016                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0093.
          05 PI-DRPA0093-PVSG-KEY.
             10 PI-DRPA0093-PVSG-VSTAT-GRP    PIC S9(004) BINARY.
             10 PI-DRPA0093-PVST-VSTAT        PIC  X(004).
      *
       01 PO-DRPA0093.
          05 PO-DRPA0093-RC                   PIC  X(003).
             88 PO-DRPA0093-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0093-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0093-PVSG-DATEN.
             10 PO-DRPA0093-KTY-VSTAT-GRP     PIC S9(004) BINARY.
          05 PO-DRPA0093-PVSG-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0093-PVSG-KENNUNG         PIC  X(008).
      *
      ****** Ende Copy-Member *****************************************
