      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0020.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0020.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:48:06   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0020  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    13.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !13.08.96! 01 ! Neuerstellung             ! Priggen  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0020                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       PDSP TBPA0001                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0020.
          05 PI-DRPA0020-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0020-PDSP-KEY.
             10 PI-DRPA0020-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0020-PDSP-DTA-ART     PIC S9(004) BINARY.
      *
       01 PO-DRPA0020.
          05 PO-DRPA0020-RC                  PIC  X(003).
             88 PO-DRPA0020-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0020-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0020-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
