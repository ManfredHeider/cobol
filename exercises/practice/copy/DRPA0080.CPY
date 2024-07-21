      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0080.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0080.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:55:48   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0080  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0080                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        PGKO TBPA0013                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0080.
          05 PI-DRPA0080-LAST-UPDATE        PIC  X(026).
          05 PI-DRPA0080-PGKO-KEY.
             10 PI-DRPA0080-PGRP-ID         PIC S9(004) BINARY.
             10 PI-DRPA0080-PGKO-GUELT-AB   PIC  X(010).
             10 PI-DRPA0080-PGKO-SEL-1-VON  PIC S9(004) BINARY.
             10 PI-DRPA0080-PGKO-SEL-2-VON  PIC S9(004) BINARY.
             10 PI-DRPA0080-PGKO-SEL-3-VON  PIC S9(004) BINARY.
      *
       01 PO-DRPA0080.
          05 PO-DRPA0080-RC                 PIC  X(003).
             88 PO-DRPA0080-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0080-RC-CHANGED                  VALUE 'CHA'.
             88 PO-DRPA0080-RC-NOT-FOUND                VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
