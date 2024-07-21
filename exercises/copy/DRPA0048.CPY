      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0048.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0048.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:51:28   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0048  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    15.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !15.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0048                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       PPGR TBPA0005                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0048.
          05 PI-DRPA0048-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0048-PPGR-KEY.
             10 PI-DRPA0048-PGEB-GES          PIC S9(004) BINARY.
             10 PI-DRPA0048-PROD-PROD-SCHL    PIC S9(004) BINARY.
             10 PI-DRPA0048-PGRP-ID           PIC S9(004) BINARY.
             10 PI-DRPA0048-PPGR-GUELT-AB     PIC  X(010).
      *
       01 PO-DRPA0048.
          05 PO-DRPA0048-RC                   PIC  X(003).
             88 PO-DRPA0048-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0048-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0048-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
