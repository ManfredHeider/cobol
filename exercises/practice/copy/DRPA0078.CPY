      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0078.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0078.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:55:34   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0078  Funktion Insert Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0078                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PGKO TBPA0013                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0078.
          05 PI-DRPA0078-PGKO-KEY.
             10 PI-DRPA0078-PGRP-ID         PIC S9(004) BINARY.
             10 PI-DRPA0078-PGKO-GUELT-AB   PIC  X(010).
             10 PI-DRPA0078-PGKO-SEL-1-VON  PIC S9(004) BINARY.
             10 PI-DRPA0078-PGKO-SEL-2-VON  PIC S9(004) BINARY.
             10 PI-DRPA0078-PGKO-SEL-3-VON  PIC S9(004) BINARY.
          05 PI-DRPA0078-PGKO-DATEN.
             10 PI-DRPA0078-PGKO-GUELT-BIS  PIC  X(010).
             10 PI-DRPA0078-PGKO-SEL-1-BIS  PIC S9(004) BINARY.
             10 PI-DRPA0078-PGKO-SEL-2-BIS  PIC S9(004) BINARY.
             10 PI-DRPA0078-PGKO-SEL-3-BIS  PIC S9(004) BINARY.
             10 PI-DRPA0078-PKON-KOND-NR    PIC S9(004) BINARY.
      *
       01 PO-DRPA0078.
          05 PO-DRPA0078-RC                 PIC  X(003).
             88 PO-DRPA0078-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0078-RC-DUPLICATE-VALUE          VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
