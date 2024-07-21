      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0046.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0046.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:51:16   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0046  Funktion Insert Standard-OZP       *
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0050                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PPGR TBPA0005              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0046.
          05 PI-DRPA0046-PPGR-KEY.
             10 PI-DRPA0046-PGEB-GES          PIC S9(004) BINARY.
             10 PI-DRPA0046-PROD-PROD-SCHL    PIC S9(004) BINARY.
             10 PI-DRPA0046-PGRP-ID           PIC S9(004) BINARY.
             10 PI-DRPA0046-PPGR-GUELT-AB     PIC  X(010).
          05 PI-DRPA0046-PPGR-DATEN.
             10 PI-DRPA0046-PPGR-GUELT-BIS    PIC  X(010).
      *
       01 PO-DRPA0046.
          05 PO-DRPA0046-RC                   PIC  X(003).
             88 PO-DRPA0046-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0046-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
