      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0074.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0074.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:55:06   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0074  Funktion Insert Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0074                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PZVP TBPA0012                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0074.
          05 PI-DRPA0074-PZVP-KEY.
             10 PI-DRPA0074-PZVP-VERFAHREN  PIC  X(020).
             10 PI-DRPA0074-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA0074-PROD-PROD-SCHL  PIC S9(004) BINARY.
             10 PI-DRPA0074-PZVP-GUELT-AB   PIC  X(010).
          05 PI-DRPA0074-PZVP-DATEN.
             10 PI-DRPA0074-PZVP-GUELT-BIS  PIC  X(010).
             10 PI-DRPA0074-PZVP-PROGRAMM   PIC  X(008).
             10 PI-DRPA0074-PZVP-FUNKTION   PIC  X(008).
             10 PI-DRPA0074-PZVP-IO-DATEN   PIC  X(008).
             10 PI-DRPA0074-PZVP-VERSION    PIC S9(004) BINARY.
      *
       01 PO-DRPA0074.
          05 PO-DRPA0074-RC                 PIC  X(003).
             88 PO-DRPA0074-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0074-RC-DUPLICATE-VALUE          VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
