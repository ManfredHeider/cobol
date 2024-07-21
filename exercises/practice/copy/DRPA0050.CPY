      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0050.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0050.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Jan 1999 16:58:12   T13EX26
      *apa00012 - Anpassung an Tabelle TBPA0006
      *
      *   Rev 1.1   Mar 06 1997 14:51:40   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0050  Funktion Insert Standard-OZP       *
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
      * apa00012!13.01.99! 02 ! Anpassung an Tabellenänd. ! Parent   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0050                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PGRP TBPA0006              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0050.
          05 PI-DRPA0050-PGRP-KEY.
             10 PI-DRPA0050-PGRP-ID        PIC S9(004) BINARY.
          05 PI-DRPA0050-PGRP-DATEN.
             10 PI-DRPA0050-KNR-PGRP-ART   PIC S9(004) BINARY.
             10 PI-DRPA0050-KTY-PGRP-ART   PIC S9(004) BINARY.
130199       10 PI-DRPA0050-KTY-PGRP-ART-AUSPR  PIC S9(4) USAGE COMP.
130199       10 PI-DRPA0050-KNR-PGRP-ART-AUSPR  PIC S9(4) USAGE COMP.
130199*      10 PI-DRPA0050-PGRP-BEZ          PIC  X(040).
             10 PI-DRPA0050-PGRP-ALPHA     PIC  X(010).
             10 PI-DRPA0050-PGRP-NUMMER    PIC S9(009) BINARY.
      *
       01 PO-DRPA0050.
          05 PO-DRPA0050-RC                PIC  X(003).
             88 PO-DRPA0050-RC-OK                      VALUE 'OK '.
             88 PO-DRPA0050-RC-DUPLICATE-VALUE         VALUE 'DVA'.
      *
      ******** Ende Copy-Member **************************************
