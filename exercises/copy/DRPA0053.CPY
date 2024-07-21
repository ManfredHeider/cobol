      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0053.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0053.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Jan 1999 16:59:28   T13EX26
      *apa00012 - Anpassung an Tabelle TBPA0006
      *
      *   Rev 1.1   Mar 06 1997 14:52:10   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0053  Funktion Select Standard-OZP       *
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
      * apa00012!13.01.99! 02 ! Anpassung an Tabellenänd. ! Parent   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0053                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PGRP TBPA0006                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0053.
          05 PI-DRPA0053-PGRP-KEY.
             10 PI-DRPA0053-PGRP-ID           PIC S9(004) BINARY.
      *
       01 PO-DRPA0053.
          05 PO-DRPA0053-RC                   PIC  X(003).
             88 PO-DRPA0053-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0053-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0053-PGRP-DATEN.
             10 PO-DRPA0053-KNR-PGRP-ART      PIC S9(004) BINARY.
             10 PO-DRPA0053-KTY-PGRP-ART      PIC S9(004) BINARY.
130199       10 PO-DRPA0053-KTY-PGRP-ART-AUSPR  PIC S9(4) USAGE COMP.
130199       10 PO-DRPA0053-KNR-PGRP-ART-AUSPR  PIC S9(4) USAGE COMP.
130199*      10 PO-DRPA0053-PGRP-BEZ          PIC  X(040).
             10 PO-DRPA0053-PGRP-ALPHA        PIC  X(010).
             10 PO-DRPA0053-PGRP-NUMMER       PIC S9(009) BINARY.
          05 PO-DRPA0053-PGRP-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA0053-PGRP-KENNUNG         PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************
