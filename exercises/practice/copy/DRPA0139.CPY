      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0139.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0139.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:54   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0139  FUNKTION PRODUKTGRUPPE ERMITTELN   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    12.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !12.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0139                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0139.
          05 PI-DRPA0139-DATEN.
             10 PI-DRPA0139-PGEB-GES          PIC S9(004)  BINARY.
             10 PI-DRPA0139-PROD-PROD-SCHL    PIC S9(004)  BINARY.
             10 PI-DRPA0139-STICHTAG          PIC  X(010).
      *
       01 PO-DRPA0139.
          05 PO-DRPA0139-RC                   PIC  X(003).
             88 PO-DRPA0139-RC-OK                  VALUE 'OK '.
             88 PO-DRPA0139-RC-LEER                VALUE 'LER'.
          05 PO-DRPA0139-DATEN.
             10 PO-DRPA0139-ANZAHL            PIC S9(004)  BINARY.
             10 PO-DRPA0139-TABELLE OCCURS 10.
                15 PO-DRPA0139-PGRP-ID        PIC S9(004)  BINARY.
                15 PO-DRPA0139-KNR-PGRP-ART   PIC S9(004)  BINARY.
                15 PO-DRPA0139-PGRP-ALPHA     PIC  X(010).
                15 PO-DRPA0139-PGRP-NUMMER    PIC S9(009)  BINARY.
      *
      ******** Ende Copy-Member **************************************
