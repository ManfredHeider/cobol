      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0136.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0136.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:36   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0136  FUNKTION PRODUKTGRUPPE ERMITTELN   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !09.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0136                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0136.
          05 PI-DRPA0136-DATEN.
             10 PI-DRPA0136-PGEB-GES          PIC S9(004)  BINARY.
             10 PI-DRPA0136-PROD-PROD-SCHL    PIC S9(004)  BINARY.
             10 PI-DRPA0136-STICHTAG          PIC  X(010).
             10 PI-DRPA0136-KNR-PGRP-ART      PIC S9(004)  BINARY.
      *
       01 PO-DRPA0136.
          05 PO-DRPA0136-RC                   PIC  X(003).
             88 PO-DRPA0136-RC-OK                  VALUE 'OK '.
             88 PO-DRPA0136-RC-NOT-FOUND           VALUE 'NFD'.
          05 PO-DRPA0136-DATEN.
             10 PO-DRPA0136-PGRP-ID           PIC S9(004)  BINARY.
             10 PO-DRPA0136-PGRP-ALPHA        PIC  X(010).
             10 PO-DRPA0136-PGRP-NUMMER       PIC S9(009)  BINARY.
      *
      ******** Ende Copy-Member **************************************
