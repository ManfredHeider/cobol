      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0122.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0122.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:01:02   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0122  Geschäfts-Kto. bilden              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !28.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      *         !        !    !                           !          *
      * Daisy 1 !22.01.97! 02 ! Überarbeitung PEW-Release ! Wolff    *
      * PEW-Rel.!        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0122                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0122.
          05 PI-DRPA0122-KNR-VERTRIEB-GES     PIC S9(004) BINARY.

       01 PO-DRPA0122.
          05 PO-DRPA0122-RC                   PIC  X(003).
             88 PO-DRPA0122-RC-OK                  VALUE 'OK '.
      *
      ******** Ende Copy-Member **************************************
