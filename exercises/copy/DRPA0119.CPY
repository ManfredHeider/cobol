      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0119.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0119.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:00:46   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0119  Funktion Prov.-Struk ergänzen      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !27.08.96! 01 ! Neuerstellung             ! Prömpeler*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0119                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0119.
          05 PI-DRPA0119-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA0119-DATEN.
             10 PI-DRPA0119-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRPA0119-ZVP-STICHTAG        PIC  X(010).

       01 PO-DRPA0119.
          05 PO-DRPA0119-RC                     PIC  X(003).
             88 PO-DRPA0119-RC-OK                    VALUE 'OK '.
          05 PO-DRPA0119-DATEN.
             10 PO-DRPA0119-PEWA-VSTAT          PIC  X(004).
             10 PO-DRPA0119-PEWA-AVERM          PIC S9(009) BINARY.
             10 PO-DRPA0119-PEWA-VM             PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************
