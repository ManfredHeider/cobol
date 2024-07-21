      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0179.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0179.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:03:56   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0179  Funktion "Empf-Typen sethen"       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Schrieber                                       *
      *    DATUM:    22.01.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !22.01.97! 01 ! Neuerstellung             ! Schrieber*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0179                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0179.
          05 PI-DRPA0179-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA0179-DATEN.
             10 PI-DRPA0179-AVERM               PIC S9(009) BINARY.
             10 PI-DRPA0179-VM                  PIC S9(009) BINARY.
             10 PI-DRPA0179-STICHTAG            PIC  X(010).

       01 PO-DRPA0179.
          05 PO-DRPA0179-RC                     PIC  X(003).
             88 PO-DRPA0179-RC-OK                    VALUE 'OK '.
      *
      ******** Ende Copy-Member **************************************
