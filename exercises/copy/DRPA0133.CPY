      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0133.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0133.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:20   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0133  FUNKTION Zeitraum prüfen           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    04.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !04.09.96! 01 ! Neuerstellung             ! Prömpeler*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0133                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0133.
          05 PI-DRPA0133-STICHTAG            PIC  X(010).
          05 PI-DRPA0133-DATUM-ANF           PIC  X(010).
          05 PI-DRPA0133-DATUM-END           PIC  X(010).
          05 PI-DRPA0133-DAUER               PIC S9(004) BINARY.
          05 PI-DRPA0133-DAUER-FORMAT        PIC  X(001).
             88 PI-DRPA0133-FORMAT-TAG             VALUE 'T'.
             88 PI-DRPA0133-FORMAT-MON             VALUE 'M'.
             88 PI-DRPA0133-FORMAT-JHR             VALUE 'J'.
          05 PI-DRPA0133-UFKT-CODE           PIC  X(002).
             88 PI-DRPA0133-ANF-END                VALUE 'AE'.
             88 PI-DRPA0133-ANF-DAUER              VALUE 'AD'.
             88 PI-DRPA0133-END-DAUER              VALUE 'ED'.

       01 PO-DRPA0133.
          05 PO-DRPA0133-RC                     PIC  X(003).
             88 PO-DRPA0133-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0133-RC-NOK                   VALUE 'NOK'.
             88 PO-DRPA0133-RC-ZU-GROSS              VALUE 'ZGR'.
             88 PO-DRPA0133-RC-ZU-KLEIN              VALUE 'ZKL'.
      *
      ******** Ende Copy-Member **************************************
