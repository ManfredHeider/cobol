      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0132.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0132.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Nov 1998 09:02:46   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 06 1997 15:02:14   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0132  FUNKTION Datum verändern           *
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
      *    FÜR DIE FUNKTION DRPA0132                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0132.
          05 PI-DRPA0132-DATUM               PIC  X(010).
          05 PI-DRPA0132-DAUER               PIC S9(004) BINARY.
          05 PI-DRPA0132-DAUER-FORMAT        PIC  X(001).
             88 PI-DRPA0132-FORMAT-TAG            VALUE 'T'.
             88 PI-DRPA0132-FORMAT-MON            VALUE 'M'.
             88 PI-DRPA0132-FORMAT-JHR            VALUE 'J'.

       01 PO-DRPA0132.
          05 PO-DRPA0132-RC                  PIC  X(003).
             88 PO-DRPA0132-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0132-RC-NOK                VALUE 'NOK'.
          05 PO-DRPA0132-DATEN.
             10 PO-DRPA0132-DATUM            PIC  X(010).
      *
      ******** Ende Copy-Member **************************************
