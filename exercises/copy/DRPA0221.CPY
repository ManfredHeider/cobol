      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  Januar 1997
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0221.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0221.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:53:48   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:11:20   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0221  AKTUELLE STUFE ERMITTELN FüR ALLE  *
      *                           STELLENTYPEN                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Giese                                           *
      *    DATUM:    10.02.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.02.97! 01 ! Neuerstellung             ! Giese    *
      *         !05.04.97!    ! Erweiterung RCs           ! Giese    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0221                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *   MöGLICHE RETURNCODES DER FUNKTION DRPA0221                 *
      *   ------------------------------------------                 *
      *   SDN  STUFE ZUM MITGEGEBENEM DATUM NICHT GEFUNDEN           *
      *        (betr. nur MA)                                        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0221.
          05 PI-DRPA0221-DATEN.
             10 PI-DRPA0221-ST-NR               PIC S9(09) BINARY.
             10 PI-DRPA0221-STICHTAG            PIC  X(10).

       01 PO-DRPA0221.
          05 PO-DRPA0221-RC                     PIC  X(003).
             88 PO-DRPA0221-RC-OK                    VALUE 'OK '.
050497       88 PO-DRPA0221-RC-SDN                   VALUE 'SDN'.
          05 PO-DRPA0221-STUFEN-BEZ             PIC  X(004).
          05 PO-DRPA0221-STUFEN-NR              PIC  9(004).
          05 PO-DRPA0221-ST-TYP                 PIC S9(004).
      *
      ******** Ende Copy-Member **************************************
