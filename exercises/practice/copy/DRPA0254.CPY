      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0254.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0254.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Apr 09 1997 16:55:28   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.1   Apr 08 1997 09:31:00   T13AE31
      *
      *Put-Test
      *
      *   Rev 1.0   Apr 08 1997 09:10:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0254  ZULETZT GüLTIGE STUFE ERMITTELN    *
      *                           FüR ALLE STELLENTYPEN              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GIESE                                           *
      *    DATUM:    05.04.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !05.04.97! 01 ! Neuerstellung             ! Giese    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0254                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0254.
          05 PI-DRPA0254-DATEN.
             10 PI-DRPA0254-ST-NR               PIC S9(09) BINARY.

       01 PO-DRPA0254.
          05 PO-DRPA0254-RC                     PIC  X(003).
             88 PO-DRPA0254-RC-OK                    VALUE 'OK '.
          05 PO-DRPA0254-STUFEN-BEZ             PIC  X(004).
          05 PO-DRPA0254-STUFEN-NR              PIC  9(004).
          05 PO-DRPA0254-ST-TYP                 PIC S9(004).
      *
      ******** Ende Copy-Member **************************************
