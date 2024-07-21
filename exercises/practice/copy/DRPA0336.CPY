      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0336.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0336.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:52:26   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:21:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0336                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<EXT. VNR AUS PM ERM.>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    12.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.06.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0336                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0336.
          05 PI-DRPA0336-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0336-DATEN.
             10 PI-DRPA0336-AMV.
                COPY DRPA0295.
      *
       01 PO-DRPA0336.
          05 PO-DRPA0336-RC                         PIC  X(003).
             88 PO-DRPA0336-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0336-DATEN.
             10 PO-DRPA0336-VNR                     PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
