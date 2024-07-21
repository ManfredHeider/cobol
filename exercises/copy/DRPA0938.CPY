      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0938.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0938.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:49:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 14:16:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0938                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERMITTELN PEG/DIALOG>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.04.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0938                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0938.
          05 PI-DRPA0938-DATEN.
             10 PI-DRPA0938-NPM-TEIL-ALT.
                COPY DRPA0940.
             10 PI-DRPA0938-NPM-TEIL-NEU.
                COPY DRPA0940.
      *
       01 PO-DRPA0938.
          05 PO-DRPA0938-RC                         PIC  X(003).
             88 PO-DRPA0938-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0938-RC-NICHT-BUCHEN               VALUE 'NBU'.
          05 PO-DRPA0938-DATEN.
             10 PO-DRPA0938-KOFV-TEIL.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
