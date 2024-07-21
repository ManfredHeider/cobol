      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0945.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0945.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:51:44   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 14:17:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0945                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERMITTELN PEG/BATCH >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    04.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.05.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0945                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0945.
          05 PI-DRPA0945-DATEN.
             10 PI-DRPA0945-NPM-TEIL.
                COPY DRPA0940.
      *
       01 PO-DRPA0945.
          05 PO-DRPA0945-RC                         PIC  X(003).
             88 PO-DRPA0945-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0945-DATEN.
             10 PO-DRPA0945-KOFV-TEIL.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
