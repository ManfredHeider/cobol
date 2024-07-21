      *-------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *-------------------------------------------------------------*
      *
      *-------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *-------------------------------------------------------------*
      *    $Workfile:   DRPA0935.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0935.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:37:54   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:49:34   P13SP99
      *Initial revision.
      *
      *-------------------------------------------------------------*
      ***************************************************************
      *    COPY-MEMBER: DRPA0935 FUNKTIONEN init provisions-         *
      *                                          datenmodul          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    18.05.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!18.05.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0935                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0935.
          05  PI-DRPA0935-UFKT                 PIC  X(002).
              88 PI-DRPA0935-INIT-TS-QUEUE              VALUE '01'.

      *
       01 PO-DRPA0935.
          05 PO-DRPA0935-RC                    PIC  X(003).
             88 PO-DRPA0935-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0935-RC-NOK                      VALUE 'NOK'.

      *
      ******** Ende Copy-Member **************************************
