      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0292.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0292.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 09:24:38   T13EX16
      *APA00032 - PEW-Release 2.0
      *
      *   Rev 1.0   Apr 24 1997 14:31:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0292  FUNKTION "INSERT VSTATUS"          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    16.05.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.05.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0292                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0292.
          05 PI-DRPA0292-DATEN.
             10 PI-DRPA0292-VTAB-ZEILE.
                15 PI-DRPA0292-VSTAT            PIC  X(004).
                15 PI-DRPA0292-VSTAT-UNR        PIC S9(004) BINARY.
                15 PI-DRPA0292-ORT              PIC  X(030).
                15 PI-DRPA0292-BEREICH          PIC  X(010).
      *
       01 PO-DRPA0292.
          05 PO-DRPA0292-RC                     PIC  X(003).
             88 PO-DRPA0292-RC-OK                    VALUE 'OK '.
      *   05 PO-DRPA0292-DATEN.
      *
      ******** ENDE COPY-MEMBER **************************************
