      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0296.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0296.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Aug 1998 18:38:12   T13EX22
      *APAA0032 - PEW Release 2.0
      *
      *   Rev 1.0   Apr 24 1997 14:32:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0296  FUNKTION "SUCHEN VSTATUS"          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    21.05.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!21.05.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0296                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0296.
          05 PI-DRPA0296-DATEN.
             10 PI-DRPA0296-VSTAT-1            PIC  X(004).
             10 PI-DRPA0296-VSTAT-2            PIC  X(004).
             10 PI-DRPA0296-VSTAT-3            PIC  X(004).
      *
       01 PO-DRPA0296.
          05 PO-DRPA0296-RC                    PIC  X(003).
             88 PO-DRPA0296-RC-OK                   VALUE 'OK '.
          05 PO-DRPA0296-VSTAT-1-KZ           PIC  X(001).
             88 PO-DRPA0296-VSTAT-1-VORH            VALUE 'J'.
          05 PO-DRPA0296-VSTAT-2-KZ           PIC  X(001).
             88 PO-DRPA0296-VSTAT-2-VORH            VALUE 'J'.
          05 PO-DRPA0296-VSTAT-3-KZ           PIC  X(001).
             88 PO-DRPA0296-VSTAT-3-VORH            VALUE 'J'.
      *
      ******** ENDE COPY-MEMBER **************************************
