      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0294.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0294.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 09:28:48   T13EX16
      *APA00032 - PEW-Release 2.0
      *
      *   Rev 1.0   Apr 24 1997 14:31:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0294  FUNKTION "BEWERTEN VSTATUS"        *
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
      * AFPA0012!24.11.97!    ! EINBAU CHECKPOINT         ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0294                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0294.
          05 PI-DRPA0294-DATEN.
241197*      10 FILLER                          PIC  X(001).
241197       10 PI-DRPA0294-UFKT                PIC  9(002).
241197          88 PI-DRPA0294-BEWERTEN-KOMPLETT     VALUE 01.
241197          88 PI-DRPA0294-BEWERTEN-AB-CHECK     VALUE 02.
241197       10 PI-DRPA0294-CHECKPOINT          PIC S9(004) BINARY.
      *
       01 PO-DRPA0294.
          05 PO-DRPA0294-RC                     PIC  X(003).
             88 PO-DRPA0294-RC-OK                    VALUE 'OK '.
          05 PO-DRPA0294-DATEN.
             10 PO-DRPA0294-VTAB-ZEILE.
                15 PO-DRPA0294-VSTAT            PIC  X(004).
                15 PO-DRPA0294-VSTAT-UNR        PIC S9(004) BINARY.
                15 PO-DRPA0294-ORT              PIC  X(030).
                15 PO-DRPA0294-BEREICH          PIC  X(010).
             10 PO-DRPA0294-VTAB-ANZ            PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
