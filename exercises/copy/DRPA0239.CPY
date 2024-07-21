      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0239.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0239.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Jun 1997 09:40:30   T13AE31
      *AFPA0017 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:13:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0239  Funktion Update-VSTAT Standard-OZP *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Cecconi                                         *
      *    DATUM:    12.03.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !12.03.97! 01 ! Neuerstellung             ! Cecconi  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0239                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE-VSTAT  <<<       PEWP TBPA0002                *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0239.
          05 PI-DRPA0239-PEWP-LAST-UPDATE    PIC  X(026).
          05 PI-DRPA0239-PEWP-KEY.
             10 PI-DRPA0239-PEWA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0239-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0239-PROD-PROD-SCHL   PIC S9(004) BINARY.
          05 PI-DRPA0239-PEWP-DATEN.
             10 PI-DRPA0239-PEWP-VSTAT       PIC  X(004).
      *
       01 PO-DRPA0239.
          05 PO-DRPA0239-RC                  PIC  X(003).
             88 PO-DRPA0239-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0239-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0239-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende CopyMember *****************************************

