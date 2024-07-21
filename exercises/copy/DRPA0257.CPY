      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0257.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0257.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Jun 1997 08:06:46   T13AE31
      *AFPA0017 - Neuerstellung
      *
      *   Rev 1.0   28 May 1997 08:56:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0257  Funktion Update-RMELD Standard-OZP *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Cecconi                                         *
      *    DATUM:    21.04.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0017!21.04.97! 01 ! Neuerstellung             ! Cecconi  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0257                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE-RMELD  <<<       PEWP TBPA0002                *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0257.
          05 PI-DRPA0257-PEWP-LAST-UPDATE    PIC  X(026).
          05 PI-DRPA0257-PEWP-KEY.
             10 PI-DRPA0257-PEWA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0257-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0257-PROD-PROD-SCHL   PIC S9(004) BINARY.
          05 PI-DRPA0257-PEWP-DATEN.
             10 PI-DRPA0257-PEWP-VSTAT       PIC  X(004).
             10 PI-DRPA0257-PEWP-RMELD-DAT   PIC  X(010).
      *
       01 PO-DRPA0257.
          05 PO-DRPA0257-RC                  PIC  X(003).
             88 PO-DRPA0257-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0257-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0257-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende CopyMember *****************************************

