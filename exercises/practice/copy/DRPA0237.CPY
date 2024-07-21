      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0237.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0237.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   05 Jun 1997 08:03:20   T13AE31
      *AFPA0017 - Neue Funktion
      *
      *   Rev 1.0   Feb 19 1997 12:13:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0237  Funktion Select-ID Standard-OZP    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Cecconi                                         *
      *    DATUM:    12.03.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0017!12.03.97! 01 ! NEUERSTELLUNG             ! CECCONI  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0237                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ID  <<<    PEWA TBPA0011                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0237.
          05 PI-DRPA0237-DATEN.
             10 PI-DRPA0237-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0237-PEWA-PIN         PIC S9(009) BINARY.
      *
       01 PO-DRPA0237.
          05 PO-DRPA0237-RC                  PIC  X(003).
             88 PO-DRPA0237-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0237-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0237-PEWA-DATEN.
             10 PO-DRPA0237-PEWA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0237-PEWA-VSTAT       PIC  X(004).
          05 PO-DRPA0237-PEWA-LAST-UPDATE    PIC  X(026).
      *
      ****** Ende Copy-Member ****************************************
