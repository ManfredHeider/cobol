      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0922.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0922.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:10:30   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:29:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:14:30   P13SP99
      *Initial revision.
      ****************************************************************
      *    COPY-MEMBER: DRPA0922  Funktion BEENDEN PRODUKT           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!18.11.99!2.01! Neuerstellung             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0922                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  BEENDEN PRODUKT << PROD TBPA0004                     *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0922.
          05 PI-DRPA0922-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0922-PROD-KEY.
             10 PI-DRPA0922-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0922-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PI-DRPA0922-PROD-BEGINN      PIC  X(010).

          05 PI-DRPA0922-PROD-DATEN.
             10 PI-DRPA0922-PROD-ENDE        PIC  X(010).
      *
       01 PO-DRPA0922.
          05 PO-DRPA0922-RC                  PIC  X(003).
             88 PO-DRPA0922-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0922-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0922-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
      *
      *---------------------------------------------------------------*
