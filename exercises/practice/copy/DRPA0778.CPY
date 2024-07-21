      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0778.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0778.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:09:22   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:46:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0778  FUNKTION BEENDEN PRODUKT           *
      *                                     FÜR UPPA0118             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!18.11.99! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0778                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  BEENDEN  PRODUKT <<<                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0778.
          05 PI-DRPA0778-PROD-KEY.
             10 PI-DRPA0778-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA0778-PROD-PROD-SCHL     PIC S9(004)  BINARY.
             10 PI-DRPA0778-PROD-BEGINN        PIC  X(010).
             10 PI-DRPA0778-PROD-LAST-UPDATE   PIC  X(026).

          05 PI-DRPA0778-PROD-DATEN-NEU.
             10 PI-DRPA0778-PROD-ENDE          PIC  X(010).
      *
       01 PO-DRPA0778.
          05 PO-DRPA0778-RC                    PIC  X(003).
             88 PO-DRPA0778-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0778-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
