      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0749.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0749.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:09:10   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0749  FUNKTION DELETE PRODUKTENEINTRAG   *
      *                           Zuordnung für UPPA0118             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    08.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Apa00068!08.11.99! 01 ! NEUERSTELLUNG             ! Bohlinger*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0749                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE PRODUKT <<<                                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0749.
          05 PI-DRPA0749-PROD-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA0749-PROD-KEY.
             10 PI-DRPA0749-PGEB-GES        PIC S9(004)  BINARY.
             10 PI-DRPA0749-PROD-PROD-SCHL  PIC S9(004)  BINARY.
             10 PI-DRPA0749-PROD-BEGINN     PIC  X(010).
      *
       01 PO-DRPA0749.
          05 PO-DRPA0749-RC                 PIC  X(003).
             88 PO-DRPA0749-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0749-RC-NOT-FOUND                 VALUE 'NFD'.
      *
