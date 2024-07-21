      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1255.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1255.CPv  $
      *
      *   Rev 2.1   Jun 22 1999 23:17:58   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1255    FUNKTION ERSTELLEN PROTOKOLL     *
      *                             DER UMBUCHUNG                    *
      *                             FÜR PRUB-SACHBEARBEITER UPPA0117 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    08.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.06.99! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1255                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  Erstellen Protokoll der Umbuchung <<<                *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1255.
          05 PI-DRPA1255-MAX-USED-PRUB-TAB     PIC S9(004) BINARY.
          05 PI-DRPA1255-TABELLE               OCCURS 20.
             10 PI-DRPA1255-PRUB-DATEN.
                COPY DRPA1262.
      *
       01 PO-DRPA1255.
          05 PO-DRPA1255-RC                    PIC  X(003).
             88 PO-DRPA1255-RC-OK                         VALUE 'OK '.
      *---------------------------------------------------------------*
