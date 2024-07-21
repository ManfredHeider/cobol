      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1238.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drpa1238.cpv  $
      *
      *   Rev 2.2   May 31 1999 07:44:10   T01JM2S
      *APA00012 - NEUE RC'S UPPA0100
      *
      *   Rev 2.1   May 17 1999 18:32:00   T01JM2S
      *APA00012 - NEUE FUNKTIONEN DRPA1237, DRPA1238
      *
      *   Rev 2.0   Mar 30 1999 08:44:10   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1238                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. PVW/BATCH ZUK 2>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    14.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.05.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!26.05.99!2.2 ! NEU PO-DRPA1238-RC-NOK    ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1238                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1238.
          05 PI-DRPA1238-DATEN.
             10 PI-DRPA1238-NPM-TEIL.
                COPY DRPA0940.
             10 PI-DRPA1238-MELD-TEIL.
                COPY DRPA0941.
             10 PI-DRPA1238-MAX-USED-MELD-DS        PIC S9(004) BINARY.
             10 PI-DRPA1238-MELD-DS-TEIL            OCCURS 10
                                                 INDEXED BY IX-MELD-DS.
                COPY DRPA0941.
      *
       01 PO-DRPA1238.
          05 PO-DRPA1238-RC                         PIC  X(003).
             88 PO-DRPA1238-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1238-RC-NICHT-BUCHEN               VALUE 'NBU'.
             88 PO-DRPA1238-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA1238-DATEN.
             10 PO-DRPA1238-MAX-USED-KOFV-TEIL      PIC S9(004) BINARY.
             10 PO-DRPA1238-KOFV-TEIL               OCCURS 2.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
