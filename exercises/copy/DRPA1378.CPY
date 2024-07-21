      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1378.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1378.CPv  $
      *
      *   Rev 2.1   16 May 2000 09:21:34   T13EX23
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1378                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1378                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                             *
      *    DATUM:    11.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!11.05.00! 2.1 ! NEUERSTELLUNG            ! GRAY     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1378: LESEN AKTUELLSTE PINZ MIT
      *    PGEB-GES UND PIN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1378.
           05  PI-DRPA1378-DATEN.
               10 PI-DRPA1378-PGEB-GES      PIC S9(004) BINARY.
               10 PI-DRPA1378-PINZ-PIN      PIC S9(009) BINARY.
      *
       01  PO-DRPA1378.
           05  PO-DRPA1378-RC               PIC  X(003).
               88 PO-DRPA1378-RC-OK              VALUE 'OK '.
               88 PO-DRPA1378-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA1378-DATEN.
               10 PO-DRPA1378-PGEB-GES      PIC S9(004) BINARY.
               10 PO-DRPA1378-PINZ-PIN      PIC S9(009) BINARY.
               10 PO-DRPA1378-PRINTE        PIC S9(015) PACKED-DECIMAL.
               10 PO-DRPA1378-PINZ-PG-VBNR  PIC S9(009) BINARY.
               10 PO-DRPA1378-PINZ-GUELTIG-AB
                                            PIC X(010).
               10 PO-DRPA1378-PINZ-LAST-UPDATE
                                            PIC X(026).
               10 PO-DRPA1378-PINZ-KENNUNG  PIC X(008).
      *
      ******* ENDE COPY-MEMBER **************************************
