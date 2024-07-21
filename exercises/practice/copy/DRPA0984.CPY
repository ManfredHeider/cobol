      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0984.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0984.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:27:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:20:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0984                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0984                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRÜNER                                           *
      *    DATUM:    02.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.98!01.01! NEU                      ! GRÜNER   *
      *         !14.10.98!01.01! KORREKTUREN              ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
141098*    FÜR DIE FUNKTION DRPA0984: SELECT ALL PAEV
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0984.
           05  PI-DRPA0984-KEY.
               10 PI-DRPA0984-PRINTE        PIC S9(015) PACKED-DECIMAL.
141098*        10 PI-DRPA0984-VERSION       PIC S9(004) BINARY.
141098*    05  PI-DRPA0984-DATEN.
141098*        COPY DRPA0988.
      *
       01  PO-DRPA0984.
           05  PO-DRPA0984-RC               PIC  X(003).
               88 PO-DRPA0984-RC-OK              VALUE 'OK '.
               88 PO-DRPA0984-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA0984-DATEN-1.
               10 PO-DRPA0984-LAST-UPDATE   PIC  X(026).
               10 PO-DRPA0984-KENNUNG       PIC  X(008).
           05  PO-DRPA0984-DATEN.
      *    *** NESTED COPY VON DRPA0988 ***
               COPY DRPA0988.
      *
      ******* ENDE COPY-MEMBER **************************************
