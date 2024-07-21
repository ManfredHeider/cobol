      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1404.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1404.CPv  $
      *
      *   Rev 2.1   07 Sep 2000 14:56:38   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1404                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1405                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                 *
      *    DATUM:    19.07.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!19.07.00! 2.1 ! NEUERSTELLUNG            ! GRAY     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1403                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01  PI-DRPA1404.
           05  PI-DRPA1404-DATEN.
               10 PI-DRPA1404-TRANS-ID      PIC  9(006).
       01  PO-DRPA1404.
           05  PO-DRPA1404-RC               PIC  X(003).
               88 PO-DRPA1404-RC-OK              VALUE 'OK '.
               88 PO-DRPA1404-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA1404-DATEN.
               10 PO-DRPA1404-STATUS        PIC  9.
      *
      ******* ENDE COPY-MEMBER **************************************
