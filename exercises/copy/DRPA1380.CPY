      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1380.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1380.CPv  $
      *
      *   Rev 2.1   16 May 2000 09:22:32   T13EX23
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1380                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1379                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                 *
      *    DATUM:    15.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!15.05.00! 2.1 ! NEUERSTELLUNG            ! GRAY     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1379                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01  PI-DRPA1380.
           05  PI-DRPA1380-DATEN.
               10 PI-DRPA1380-PGEB-GES      PIC S9(004) BINARY.
               10 PI-DRPA1380-PRINTE        PIC S9(015) PACKED-DECIMAL.
      *
       01  PO-DRPA1380.
           05  PO-DRPA1380-RC               PIC  X(003).
               88 PO-DRPA1380-RC-OK              VALUE 'OK '.
               88 PO-DRPA1380-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA1380-DATEN.
               10 PO-DRPA1380-PINZ-GUELTIG-AB
                                            PIC X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
