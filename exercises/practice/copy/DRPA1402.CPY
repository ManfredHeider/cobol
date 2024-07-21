      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1402.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1402.CPv  $
      *
      *   Rev 2.1   07 Sep 2000 14:56:28   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1402                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1402                    *
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
      *    FÜR DIE FUNKTION DRPA1402                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01  PI-DRPA1402.
           05  PI-DRPA1402-DATEN.
               10 PI-DRPA1402-PGEB-GES               PIC S9(004) BINARY.
               10 PI-DRPA1402-GRP-DATEN              OCCURS 10.
                  15 PI-DRPA1402-KNR-DIA-STAT-GRP    PIC S9(004) BINARY.
       01  PO-DRPA1402.
           05  PO-DRPA1402-RC                        PIC  X(003).
               88 PO-DRPA1402-RC-OK                  VALUE 'OK '.
           05  PO-DRPA1402-DATEN.
               10 PO-DRPA1402-GRP-DATEN              OCCURS 10.
                  15 PO-DRPA1402-KNR-DIA-STAT-GRP    PIC  9(004) BINARY.
                  15 PO-DRPA1402-KNR-DIA-FKT-ZUL     PIC  9(004) BINARY.
                  15 PO-DRPA1402-LAST-UPDATE         PIC  X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
