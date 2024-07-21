      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1381.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1381.CPv  $
      *
      *   Rev 2.1   15 May 2000 08:49:48   T13EX21
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1381 FUNKTION CHECK KARDINALITÄTS-       *
      *                                   BEZIEHUNG                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    11.05.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!11.05.00!2.1 !NEU                        ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1381                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1381.
             10 PI-DRPA1381-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1381-KNR-KARD         OCCURS 10.
                15 PI-DRPA1381-KNR-KARD-BEZ    PIC S9(004) BINARY.

       01 PO-DRPA1381.
          05 PO-DRPA1381-RC                    PIC  X(003).
             88 PO-DRPA1381-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1381-KARD-TAB              OCCURS 10.
            10 PO-DRPA1381-KNR-KARD-BEZ        PIC  S9(004) BINARY.
            10 PO-DRPA1381-KNR-KARD            PIC  S9(004) BINARY.
            10 PO-DRPA1381-KONF-Z-INFO         PIC  S9(004) BINARY.
      ******** ENDE COPY-MEMBER **************************************
