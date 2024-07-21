      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0563.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0563.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:01:00   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 11:55:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0563                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PDPT/PDPE ERSTELLEN >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    24.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.10.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!10.08.98! 01 ! DRPA0413 raus, nur noch   ! Foltz    *
      *         !        !    ! pmpt-version und -id      !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0563                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0563.
          05 PI-DRPA0563-DATEN.
             10 PI-DRPA0563-PEST-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0563-GEF-ANZ-PDPE            PIC S9(004) BINARY.
             10 PI-DRPA0563-KNR-WAEHRUNG            PIC S9(004) BINARY.
980810*      10 PI-DRPA0563-PMPT
980810*         COPY DRPA0413.
980810       10 PI-DRPA0563-PMPT-ID                 PIC S9(015)
980810                                                  PACKED-DECIMAL.
980810       10 PI-DRPA0563-PMPT-VERSION            PIC S9(004) BINARY.

       01 PO-DRPA0563.
          05 PO-DRPA0563-RC                         PIC  X(003).
             88 PO-DRPA0563-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0563-DATEN.
             10 PO-DRPA0563-PDPT-PDPE.
                COPY DRPA0451.
      *
      ******* ENDE COPY-MEMBER **************************************
