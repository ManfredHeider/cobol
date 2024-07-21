      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1374.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1374.CPv  $
      *
      *   Rev 2.1   Nov 26 2001 16:39:10   t13ex21
      *FPA00293 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1374                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN AKTUELLSTE PMPT-VERSION>>          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    06.11.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00293!06.11.01!2.1 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1374                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1374.
          05 PI-DRPA1374-DATEN.
             10 PI-DRPA1374-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1374.
          05 PO-DRPA1374-RC                         PIC  X(003).
             88 PO-DRPA1374-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1374-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1374-DATEN.
             10 PO-DRPA1374-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1374-PMPT-VERSION            PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
