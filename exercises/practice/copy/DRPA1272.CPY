      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1272.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1272.CPv  $
      *
      *   Rev 2.1   Jun 24 1999 16:56:00   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:20   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1272                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN PG-ABR-DAT MIT PMBE-ID>>           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    24.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.06.99! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1272                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1272.
          05 PI-DRPA1272-DATEN.
             10 PI-DRPA1272-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1272.
          05 PO-DRPA1272-RC                         PIC  X(003).
             88 PO-DRPA1272-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1272-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1272-DATEN.
             10 PO-DRPA1272-PMBE-PG-ABR-DAT         PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
