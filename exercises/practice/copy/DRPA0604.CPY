      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0604.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0604.cpv  $
      *
      *   Rev 2.0   May 27 1999 12:36:52   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:54:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0604  FUNKTION Vertrag ermitteln         *
      *                           (UPPA0077)                         *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    11.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !11.11.97!01.01! NEUERSTELLUNG             ! JÄGER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0604                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0604.
          05 PI-DRPA0604-DATEN.
             10 PI-DRPA0604-PIN              PIC S9(009) BINARY.
             10 PI-DRPA0604-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0604-VNR              PIC  X(015).
             10 PI-DRPA0604-PGEB-GES         PIC S9(004) BINARY.
      *
       01 PO-DRPA0604.
          05 PO-DRPA0604-RC                  PIC  X(003).
             88 PO-DRPA0604-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0604-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRPA0604-RC-SEL-KRIT-NOK          VALUE 'SKN'.
          05 PO-DRPA0604-DATEN.
             10 PO-DRPA0604-PVER.
                COPY DRPA0408.
      *
