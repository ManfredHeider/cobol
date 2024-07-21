      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0627.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0627.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:07:34   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:56:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0627  FUNKTION Vertrag ermitteln         *
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
      *    FÜR DIE FUNKTION DRPA0627                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0627.
          05 PI-DRPA0627-DATEN.
             10 PI-DRPA0627-PIN              PIC S9(009) BINARY.
             10 PI-DRPA0627-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0627-VNR              PIC  X(015).
             10 PI-DRPA0627-PGEB-GES         PIC S9(004) BINARY.
      *
       01 PO-DRPA0627.
          05 PO-DRPA0627-RC                  PIC  X(003).
             88 PO-DRPA0627-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0627-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRPA0627-RC-LEER                  VALUE 'LER'.
             88 PO-DRPA0627-RC-SEL-KRIT-NOK          VALUE 'SKN'.
             88 PO-DRPA0627-RC-KEY-NOT-UNIQUE        VALUE 'KNU'.
          05 PO-DRPA0627-DATEN.
             10 PO-DRPA0627-PVER.
                COPY DRPA0408.
             10 PO-DRPA0627-PVPT.
                COPY DRPA0420.
      *
