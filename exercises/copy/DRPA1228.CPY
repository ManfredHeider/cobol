      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1228.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1228.cpv  $
      *
      *   Rev 2.1   May 14 1999 09:04:22   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1229
      *
      *   Rev 2.0   Mar 30 1999 08:44:08   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1228                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. ABSCHL. ZU GES >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    10.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!10.05.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1228                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1228.
          05 PI-DRPA1228-DATEN.
             10 PI-DRPA1228-PROGRAMM                PIC  X(008).
             10 PI-DRPA1228-UFKT                    PIC S9(004) BINARY.
             10 PI-DRPA1228-PGEB-GES                PIC S9(004) BINARY.
      *
       01 PO-DRPA1228.
          05 PO-DRPA1228-RC                         PIC  X(003).
             88 PO-DRPA1228-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1228-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1228-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1228-DATEN.
             10 PO-DRPA1228-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1228-KNR-WAEHRUNG            PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
