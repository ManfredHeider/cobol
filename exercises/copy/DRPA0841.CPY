      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0841.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0841.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:34:06   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:45:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0841                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF MASSEN-UPDATE FÜR STANDARD-OZP ZTPA0126    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    10.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0841                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0841.
          05 PI-DRPA0841-DATEN.
             10 PI-DRPA0841-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0841-KNR-WAEHRUNG            PIC S9(004) BINARY.
             10 PI-DRPA0841-ABSCHLUSS-DAT-ALT       PIC  X(010).
             10 PI-DRPA0841-ABSCHLUSS-DAT-NEU       PIC  X(010).
      *
       01 PO-DRPA0841.
          05 PO-DRPA0841-RC                         PIC  X(003).
             88 PO-DRPA0841-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0841-RC-CHANGED                    VALUE 'CHA'.
      *
      ******* ENDE COPY-MEMBER **************************************
