      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0880.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0880.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:42:10   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:48:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0880                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<SUM. PGEB/KOMBI. 2  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.03.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0880                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0880.
          05 PI-DRPA0880-DATEN.
             10 PI-DRPA0880-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0880-KNR-FKT-BEREICH-AB      PIC S9(004) BINARY.
             10 PI-DRPA0880-KNR-FKT-BEREICH-ZU      PIC S9(004) BINARY.
             10 PI-DRPA0880-KNR-WAEHRUNG            PIC S9(004) BINARY.
             10 PI-DRPA0880-STICHTAG-VON            PIC  X(010).
             10 PI-DRPA0880-STICHTAG-BIS            PIC  X(010).
      *
       01 PO-DRPA0880.
          05 PO-DRPA0880-RC                         PIC  X(003).
             88 PO-DRPA0880-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0880-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0880-DATEN.
             10 PO-DRPA0880-GSMT-PROV               PIC S9(013)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRPA0880-ANZ-BEW-AB              PIC S9(009) BINARY.
             10 PO-DRPA0880-ANZ-BEW-ZU              PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
