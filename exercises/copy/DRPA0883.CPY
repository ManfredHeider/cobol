      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0883.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0883.CPv  $
      *
      *   Rev 2.1   May 05 1999 15:45:20   T01JM2S
      *APA00012 - GSMT-PROV MIT NACHKOMMASTELLEN
      *
      *   Rev 2.0   Feb 27 1999 14:29:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:43:52   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:48:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0883                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DTA/FKT-BER. SUMME  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    04.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.03.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!05.05.99!2.1 ! GSMT-PROV MIT NACHKOMMA-  ! WOLFF    *
      *         !        !    ! STELLEN DEFINIERT         !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0883                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0883.
          05 PI-DRPA0883-DATEN.
             10 PI-DRPA0883-PROGRAMM                PIC  X(008).
             10 PI-DRPA0883-UFKT                    PIC S9(004) BINARY.
             10 PI-DRPA0883-PDTA-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0883-KNR-WAEHRUNG            PIC S9(004) BINARY.
      *
       01 PO-DRPA0883.
          05 PO-DRPA0883-RC                         PIC  X(003).
             88 PO-DRPA0883-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0883-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0883-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0883-DATEN.
             10 PO-DRPA0883-KNR-FKT-BEREICH         PIC S9(004) BINARY.
             10 PO-DRPA0883-KOF-GSMT-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0883-KOF-ANZ-BEWEGUNG        PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
