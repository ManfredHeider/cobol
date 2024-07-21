      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0878.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0878.cpv  $
      *
      *   Rev 2.1   May 05 1999 10:24:14   T01JM2S
      *APA00012 - ÄNDERUNG DRPA0878
      *
      *   Rev 2.0   Feb 27 1999 14:29:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:41:02   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:48:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0878                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<STAND ANZEIGEN PROV.>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!        ! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!04.05.99!2.1 ! EINBAU PEG-FEHLER, WEGFALL! WOLFF    *
      *         !        !    ! UMBUCHUNG                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0878                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0878.
          05 PI-DRPA0878-DATEN.
             10 PI-DRPA0878-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0878-KNR-WAEHRUNG            PIC S9(004) BINARY.
             10 PI-DRPA0878-LET-MA-ABR-DAT          PIC  X(010).
             10 PI-DRPA0878-STICHTAG                PIC  X(010).
      *
       01 PO-DRPA0878.
          05 PO-DRPA0878-RC                         PIC  X(003).
             88 PO-DRPA0878-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0878-DATEN.
             10 PO-DRPA0878-BATCH-ALT-PROV          PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-BATCH-ALT-BEW           PIC S9(009) BINARY.
             10 PO-DRPA0878-BATCH-NEU-PROV          PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-BATCH-NEU-BEW           PIC S9(009) BINARY.
             10 PO-DRPA0878-BATCH-SALDO-PROV        PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-BATCH-SALDO-BEW         PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-FEHL-ALT-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHL-ALT-BEW            PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHL-NEU-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHL-NEU-BEW            PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHL-SALDO-PROV         PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHL-SALDO-BEW          PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-SUM1-ALT-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-SUM1-ALT-BEW            PIC S9(009) BINARY.
             10 PO-DRPA0878-SUM1-NEU-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-SUM1-NEU-BEW            PIC S9(009) BINARY.
             10 PO-DRPA0878-SUM1-SALDO-PROV         PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-SUM1-SALDO-BEW          PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-FEHLPEG-ALT-PROV        PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLPEG-ALT-BEW         PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHLPEG-NEU-PROV        PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLPEG-NEU-BEW         PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHLPEG-SALDO-PROV      PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLPEG-SALDO-BEW       PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-ZUK-ALT-PROV            PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-ZUK-ALT-BEW             PIC S9(009) BINARY.
             10 PO-DRPA0878-ZUK-NEU-PROV            PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-ZUK-NEU-BEW             PIC S9(009) BINARY.
             10 PO-DRPA0878-ZUK-SALDO-PROV          PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-ZUK-SALDO-BEW           PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-FEHLIFS-ALT-PROV        PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLIFS-ALT-BEW         PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHLIFS-NEU-PROV        PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLIFS-NEU-BEW         PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHLIFS-SALDO-PROV      PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLIFS-SALDO-BEW       PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-FEHLK-ALT-PROV          PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLK-ALT-BEW           PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHLK-NEU-PROV          PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLK-NEU-BEW           PIC S9(009) BINARY.
             10 PO-DRPA0878-FEHLK-SALDO-PROV        PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-FEHLK-SALDO-BEW         PIC S9(009) BINARY.
      *
             10 PO-DRPA0878-SUM2-ALT-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-SUM2-ALT-BEW            PIC S9(009) BINARY.
             10 PO-DRPA0878-SUM2-NEU-PROV           PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-SUM2-NEU-BEW            PIC S9(009) BINARY.
             10 PO-DRPA0878-SUM2-SALDO-PROV         PIC S9(013)V99
                                                        PACKED-DECIMAL.
             10 PO-DRPA0878-SUM2-SALDO-BEW          PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
