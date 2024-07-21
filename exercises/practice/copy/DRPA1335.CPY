      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1335.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1335.CPv  $
      *
      *   Rev 2.6   Aug 12 2015 12:24:12   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: MSC
      *
      *   Rev 2.5   Dec 20 2013 10:56:48   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.4   Jun 10 2008 10:03:22   t13ex21
      *APA00712: FVD Stufe 1: Programmänderungen
      *
      *   Rev 2.3   Dec 10 2007 12:44:44   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.2   12 May 2000 08:21:50   T01JMBQ
      *APA00084 - ÄNDERUNGEN
      *
      *   Rev 2.1   25 Apr 2000 10:15:16   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Feb 08 2000 11:30:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1335                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0057 (COPY-1)                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    01.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00084!01.03.00!2.1 ! NEU                       ! KRUFF    *
      * APA00084!12.05.00!2.2 ! ÄNDERUNGEN                !          *
      *         !        !    ! (ALLE ANZ-ZÄHLER RESTART- !          *
      *         !        !    ! FÄHIG)                    !          *
      * APA00622!17.09.07!2.3 ! NEUES FELD HINZU          ! STORCH   *
      * APA00712!25.03.08!2.4 ! NEUE FELDER HINZU         ! STORCH   *
      * APA01526!27.09.13!2.5 ! KALYPSO FELDER HINZU      ! BUCH     *
      * APA01932!10.08.15!2.6 ! NEUTRALISIERUNG VIA KONFIG! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0057 (SST-MA).                              *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1335-RESTART.
           05 DRPA1335-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1335'.
      *
           05 DRPA1335-DATEN.
              10 DRPA1335-S-NZ                      PIC  X(001).
DB2           10 DRPA1335-PGEB-GES                  PIC S9(004) BINARY.
XDB   *       10 DRPA1335-PGEB-GES                  PIC  9(002).
DB2           10 DRPA1335-PRINTE                    PIC S9(015)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-PRINTE                    PIC  9(015).
              10 DRPA1335-PVER-VNR                  PIC  X(015).
270913        10 DRPA1335-PMBE-UNR                  PIC S9(4)
270913                                                      USAGE COMP.
270913        10 DRPA1335-PMBE-SCHICHT-NR           PIC S9(4)
270913                                                      USAGE COMP.
              10 DRPA1335-PMPT-FAELLIG-VON          PIC  X(010).
      *
DB2           10 DRPA1335-ANZ-PAMS                  PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-PAMS                  PIC  9(009).
DB2           10 DRPA1335-ANZ-AFILE1                PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-AFILE1                PIC  9(009).
DB2           10 DRPA1335-ANZ-AFILE2                PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-AFILE2                PIC  9(009).
DB2           10 DRPA1335-ANZ-AFILE3                PIC S9(009)
DB2                                                     PACKED-DECIMAL.
250308        10 DRPA1335-ANZ-AFILE4                PIC S9(009)
250308                                                  PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-AFILE3                PIC  9(009).
DB2           10 DRPA1335-ANZ-PDPE-NEUTR-VN         PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-PDPE-NEUTR-VN         PIC  9(009).
DB2           10 DRPA1335-ANZ-PDPE-NEUTR            PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-PDPE-NEUTR            PIC  9(009).
DB2           10 DRPA1335-ANZ-PDPT-NEUTR            PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-PDPT-NEUTR            PIC  9(009).
DB2           10 DRPA1335-ANZ-DATENSATZ-AFILE1      PIC S9(009)
DB2                                                     PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-DATENSATZ-AFILE1      PIC  9(009).
DB2           10 DRPA1335-ANZ-DATENSATZ-AFILE2      PIC S9(009)
DB2                                                     PACKED-DECIMAL.
170907        10 DRPA1335-ANZ-DATENSATZ-AFILE3      PIC S9(009)
170907                                                  PACKED-DECIMAL.
250308        10 DRPA1335-ANZ-DATENSATZ-AFILE4      PIC S9(009)
250308                                                  PACKED-DECIMAL.
XDB   *       10 DRPA1335-ANZ-DATENSATZ-AFILE2      PIC  9(009).
              10 DRPA1335-AUFSETZPUNKT              PIC  X(002).
DB2           10 DRPA1335-KNR-VERTRIEB-GES          PIC S9(004) BINARY.
XDB   *       10 DRPA1335-KNR-VERTRIEB-GES          PIC  9(002).
100815        10 DRPA1335-S-NEUTRALISIERUNG         PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

