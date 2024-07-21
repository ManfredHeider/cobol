      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1341.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1341.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:03:36   t13ex21
      *APA00712: FVD Stufe 1: Programmänderungen
      *
      *   Rev 2.1   25 Apr 2000 10:17:04   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Mar 01 2000 09:57:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1341                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0057 (COPY-7)                  *
      *       FÜR OPERATOR-INFO                                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    01.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00084!01.03.00! 01 ! NEU                       ! KRUFF    *
      * APA00712!25.03.07!2.02! NEUES FELD HINZU          ! STORCH   *
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
       01  DRPA1341-RESTART.
           05 DRPA1341-ZEILE-1.
              10 DRPA1341-ID.
                 15 FILLER                          PIC  X(008)
                                                      VALUE 'DRPA1341'.
                 15 FILLER                          PIC  X(018)
                                            VALUE ' - OPERATOR-INFO: '.
                 15 Z-RESTART-PGM                   PIC  X(008).
                 15 FILLER                          PIC  X(001)
                                                      VALUE SPACE.
                 15 Z-RESTART-DATE                  PIC  X(010).
                 15 FILLER                          PIC  X(001)
                                                      VALUE SPACE.
                 15 Z-RESTART-TIME                  PIC  X(008).
                 15 FILLER                          PIC  X(026)
                                                      VALUE SPACE.
      *
           05 DRPA1341-ZEILE-2.
              10 FILLER                             PIC  X(080)
                                                      VALUE ALL '-'.
      *
           05 DRPA1341-ZEILE-3.
              10 FILLER                             PIC  X(034)
                            VALUE 'Anzahl eingelesener PAMS-Daten:   '.
              10 DRPA1341-ANZ-PAMS                  PIC  9(009).
              10 FILLER                             PIC  X(035)
                                                      VALUE SPACE.
      *
           05 DRPA1341-ZEILE-4.
              10 FILLER                             PIC  X(034)
                            VALUE 'Anzahl ausgegebener Sätze AFILE1: '.
              10 DRPA1341-ANZ-AFILE1                PIC  9(009).
              10 FILLER                             PIC  X(035)
                                                      VALUE SPACE.
      *
           05 DRPA1341-ZEILE-5.
              10 FILLER                             PIC  X(034)
                            VALUE 'Anzahl ausgegebener Sätze AFILE2: '.
              10 DRPA1341-ANZ-AFILE2                PIC  9(009).
              10 FILLER                             PIC  X(035)
                                                      VALUE SPACE.
      *
           05 DRPA1341-ZEILE-6.
              10 FILLER                             PIC  X(034)
                            VALUE 'Anzahl ausgegebener Sätze AFILE3: '.
              10 DRPA1341-ANZ-AFILE3                PIC  9(009).
              10 FILLER                             PIC  X(035)
                                                      VALUE SPACE.
      *
250308     05 DRPA1341-ZEILE-7.
              10 FILLER                             PIC  X(034)
                            VALUE 'ANZAHL AUSGEGEBENER SÄTZE AFILE4: '.
              10 DRPA1341-ANZ-AFILE4                PIC  9(009).
              10 FILLER                             PIC  X(035)
                                                      VALUE SPACE.
      *
      ******* ENDE COPY-MEMBER **************************************
