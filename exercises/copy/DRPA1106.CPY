      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1106.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1106.CPv  $
      *
      *   Rev 2.2   06 Aug 1999 17:27:52   T01JM2E
      *APA00012 - Satzzähler für AFILE's
      *
      *   Rev 2.1   May 27 1999 10:51:56   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   21 Oct 1998 13:56:50   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1106                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0035                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    07.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.10.98! 01 ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0035 (SST-MA).                              *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1106-RESTART.
           05 DRPA1106-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1106'.
      *
           05 DRPA1106-DATEN.
              10 DRPA1106-ANZ-EFILE-READ            PIC S9(009) BINARY.
              10 DRPA1106-ANZ-AFILE1                PIC S9(009) BINARY.
              10 DRPA1106-ANZ-AFILE2                PIC S9(009) BINARY.
              10 DRPA1106-ANZ-AFILE3                PIC S9(009) BINARY.
