      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0669.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0669.CPv  $
      *
      *   Rev 2.1   Feb 15 2000 15:01:02   T13AE21
      * 
      *
      *   Rev 2.0   Feb 27 1999 12:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:28:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0669                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMKI0328                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. Philipp                                       *
      *    DATUM:    16.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00043!16.11.99! 01 ! NEU                       ! PHILIPP  *
      *                                                              *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMKI0328 (Kundenbestandsstatistik PMG1          *
      *                                                              *
      ****************************************************************
      *
       01  DRKI0669-RESTART.
           05 DRKI0669-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRKI0669'.
      *
           05 DRKI0669-DATEN.
              10 DRKI0669-ANZ-EFILE-READ            PIC S9(009) BINARY.
              10  DRKI0669-RESTART-KEY.
                 15  DRKI0669-VERTRAGSID       PIC X(13).
      *       10  DRKI0669-
