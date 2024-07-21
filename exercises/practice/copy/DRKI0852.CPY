      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0852.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0852.CPv  $
      *
      *   Rev 2.2   May 04 1999 09:21:42   T13AE21
      *AKI00013
      *Copystrecke fuer Restart PMKI0249
      *
      *   Rev 2.1   Apr 29 1999 14:39:38   T13AE21
      *
      *
      *   Rev 2.0   Apr 29 1999 13:56:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION   01
      *    Copy-Strecke für Restart von PMKI0852
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00013!29.04.99! 01 ! NEUERSTELLUNG             !PHILIPP   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI0852-RESTART.
           05 DRKI0852-COPY-NAME      PIC X(008)   VALUE 'DRKI0852'.
           05 DRKI0852-RESTART-ID     PIC X(013)   VALUE ZERO.

           05 FILLER                  PIC X(3979)  VALUE SPACE.
