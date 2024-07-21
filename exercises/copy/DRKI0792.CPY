      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0792.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0792.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 14:15:06   T13EX12
      *AKI00014 - Restart-Parameter fuer PMKI0167
      *
      *   Rev 1.0   06 Oct 1998 15:40:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION   01
      *    Copy Strecke für Restart des PMKI0167 Programmes
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!10.12.98! 01 ! NEUERSTELLUNG             !v.d.Goltz *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI0792-RESTART.
           05 DRKI0792-COPY-NAME      PIC X(008)   VALUE 'DRKI0792'.
           05 DRKI0792-RESTART-VNR    PIC 9(015)   VALUE ZERO.

      *    05 FILLER                  PIC X(3986)  VALUE SPACE.
