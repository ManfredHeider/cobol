      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0302.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRAT0302.CPv  $
      *
      *   Rev 2.3   12 Sep 2001 14:36:24   t13ae46
      *APA00144: K-KNR-MAN-DTA-SICHERN ergaenzt
      *
      *   Rev 2.2   Apr 04 2000 08:50:44   T13EX21
      *APA00088 - Neue KNR = 19
      *
      *   Rev 2.1   Nov 25 1999 09:07:46   T13EX21
      *APA00063 - Neuerstellung
      *
      *   Rev 2.0   Mar 16 1999 09:57:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0302  KONSTANTEN FÜR KTY = 174           *
      *                           (DTA-BEARBEITUNGSFUNKTIONEN)       *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    06.10.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00063!06.10.99!2.01 ! NEUERSTELLUNG             ! STORCH  *
      * APA00088!15.03.00!2.01 ! NEUE KNR = 19             ! STORCH  *
      * APA00144!14.08.01!2.3  ! K-KNR-MAN-DTA-SICHERN     ! WANG    *
      *         !        !     ! ERGÄNZT                   !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-DTA-ERSTELLEN        PIC S9(004) BINARY VALUE 1.
           05 K-KNR-PEG-BATCH            PIC S9(004) BINARY VALUE 3.
           05 K-KNR-DTA-SPERREN          PIC S9(004) BINARY VALUE 5.
           05 K-KNR-PEG-FEHLERB-O106     PIC S9(004) BINARY VALUE 6.
           05 K-KNR-PEG-FEHLERB-DIALOG   PIC S9(004) BINARY VALUE 7.
           05 K-KNR-MAN-PM-AUFGEBEN      PIC S9(004) BINARY VALUE 8.
           05 K-KNR-DTA-SPLITTEN         PIC S9(004) BINARY VALUE 9.
           05 K-KNR-DTA-FREIGABE-PVW     PIC S9(004) BINARY VALUE 11.
           05 K-KNR-PVW-BATCH            PIC S9(004) BINARY VALUE 13.
           05 K-KNR-PVW-ONLINE           PIC S9(004) BINARY VALUE 15.
           05 K-KNR-DTA-FREIGABE-SST-MA  PIC S9(004) BINARY VALUE 17.
150300     05 K-KNR-SST-MA               PIC S9(004) BINARY VALUE 19.
140801     05 K-KNR-MAN-DTA-SICHERN      PIC S9(004) BINARY VALUE 10.
