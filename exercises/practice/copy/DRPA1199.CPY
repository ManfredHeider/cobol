      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1199.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1199.cpv  $
      *
      *   Rev 2.1   Mar 31 1999 09:51:12   T13EX22
      *Sichern verarbeitete nPM's
      *
      *   Rev 2.0   Mar 25 1999 12:09:16   T13EX22
      *MFE Umstellung
      *
      *   Rev 1.0   21 Oct 1998 14:05:40   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1199                                      *
      *    --------------------------------------------------------   *
      *    WORKFLOW VERTRAGSDATEN                                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.03.1999                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * APA00012!22.03.99! 01 ! NEU                       ! MUND       *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *                                                                *
      *    DIESES COPY ENTHÄLT DEN SATZAUFBAU FÜR DIE DATEI PVW3200    *
      *                                                                *
      ******************************************************************
      *
          10 DRPA1199-DATEN.
             15 PMPG-ID                PIC S9(015) PACKED-DECIMAL.
             15 PMPG-VERSION           PIC S9(004) BINARY.
             15 PMPE-VNR               PIC  X(015).
             15 PMPE-PIN               PIC S9(009) BINARY.
             15 PMPP-TABELLE.
                20 MAX-USED-PMPP       PIC S9(004) BINARY.
                20 PMPP-ID             PIC S9(015) PACKED-DECIMAL
                                       OCCURS 10.
      *
      ******* ENDE COPY-MEMBER *****************************************
