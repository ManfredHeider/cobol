      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1412.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1412.CPv  $
      *
      *   Rev 2.3   Sep 19 2017 10:03:50   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.2   Jul 27 2005 13:04:26   t13ae31
      *AKI00469: Anpassungen Central-DTA
      *
      *   Rev 2.1   Jun 03 2002 10:36:56   T13EX27
      *AKI00284 Erweiterung DTA-Kontrolle für CKV
      *
      *   Rev 2.0   Apr 23 2002 16:01:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00284!30.04.02!    ! NEU                       ! Frenzel  *
      *         !        !    !                           !          *
      * AKI00469!28.02.05!    ! Anpassung an neue Schnitt-! Hübner   *
      *         !        !    !       stelle              !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    COPY-Member fuer Bestandssaetze CKV aus dem FTP           *
      *    -----------------------------------------------           *
      *    Feste Satzlaenge 171 Bytes NEU 340                        *
      *                                                              *
      ****************************************************************
      *---------------------------------------------------------------*
       01  CKV-SATZA.
           10  CKV-V-SA                PIC X.
           10  CKV-V-ABSENDER          PIC X(007).
           10  FILLER                  PIC x(020).
           10  CKV-V-DATUM.
               20 CKV-TT               PIC 9(002).
               20 CKV-MM               PIC 9(002).
               20 CKV-JJJJ             PIC 9(004).
           10  FILLER                  PIC X(008).
           10  CKV-V-EMPFAENGER        PIC X(004).
           10  FILLER                  PIC X(023).
           10  CKV-V-TEXT              PIC X(017).
      ***    10  CKV-RESERVEA            PIC X(083).
           10  CKV-RESERVEA            PIC X(252).

       01  CKV-SATZE.
           10  CKV-N-SA                PIC X.
           10  CKV-N-ANZ               PIC 9(007) COMP-3.
      ***    10  CKV-RESERVEE            PIC X(166).
           10  CKV-RESERVEE            PIC X(335).

