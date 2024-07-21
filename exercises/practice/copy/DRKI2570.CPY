      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2570.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2570.CPv  $
      *
      *   Rev 2.1   Sep 20 2012 10:28:58   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung: VN-Wechsel
      *
      *   Rev 2.0   Mar 08 2012 14:30:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *   Konstanten-Copy für Bestandsfunktionen                     *
      *                                                              *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!08.03.12!2.1 ! NEU                       ! IFR      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  NEUKUNDE.
      *    Bewertungskennzeichen
           05  K-NKD-BWKZ-NEUKUNDE          PIC X(002) VALUE 'NK'.
           05  K-NKD-BWKZ-WIN-BACK          PIC X(002) VALUE 'WK'.
           05  K-NKD-BWKZ-ABGANG            PIC X(002) VALUE 'AK'.
           05  K-NKD-BWKZ-GELOESCHT         PIC X(002) VALUE 'LK'.

      *    Ereigniskennzeichen
           05  K-NKD-EKZ-NEUER-VERTRAG      PIC S9(004) COMP VALUE 1.
           05  K-NKD-EKZ-NEUER-KONTOIN      PIC S9(004) COMP VALUE 2.
           05  K-NKD-EKZ-VN-ABGANG          PIC S9(004) COMP VALUE 3.
           05  K-NKD-EKZ-VERTRAGSABGANG     PIC S9(004) COMP VALUE 4.
           05  K-NKD-EKZ-WIK                PIC S9(004) COMP VALUE 5.
           05  K-NKD-EKZ-VN-WECHSEL         PIC S9(004) COMP VALUE 6.
           05  K-NKD-EKZ-KUZUSAFU           PIC S9(004) COMP VALUE 7.
           05  K-NKD-EKZ-KORR-STATUS        PIC S9(004) COMP VALUE 8.
           05  K-NKD-EKZ-BEREINIGUNG        PIC S9(004) COMP VALUE 9.

       01  HISTADRESSAENDERG.
      *    Änderungsgrund
           05  K-HAD-AEGR-UMZUG             PIC S9(004) COMP VALUE 1.
           05  K-HAD-AEGR-SCHREIBWEISE      PIC S9(004) COMP VALUE 2.
           05  K-HAD-AEGR-SONSTIGE          PIC S9(004) COMP VALUE 3.

      *    Status
           05  K-HAD-STAT-VON-TICKET        PIC S9(004) COMP VALUE 1.
           05  K-HAD-STAT-AN-PARTNER        PIC S9(004) COMP VALUE 2.
           05  K-HAD-STAT-VON-PARTNER       PIC S9(004) COMP VALUE 3.
           05  K-HAD-STAT-VERWORFEN-P       PIC S9(004) COMP VALUE 4.
           05  K-HAD-STAT-N-AN-PARTNER      PIC S9(004) COMP VALUE 5.
           05  K-HAD-STAT-VERWORFEN-AD      PIC S9(004) COMP VALUE 6.
           05  K-HAD-STAT-GLEICHE-ADR       PIC S9(004) COMP VALUE 7.
           05  K-HAD-STAT-GELEOSCHT-3-MON   PIC S9(004) COMP VALUE 8.






