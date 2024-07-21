      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0425.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0425.CPv  $
      *
      *   Rev 2.2   07 Nov 2007 13:12:42   t13ae35
      *ARW00048: Zwischensummen im Buchungszeilen der VB-Abrechnung
      *
      *   Rev 2.1   08 Nov 2006 16:51:04   t13ae35
      *AIS00060: Summenblatt: Füllen SONSTBUCH
      *
      *   Rev 2.0   24 Oct 2006 11:03:58   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0425  KONSTANTEN FÜR KTY = 97            *
      *                           Buchungstypen                      *
      *    ERSTELLT: J. Hering                                       *
      *    DATUM:    26.10.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00060!26.10.06!2.1 ! NEU                       ! J.Hering *
      * ARW00048!05.11.07!2.2 ! weitere Konstanten hinzu  ! J.Hering *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-ABSC-KA              PIC S9(004) BINARY VALUE    1.
           05 K-KNR-RUEC-KA              PIC S9(004) BINARY VALUE   10.
           05 K-KNR-DARL-GEWAEHR         PIC S9(004) BINARY VALUE  100.
           05 K-KNR-DARL-TILGUNG         PIC S9(004) BINARY VALUE  101.
           05 K-KNR-DISK-KA00            PIC S9(004) BINARY VALUE 1000.
051107     05 K-KNR-DISK-SONST-AUS04     PIC S9(004) BINARY VALUE 1005.
051107     05 K-KNR-DISK-KA01            PIC S9(004) BINARY VALUE 1010.
051107     05 K-KNR-DISK-KA02            PIC S9(004) BINARY VALUE 1020.
051107     05 K-KNR-DISK-UMB-RUECK       PIC S9(004) BINARY VALUE 1100.
           05 K-KNR-DISK-DIM7-AUS00      PIC S9(004) BINARY VALUE 1200.
051107     05 K-KNR-DISK-KA03            PIC S9(004) BINARY VALUE 1210.
051107     05 K-KNR-DISK-DIM7-AUS01      PIC S9(004) BINARY VALUE 1220.
           05 K-KNR-DISK-SONST-AUS00     PIC S9(004) BINARY VALUE 1300.
051107     05 K-KNR-DISK-KA04            PIC S9(004) BINARY VALUE 1310.
           05 K-KNR-DISK-SONST-AUS02     PIC S9(004) BINARY VALUE 1320.
           05 K-KNR-DISK-SONST-AUS03     PIC S9(004) BINARY VALUE 1330.
           05 K-KNR-DISK-DARL-ZINS00     PIC S9(004) BINARY VALUE 1400.
           05 K-KNR-DISK-DARL-ZINS01     PIC S9(004) BINARY VALUE 1410.
           05 K-KNR-DISK-DARL-TILG00     PIC S9(004) BINARY VALUE 1500.
           05 K-KNR-DISK-SONST-GUB00     PIC S9(004) BINARY VALUE 1600.
           05 K-KNR-KEINE-ZUORDNUNG      PIC S9(004) BINARY VALUE 9999.


