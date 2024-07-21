      *****************************************************************
      *    COPY-MEMBER: DRAT0050   KNR-KONST. FUER MAT-AUSTRITTSGRUND *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHOELLHAMMER                                    *
      *    DATUM:    10.05.1993                                       *
      *****************************************************************
      *    VERSION   1    04.07.95 17:10:59
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !16.09.93! 01 ! NEU                       ! SCHÖLLH. *
      * FEVB0264!04.07.95! 02 ! NEU: AUSTRITTSGRUND 50    ! SCHÖLLH. *
      * AVB00954!11.01.08!2.3 ! Neu: Austrittsgrund 21    ! S. Walb  *
      *         !25.01.08!2.4 ! Probleme beim Check-in    !          *
      * AVB00996!24.11.08!2.5 ! 25 und 75 ändern          ! S. Walb  *
      * ACV00156!18.11.15!2.6 ! neu: Austrittsgrund 40    ! S. Walb  *
      *                                                              *
261021* VLA-3599!26.10.21! AUSTRITTSGRUND 41 HINZU        ! KLEEMEIER*
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE FÜR MAT-AUSTRITTSGRUND
      *
       01  FILLER.
           05  K-KNR-OHNE-KUEND               PIC S9(004) COMP VALUE 00.
 
           05  K-KNR-KUEVB-VB-OHNE-BEGRUEND   PIC S9(004) COMP VALUE 10.
           05  K-KNR-KUEVB-VB-UNZUFR-BETREU   PIC S9(004) COMP VALUE 11.
           05  K-KNR-KUEVB-VB-WGRD-ALLG       PIC S9(004) COMP VALUE 16.
           05  K-KNR-KUEVB-VB-WGRD-KONKURR    PIC S9(004) COMP VALUE 17.
           05  K-KNR-KUEVB-VB-SONSTIGES       PIC S9(004) COMP VALUE 18.
 
           05  K-KNR-KUEVB-GESL-OHNE-BEGRUEND PIC S9(004) COMP VALUE 20.
110108     05  K-KNR-KUEVB-GESL-GEW-ZENTR-REG PIC S9(004) COMP VALUE 21.
           05  K-KNR-KUEVB-GESL-INAKTIV       PIC S9(004) COMP VALUE 22.
           05  K-KNR-KUEVB-GESL-SOLL-SALDO    PIC S9(004) COMP VALUE 23.
           05  K-KNR-KUEVB-GESL-NEG-AUSKUNFT  PIC S9(004) COMP VALUE 24.
241108*    05  K-KNR-KUEVB-GESL-TOD           PIC S9(004) COMP VALUE 25.
241108     05  K-KNR-KUEVB-GESL-SCHUFA-FZ-GEW PIC S9(004) COMP VALUE 25.
           05  K-KNR-KUEVB-GESL-WGRD-ALLG     PIC S9(004) COMP VALUE 26.
           05  K-KNR-KUEVB-GESL-WGRD-KONKURR  PIC S9(004) COMP VALUE 27.
           05  K-KNR-KUEVB-GESL-SONSTIGES     PIC S9(004) COMP VALUE 28.
           05  K-KNR-KUEVB-GESL-RUECKSTUF-VM  PIC S9(004) COMP VALUE 29.
 
           05  K-KNR-KUEVB-EINV-OHNE-BEGRUEND PIC S9(004) COMP VALUE 30.
           05  K-KNR-KUEVB-EINV-UNZUFR-BETREU PIC S9(004) COMP VALUE 31.
           05  K-KNR-KUEVB-EINV-INAKTIV       PIC S9(004) COMP VALUE 32.
           05  K-KNR-KUEVB-EINV-SOLL-SALDO    PIC S9(004) COMP VALUE 33.
           05  K-KNR-KUEVB-EINV-NEG-AUSKUNFT  PIC S9(004) COMP VALUE 34.
           05  K-KNR-KUEVB-EINV-TOD           PIC S9(004) COMP VALUE 35.
           05  K-KNR-KUEVB-EINV-WGRD-ALLG     PIC S9(004) COMP VALUE 36.
           05  K-KNR-KUEVB-EINV-WGRD-KONKURR  PIC S9(004) COMP VALUE 37.
           05  K-KNR-KUEVB-EINV-SONSTIGES     PIC S9(004) COMP VALUE 38.
           05  K-KNR-KUEVB-EINV-RUECKSTUF-VM  PIC S9(004) COMP VALUE 39.

181115     05  K-KNR-KUEVB-GESL-WEISUNG-IHK   PIC S9(004) COMP VALUE 40.
261021     05  K-KNR-KUEVB-GESL-FEHL-DBBV     PIC S9(004) COMP VALUE 41.
 
040795     05  K-KNR-FEHLERHAFTE-ERFASSUNG    PIC S9(004) COMP VALUE 50.
 
           05  K-KNR-KUEVM-VM-OHNE-BEGRUEND   PIC S9(004) COMP VALUE 60.
           05  K-KNR-KUEVM-VM-UNZUFR-BETREU   PIC S9(004) COMP VALUE 61.
           05  K-KNR-KUEVM-VM-WGRD-ALLG       PIC S9(004) COMP VALUE 66.
           05  K-KNR-KUEVM-VM-WGRD-KONKURR    PIC S9(004) COMP VALUE 67.
           05  K-KNR-KUEVM-VM-SONSTIGES       PIC S9(004) COMP VALUE 68.
 
           05  K-KNR-KUEVM-GESL-OHNE-BEGRUEND PIC S9(004) COMP VALUE 70.
           05  K-KNR-KUEVM-GESL-INAKTIV       PIC S9(004) COMP VALUE 72.
           05  K-KNR-KUEVM-GESL-SOLL-SALDO    PIC S9(004) COMP VALUE 73.
           05  K-KNR-KUEVM-GESL-NEG-AUSKUNFT  PIC S9(004) COMP VALUE 74.
241108*    05  K-KNR-KUEVM-GESL-TOD           PIC S9(004) COMP VALUE 75.
241108     05  K-KNR-KUEVM-GESL-SCHUFA-FZ     PIC S9(004) COMP VALUE 75.
           05  K-KNR-KUEVM-GESL-WGRD-ALLG     PIC S9(004) COMP VALUE 76.
           05  K-KNR-KUEVM-GESL-WGRD-KONKURR  PIC S9(004) COMP VALUE 77.
           05  K-KNR-KUEVM-GESL-SONSTIGES     PIC S9(004) COMP VALUE 78.
 
           05  K-KNR-KUEVM-EINV-OHNE-BEGRUEND PIC S9(004) COMP VALUE 80.
           05  K-KNR-KUEVM-EINV-UNZUFR-BETREU PIC S9(004) COMP VALUE 81.
           05  K-KNR-KUEVM-EINV-INAKTIV       PIC S9(004) COMP VALUE 82.
           05  K-KNR-KUEVM-EINV-SOLL-SALDO    PIC S9(004) COMP VALUE 83.
           05  K-KNR-KUEVM-EINV-NEG-AUSKUNFT  PIC S9(004) COMP VALUE 84.
           05  K-KNR-KUEVM-EINV-TOD           PIC S9(004) COMP VALUE 85.
           05  K-KNR-KUEVM-EINV-WGRD-ALLG     PIC S9(004) COMP VALUE 86.
           05  K-KNR-KUEVM-EINV-WGRD-KONKURR  PIC S9(004) COMP VALUE 87.
           05  K-KNR-KUEVM-EINV-SONSTIGES     PIC S9(004) COMP VALUE 88.
      *
      ******** ENDE COPY-MEMBER DRAT0050  KNR-K. MAT-AUSTRITTSGRUND ***

