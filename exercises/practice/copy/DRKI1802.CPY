      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1802.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI1802.CPv  $
      *
      *   Rev 2.8   Mar 09 2017 17:45:32   t13ac14
      *AKI01492: AML-Änderungen zum RT 03/2017-Riester
      *
      *   Rev 2.7   Apr 21 2009 10:44:34   t13ex48
      *AKI00767: CR 14112 - Historisierung Riesterdaten-RIESTERZULAGE
      *
      *   Rev 2.6   Apr 21 2009 09:08:12   t13ex48
      *AKI00767: CR 14112 - Historisierung Riesterdaten-RIESTERZULAGE
      *
      *   Rev 2.5   Apr 15 2009 16:24:06   t13ex48
      *AKI00767: CR 14112 - Historisierung Riesterdaten-RIESTERZULAGE
      *
      *   Rev 2.4   Mar 30 2009 11:57:22   t13ex48
      *AKI00767: CR 14112 - Historisierung Riesterdaten-RIESTERZULAGE
      *
      *   Rev 2.3   10 Jun 2008 16:21:04   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.2   04 Jun 2008 16:03:54   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.1   Sep 20 2005 08:52:12   t13ex49
      *AKI00483: Riester Förderung - PGM und COPY's - BTGVERF
      *
      *   Rev 2.0   May 17 2005 12:41:56   t13ex49
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *              #####################
      *     ########    D R K I 1 8 0 2    ########
      *              #####################
      *
      *  COPY-MEMBER FUER Aktualisierungsprogramm AML-Riesterförderung
      *
      *    SATZLÄNGE   92
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00483!13.05.05!    ! NEU                       !P. Pohl   *
      * AKI00711!09.04.08!2.2 ! neu Dauerzulagen-KZ       !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI00767!12.03.09!2.3 ! Loeschen der Felder:      ! HSE      *
      *         !        !    ! - R-BTGVERF               !          *
      *         !        !    ! - R-AEND-BTGVERF-KZ       !          *
      *         !        !    ! - R-DAUZULANTRKZN und     !          *
      *         !        !    ! - R-AEND-Zul-Beitrags*-KZ !          *
      * -------------------------------------------------------------*
      * AKI01492!23.02.17!2.8 ! neue Felder:              !von Helden*
      *         !        !    ! - R-KUERZ-SL              !          *
      *         !        !    ! - R-AEND-KUERZ-SL         !          *
      *         !        !    ! - R-KEIN-ANSPR-SL         !          *
      *         !        !    ! - R-AEND-KEIN-ANSPR-SL;   !          *
      *         !        !    ! Änderung Länge Reserve    !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      ****************************************************************
       01  DRKI1802.
      *
           02 AML-RIESTER.
           05 R-GES                           PIC  9(02).
           05 R-VNR                           PIC  9(15).
      *
           05 R-VERTRAGSID                    PIC  X(13).
      *
120309*    05 R-BTGVERF                       PIC  X(01).
120309*    05 R-AEND-BTGVERF-KZ               PIC  X(01).
      *       J WENN BTGVERF GEAENDERT
      *
           05 R-Zul-Datum                     PIC  9(08).
      *       Eingang der Zulage bzw. Zulagekorrektur JJJJMMTT
           05 R-AEND-Zul-Datum-KZ             PIC  X(01).
      *       J WENN Zulagedatum GEAENDERT
      *
           05 R-Zul-Beitragsjahr              PIC  9(04).
      *       Beitragsjahr der Zulage JJJJ
120309*    05 R-AEND-Zul-Beitragsjahr-KZ      PIC  X(01).
      *       J WENN Beitragsjahr GEAENDERT
      *
           05 R-Zul-Wert                      PIC S9(13)V99.
      *       Höhe der Zulage (Summe pro Jahr)
           05 R-AEND-Zul-Wert-KZ              PIC  X(01).
      *       J WENN Zulagewert GEAENDERT
      *
           05 R-Zul-Wert-geliefert            PIC S9(13)V99.
      *       Höhe der Zulage lt. Lieferung
      *       Feldinhalt hat hier nur dokumentarischen Wert
      *
           05 R-WKZ                           PIC  X(01).
           05 R-AEND-WKZ                      PIC  X(01).
      *       J WENN WAEHRUNGSÄNDERUNG
230217*
230217     05 R-KUERZ-SL                      PIC  X(02).
230217     05 R-AEND-KUERZ-SL                 PIC  X(01).
230217*       J WENN Kürzungsschlüssel geändert
230217*
230217     05 R-KEIN-ANSPR-SL                 PIC  X(02).
230217     05 R-AEND-KEIN-ANSPR-SL            PIC  X(01).
230217*       J WENN Anspruchschlüssel geändert
      *
120309*    Dauerzulagen-KZ geloescht
090408*    05 R-DAUZULANTRKZN                 PIC X(01).
090408*       Dauerzulagekennzeichen
090408*    05 R-AEND-DAUZULANTRKZN-KZ         PIC X(01).
090408*       J wenn Dauerzulagekennzeichen geändert
      *
090408*    05 R-Reserve                       PIC  X(12).
090408*    05 R-Reserve                       PIC  X(10).
120309*    05 R-Reserve                       PIC  X(15).
230217     05 R-Reserve                       PIC  X(09).
      *       Reservefeld
      *
           05 R-AKTUAL-KZ                     PIC  X(01).

