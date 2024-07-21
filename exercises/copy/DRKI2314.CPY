      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2314.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2314.CPv  $
      *
      *   Rev 2.4   Mar 26 2017 08:51:34   t13ex39
      *AKI01494: FoDB Premium Depot 
      *
      *   Rev 2.3   Mar 26 2017 08:41:04   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.2   Mar 26 2017 08:27:44   t13ex39
      *AKI01494: FoDB Premium Depot
      *
      *   Rev 2.1   Jun 15 2009 16:34:02   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik
      *
      *   Rev 2.0   Mar 19 2009 14:54:18   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 3 1 4    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN GUTHABENPRODUKT FUER AGI           *
      *                                                                *
      *    SATZLÄNGE   104                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *26.01.09!NEUERSTELLUNG                          !HORNUNG  ! 01  *
      *AKI00740!                                       !         !     *
      * -------------------------------------------------------------*
010217* AKI01494!01.02.17!    ! Premiumdepot bzw.         ! WHO      *
      *         !        !    ! Premiummandat             !          *
      *         !        !    ! REVERS-Felder entfernt    !          *
      *         !        !    ! GHP-EIGEN-KZ-FIX entfrent !          *
      *         !        !    ! GHP-VERFUEGUNGS-KZ entfernt          *
      * -------------------------------------------------------------*
      * DBO-9297!08.10.21!    ! RISKOKLASSE WIEDE AUFGE-  ! T13EX39  *
      *         !        !    ! NOMMEN                    !          *
      *         !        !    !                           !          *
      ******************************************************************
       01  DRKI2314.

           05  GHP-GES                     PIC  9(02).
           05  GHP-VNR                     PIC  9(15).
           05  GHP-LFDNR                   PIC  9(02).
      *        NUR Unterkontonummer
           05  GHP-PRODUKT-ID              PIC  S9(04).

           05  GHP-DATUM-EROEFFNUNG        PIC  X(10).

           05  GHP-DATUM-SCHLIESSUNG       PIC  X(10).

           05  GHP-VL-KZ                   PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG

           05  GHP-WKZ                     PIC  X(01).

           05  GHP-DEPOT-ART               PIC  X(01).

           05  GHP-AKTUAL-KZ               PIC  X(01).

           05  GHP-RISIKO-KLASSE           PIC  9(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
010217     05  FILLER                      PIC  X(56).
      *        Reserve
      ******************************************************************



