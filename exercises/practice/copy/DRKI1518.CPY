      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1518.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1518.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 09:18:02   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.3   Nov 16 2015 10:30:22   t13ex39
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens    WHO
      *
      *   Rev 2.2   Apr 27 2005 14:26:38   t13ex39
      *AKI00249: ART DES DEPOTS
      *
      *   Rev 2.1   Jul 10 2003 13:45:30   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *
      *   Rev 2.0   Feb 27 1999 12:53:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:54:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 5 1 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN GUTHABENPRODUKT FUER DEUTSCHE BANK *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.02.03!NEUERSTELLUNG                          !HORNUNG  ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *01.04.03!                                       !GOLÜCKE  !     *
      *--------!---------------------------------------!---------!-----*
      *18.03.05!DEPOT-ART aufgenommen                  !HORNUNG  ! 02  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *--------------------------------------------------------------*
      * AKI01385!13.11.15!    ! Neues Attribut Provisions-! WHO      *
      *         !        !    ! relevant                  !          *
      *---------!--------!--------------------------------!----------*
      *AKI01805 !14.10.20! NEUES ATTRIBUT ESAFE           ! ATAR     *
      *         !        !                                !          *
      ******************************************************************
       01  DRKI1518.

           05  GHP-GES                     PIC  9(02).
           05  GHP-VNR                     PIC  9(15).
           05  GHP-LFDNR                   PIC  9(02).
      *        NUR Unterkontonummer
           05  GHP-PRODUKT-ID              PIC  S9(04).

           05  GHP-DATUM-EROEFFNUNG        PIC  X(10).
           05  GHP-AEND-DATUM-E-KZ         PIC  X(01).
      *        J WENN ERÖFFNUNGSDATUM GEAENDERT

           05  GHP-DATUM-SCHLIESSUNG       PIC  X(10).
           05  GHP-AEND-DATUM-S-KZ         PIC  X(01).
      *        J WENN SCHLIESSUNGSDATUM GEAENDERT

           05  GHP-VL-KZ                   PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG

           05  GHP-WKZ                     PIC  X(01).
           05  GHP-AEND-WKZ-KZ             PIC  X(01).
      *        J WENN WÄHRUNGSKENNZEICHEN GEAENDERT

           05  GHP-RISIKOKLASSE            PIC  X(01).
      *        Risikoklasse 0 - 9
           05  GHP-AEND-RISIKO-KZ          PIC  X(01).
      *        J WENN Risikoklasse GEAENDERT

           05  GHP-AEND-PRODUKT-ID-KZ      PIC  X(01).
      *        J wenn Produkt-ID GEAENDERT

           05  GHP-ANLAGE-ZIEL             PIC  X(01).
      *        ANLAGEZIEL
           05  GHP-AEND-ANLAGE-ZIEL-KZ     PIC  X(01).
      *        J Wenn geändert

           05  GHP-VERFUEG-KZ              PIC  X(001).
           05  GHP-AEND-VERFUEG-KZ-KZ      PIC  X(001).
           05  GHP-TELREVERS               PIC  X(001).
           05  GHP-AEND-TELREVERS-KZ       PIC  X(001).
           05  GHP-ONLREVERS               PIC  X(001).
           05  GHP-AEND-ONLREVERS-KZ       PIC  X(001).

           05  GHP-EIGEN-KZ                PIC  X(001).

           05  GHP-AKTUAL-KZ               PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
180305     05  GHP-DEPOT-ART               PIC  X(01).
180305     05  GHP-AEND-DEPOT-ART-KZ       PIC  X(01).

131115     05  GHP-PROV-REL                PIC  X(01).
311115     05  GHP-AEND-PROV-REL-KZ        PIC  X(01).

           05  GHP-ESAFE                   PIC  X(001).
           05  GHP-AEND-ESAFE-KZ           PIC  X(001).

131115     05  GHP-FILLER                  PIC  X(33).
311115*    05  FILLER                      PIC  X(37).
      *        Reserve
      ******************************************************************


