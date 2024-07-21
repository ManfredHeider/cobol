      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.05.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3790.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3790.CPv  $
      *
      *   Rev 2.1   Jul 07 2020 13:01:04   t13ex78
      *AKI01780: Programme und Copies
      *
      *   Rev 2.0   May 20 2020 09:00:02   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      ******************************************************************
      *                                                                *
      *                                                                *
      * G90 - EOS                                                      *
      *    Vertragsdaten - Ausgabe Ermittlung
      *                                                                *
      *    Satzlänge 500 Bytes                                         *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01780!22.06.20!2.0 ! Neu                         ! t13ex78   *
      *        !        !    !                             !           *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-AEND-SATZ.
      *
      * Änderungskennzeichen - I oder U'
           05  'XX'-AEND-AKTU-KZN              PIC  X(001).
      *
      * Gesellschaft - immer '90'
           05  'XX'-AEND-GESELLSCHAFT          PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           05  'XX'-AEND-VNR                   PIC  X(015).
      *
      * Abschlussvermittler
           05  'XX'-AEND-VBNR                  PIC  9(007).
      *
      * Vertragszustand
      * A - aktiv
      * D - abgelaufen
      * S - Infosperre
           05  'XX'-AEND-ZUSTAND               PIC  X(001).
      *
      * Produktart
           05  'XX'-AEND-PRODUKTART            PIC  9(001).
      *
      * Gebühr nicht angekauft in %
           05  'XX'-AEND-GEBNICHTANPROZ        PIC  9(001)V9(002).
           05  'XX'-AEND-GEBNICHTANPROZ-KZN    PIC  X(001).
      *
      * Factoringgebühr in %
           05  'XX'-AEND-FACTGEBPROZ           PIC  9(001)V9(002).
           05  'XX'-AEND-FACTGEBPROZ-KZN       PIC  X(001).
      *
      * Factoring Ankaufrahmen in EUR
           05  'XX'-AEND-FACTANKRAHMEN         PIC  9(010)V9(002).
           05  'XX'-AEND-FACTANKRAHMEN-KZN     PIC  X(001).
      *
      * Vertragsbeginndatum JHJJMMTT
           05  'XX'-AEND-VTGBEGINNDATUM        PIC  9(008).
           05  'XX'-AEND-VTGBEGINNDATUM-KZN    PIC  X(001).
      *
      * Vertragsendedatum
           05  'XX'-AEND-VTGENDEDATUM          PIC  9(008).
           05  'XX'-AEND-VTGENDEDATUM-KZN      PIC  X(001).
      *
      * DVM-Einrichtungsgebühr in EUR
           05  'XX'-AEND-EINRICHTGEBUEHR       PIC  9(008)V9(002).
           05  'XX'-AEND-EINRICHTGEB-KZN       PIC  X(001).
      *
      * IBAN
           05  'XX'-AEND-IBAN                  PIC  X(034).
           05  'XX'-AEND-IBAN-KZN              PIC  X(001).
      *
      * Infosperre Kennzeichen
           05  'XX'-AEND-INFOSPERRE            PIC  X(001).
           05  'XX'-AEND-INFOSPERRE-KZN        PIC  X(001).
      *
      * "Prognose von" Datum JHJJMMTT - aufs Jahr
           05  'XX'-AEND-PROGVONDATPA          PIC  9(008).
           05  'XX'-AEND-PROGVONDATPA-KZN      PIC  X(001).
      *
      * Jahresstartdatum Umsatz JHJJMMTT
           05  'XX'-AEND-PROGBISDATPA          PIC  9(008).
           05  'XX'-AEND-PROGBISDATPA-KZN      PIC  X(001).
      *
      * Anzahl Rechnungen seit Jahresbeginn
           05  'XX'-AEND-PROGANZRECHPA         PIC  9(008).
           05  'XX'-AEND-PROGANZRECHPA-KZN     PIC  X(001).
      *
      * kumulierter Umsatz seit Jahresbeginn
           05  'XX'-AEND-PROGUMSATZPA          PIC  9(008)V9(002).
           05  'XX'-AEND-PROGUMSATZPA-KZN      PIC  X(001).
      *
      * Monatsstartdatum Umsatz JHJJMMTT
           05  'XX'-AEND-PROGVONDATPM          PIC  9(008).
           05  'XX'-AEND-PROGVONDATPM-KZN      PIC  X(001).
      *
      * Datum Monatsumsatz bis JHJJMMTT
           05  'XX'-AEND-PROGBISDATPM          PIC  9(008).
           05  'XX'-AEND-PROGBISDATPM-KZN      PIC  X(001).
      *
      * Anzahl Rechnungen seit Monatsbeginn
           05  'XX'-AEND-PROGANZRECHPM         PIC  9(008).
           05  'XX'-AEND-PROGANZRECHPM-KZN     PIC  X(001).
      *
      * kumulierter Umsatz seit Monatsbeginn
           05  'XX'-AEND-PROGUMSATZPM          PIC  9(008)V9(002).
           05  'XX'-AEND-PROGUMSATZPM-KZN      PIC  X(001).
      *
      * Reserve
           05  FILLER                          PIC  X(310).
 
