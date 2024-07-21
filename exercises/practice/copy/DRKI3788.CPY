      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.05.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3788.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3788.CPv  $
      *
      *   Rev 2.1   Jul 07 2020 13:00:26   t13ex78
      *AKI01780: Programme und Copies
      *
      *   Rev 2.0   May 20 2020 08:59:40   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      * G90 - EOS                                   *
      *    Vertragsdaten - DTA
      *                                                                *
      *    Satzlänge 399 Bytes                                         *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01780!22.05.20!2.0 ! Neu                         ! t13ex78   *
      *        !        !    !                             !           *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-DTA-SATZ.
      *
      *****************************************************************
      * Vorsatz
      *****************************************************************
      *
           05  'XX'-VORSATZ.
      *
      * Satzart - immer 'A'
               10  'XX'-V-SA                       PIC  X(001).
      *
      * Absender - 'EOS'
               10  'XX'-V-ABSENDER                 PIC  X(003).
      *
      * Datum der Erstellung des DTA im Format  JHJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(010).
      *
      * Welche Inhalt hat die Lieferung?
      * 'VERTRAGSDATEN'
               10  'XX'-V-DTA-INHALT               PIC  X(020).
      *
      * K - Komplett
      * D - Delta
               10  'XX'-V-K-ODER-D                 PIC  X(001).
      *
      * Reserve
               10  FILLER                          PIC  X(356).
      *
      *****************************************************************
      * Hauptsatz
      *****************************************************************
      *
           05  'XX'-HAUPTSATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'D'
               10  'XX'-SA                         PIC  X(001).
      *
      * Gesellschaft - immer '90'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * Abschlussvermittler
               10  'XX'-VBNR                       PIC  9(007).
      *
      * Produktart
      *  1 - Abrechnung 2.0 (ProduktID 538)
      *  2 - Factoring Plus (ProduktID 539)
               10  'XX'-PRODUKTART                 PIC  9(001).
      *
      * Gebühr nicht angekauft in %
               10  'XX'-GEBNICHTANPROZ             PIC  9(001)V9(002).
      *
      * Factoringgebühr in %
               10  'XX'-FACTGEBPROZ                PIC  9(001)V9(002).
      *
      * Factoring Ankaufrahmen in EUR
               10  'XX'-FACTANKRAHMEN              PIC  9(010)V9(002).
      *
      * Vertragsbeginndatum JHJJMMTT
               10  'XX'-VERTRAGBEGINNDATUM         PIC  9(008).
      *
      * Vertragsendedatum JHJJMMTT
               10  'XX'-VERTRAGENDEDATUM           PIC  9(008).
      *
      * DVM-Einrichtungsgebühr EUR
               10  'XX'-EINRICHTGEBUEHR            PIC  9(008)V9(002).
      *
      * IBAN
               10  'XX'-IBAN                       PIC  X(034).
      *
      * Infosperre Kennzeichen (J oder N)
               10  'XX'-INFOSPERRE                 PIC  X(001).
      *
      * Jahresstartdatum Umsatz JHJJMMTT
               10  'XX'-PROGVONDATPA               PIC  9(008).
      *
      * Datum Jahresumsatz bis JHJJMMTT
               10  'XX'-PROGBISDATPA               PIC  9(008).
      *
      * Anzahl Rechnungen seit Jahresbeginn
               10  'XX'-PROGANZRECHNUNGPA          PIC  9(008).
      *
      * kumulierter Umsatz seit Jahresbeginn
               10  'XX'-PROGUMSATZPA               PIC  9(008)V9(002).
      *
      * Monatsstartdatum Umsatz JHJJMMTT
               10  'XX'-PROGVONDATPM               PIC  9(008).
      *
      * Datum Monatsumsatz bis JHJJMMTT
               10  'XX'-PROGBISDATPM               PIC  9(008).
      *
      * Anzahl Rechnungen seit Monatsbeginn
               10  'XX'-PROGANZRECHNUNGPM          PIC  9(008).
      *
      * kumulierter Umsatz seit Monatsbeginn
               10  'XX'-PROGUMSATZPM               PIC  9(008)V9(002).
      *
      * Reserve
               10  FILLER                          PIC  X(226).
 
      *****************************************************************
      * Endesatz
      *****************************************************************
      *
           05  'XX'-ENDESATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'E'
               10  'XX'-N-SA                       PIC  X(001).
      *
      * Satzanzahl - gelieferte D-Sätze
               10  'XX'-N-ANZAHL                   PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(390).
 
