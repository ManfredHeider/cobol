      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2963.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2963.CPv  $
      *
      *   Rev 2.3   Jul 07 2020 13:04:12   t13ex78
      *AKI01780: Programme und Copies
      *
      *   Rev 2.2   Sep 02 2015 15:10:22   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:20   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 11:39:38   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 6 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DVM Aktualisierung fuer DVMVTGEIGEN      *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  150 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *--------!---------------------------------------!---------!-----*
      *26.05.20!Neue Attribute in der Tab. DVMVTGEIGEN !t13ex78  ! 2.3 *
      *AKI01780!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
           05 'XX'-GES                          PIC  9(002).
      *
      * Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                          PIC  9(015).
      * Produktart
      * 1 = Abrechnung (DV)
      * 2 = Factoring (DV)
      * 3 = Abrechnung 2.0 (EOS)
      * 4 = Factoring Plus (EOS)
           05 'XX'-PRODUKT-ART                  PIC  9(001).
           05 'XX'-PRODUKT-ART-KZ               PIC  X(001).
      * DVM Abrechnungsgebuehrsatz
           05 'XX'-ABRECH-GEBUEHR               PIC  9(001)V99.
           05 'XX'-ABRECH-GEBUEHR-KZ            PIC  X(001).
      * DVM Factoringgebuehrsatz
           05 'XX'-FACTOR-GEBUEHR               PIC  9(001)V99.
           05 'XX'-FACTOR-GEBUEHR-KZ            PIC  X(001).
      * DVM Factoring Ankaufshöhe
           05 'XX'-FACTOR-ANK-BETR              PIC  9(010)V99.
           05 'XX'-FACTOR-ANK-BETR-KZ           PIC  X(001).
      * DVM Einrichtungsgebuehrbetrag
           05 'XX'-EIN-GEBUEHR-BTRG             PIC  9(008)V99.
           05 'XX'-EIN-GEBUEHR-BTRG-KZ          PIC  X(001).
      * DVM Informationssperrekennzeichen fuer die Masken
           05 'XX'-INFOSPERRE-KZ                PIC  X(001).
           05 'XX'-INFOSPERRE-KZ-KZ             PIC  X(001).
      * DVM Prognosezeitraum p.A.
           05 'XX'-PROG-VON-DATUM               PIC  X(010).
           05 'XX'-PROG-VON-DATUM-KZ            PIC  X(001).
           05 'XX'-PROG-BIS-DATUM               PIC  X(010).
           05 'XX'-PROG-BIS-DATUM-KZ            PIC  X(001).
      * DVM Prognose Anzahl Rechnungen fuer den Zeitraum p.A.
           05 'XX'-PROG-ANZ-RECHN               PIC  9(008).
           05 'XX'-PROG-ANZ-RECHN-KZ            PIC  X(001).
      * DVM Prognose Umsatzsumme aller Rechn. fuer den Zeitraum p.A.
           05 'XX'-PROG-UMSATZ                  PIC  9(008)V99.
           05 'XX'-PROG-UMSATZ-KZ               PIC  X(001).
      * DVM Prognosezeitraum p.M.
           05 'XX'-PROG-VON-DATUM-PM            PIC  X(010).
           05 'XX'-PROG-VON-DATUM-PM-KZ         PIC  X(001).
           05 'XX'-PROG-BIS-DATUM-PM            PIC  X(010).
           05 'XX'-PROG-BIS-DATUM-PM-KZ         PIC  X(001).
      * DVM Prognose Anzahl Rechnungen fuer den Zeitraum p.M.
           05 'XX'-PROG-ANZ-RECHN-PM            PIC  9(008).
           05 'XX'-PROG-ANZ-RECHN-PM-KZ         PIC  X(001).
      * DVM Prognose Umsatzsumme aller Rechn. fuer den Zeitraum p.M.
           05 'XX'-PROG-UMSATZ-PM               PIC  9(008)V99.
           05 'XX'-PROG-UMSATZ-PM-KZ            PIC  X(001).
      *
 
      *
           05 FILLER                            PIC  X(012).
      *    05 FILLER                            PIC  X(054).
      ******************************************************************


