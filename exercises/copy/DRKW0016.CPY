      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0016.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKW0016.CPv  $
      *
      *   Rev 2.4   Oct 11 2019 10:10:04   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer 
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.3   May 06 2019 13:50:36   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.2   Sep 28 2010 13:53:12   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.1   Sep 28 2010 13:30:20   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 18 2010 13:01:54   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG BNK (DTA)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    Länge = 200 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *18.08.10!NEUERSTELLUNG FÜR DVAG WIEN             !SWZ      ! 01 *
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      * AKI01683!24.04.19!2.3 ! Neue Attribute            ! IFR        *
      *         !        !    ! Risikoklasse und Wieder-  !            *
      *         !        !    ! anlagedatum               !            *
      *
       01  DRKW0016.
      *
               05  KOMPL-BNK-SA             PIC  X(001).
      *            SATZART        = 'D'
      *
               05  KOMPL-BNK-KEY.
                   25 KOMPL-BNK-GES              PIC  9(002).
      *               GESELLSCHAFT
                   25 KOMPL-BNK-VNR              PIC  9(015).
      *               VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES KOMPL-BNK-VNR.
                      30 KOMPL-BNK-VNR-1-15      PIC  9(015).
                   25 KOMPL-BNK-UNTERNR          PIC  9(003).
      *               UNTERDEPOTNUMMER)
                   25 KOMPL-BNK-ISIN             PIC  X(012).
      *               WERTPAPIER ISIN
               05  KOMPL-BNK-PROD-SPARTE     PIC  9(002).
      *            Schlüssel des Produkts lt. Spartenaufstellung
               05  KOMPL-BNK-DAT-DEPOTEROEFF PIC  9(008).
      *            'JJJJMMTT'
               05  KOMPL-BNK-DAT-DEPOTSCHLIESS PIC  9(008).
      *            'JJJJMMTT'
               05  KOMPL-BNK-ANZ-ANTEILE     PIC  9(006)V9999.
      *            Anzahl der Anteile
               05  KOMPL-BNK-GUTHABEN        PIC  9(009)V99.
      *            Rücknahmepreis gesamt
               05  KOMPL-BNK-DAT-GUTHABEN    PIC  9(008).
      *            Datum des Rücknahmewertes 'JJJJMMTT'
               05  KOMPL-BNK-SPARBETRAG      PIC  9(009)V99.
      *            Sparbetrag
               05  KOMPL-BNK-ZAHLWEISE       PIC  9(002).
      *            Zahlweise des Sparbetrags
               05  KOMPL-BNK-RISIKOKLASSE    PIC  9(001).
      *           Risikoklasse
               05  KOMPL-BNK-WIEDERANLAGE    PIC  9(008).
      *           Wiedernalagedatum 'JJJJMMTT'



               05  FILLER                    PIC  X(098).
      *            RESERVE





