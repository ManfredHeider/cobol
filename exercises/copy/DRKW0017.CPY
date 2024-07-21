      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0017.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKW0017.CPv  $
      *
      *   Rev 2.4   Oct 11 2019 10:12:42   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer 
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.3   May 06 2019 13:51:28   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.2   Sep 28 2010 13:53:16   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.1   Sep 28 2010 13:30:26   t13ex15
      *AKI00966: DVAG Österreich: Depot-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 18 2010 13:02:02   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aufbereitungsbestand DVAG Wien Depotdaten               *
      *                                                               *
      *    SATZLÄNGE   200                                            *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!19.08.10! 01 ! Neu                       ! SWZ      *
      * --------+--------+----+---------------------------+----------*
      * AKI01683!24.04.19!2.3 ! Neue Attribute            ! IFR        *
      *         !        !    ! Risikoklasse und Wieder-  !            *
      *         !        !    ! anlagedatum               !            *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0017.
      *
               05  BNK-SA             PIC  X(001).
      *            SATZART        = 'D'
      *
               05  BNK-KEY.
                   25 BNK-GES              PIC  9(002).
      *               GESELLSCHAFT
                   25 BNK-VNR              PIC  9(015).
      *               VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES BNK-VNR.
                      30 BNK-VNR-1-15      PIC  9(015).
                   25 BNK-UNTERNUMMER      PIC  9(003).
      *               UNTERDEPOTNUMMER)
                   25 BNK-ISIN             PIC  X(012).
      *               WERTPAPIER ISIN
               05  BNK-PROD-SPARTE         PIC  9(002).
      *            Schlüssel des Produkts lt. Spartenaufstellung
               05  BNK-AEND-PROD-SPARTE-KZ PIC  X(001).
      *            J = Änderung
               05  BNK-DAT-DEPOTEROEFF     PIC  9(008).
      *            'JJJJMMTT'
               05  BNK-AEND-DAT-DEPOTEROEFF-KZ  PIC  X(001).
      *            J = Änderung
               05  BNK-DAT-DEPOTSCHLIESS PIC  9(008).
      *            'JJJJMMTT'
               05  BNK-AEND-DAT-DEPOTSCHLIESS-KZ  PIC  X(001).
      *            J = Änderung
               05  BNK-ANZ-ANTEILE         PIC  9(006)V9999.
      *            Anzahl der Anteile
               05  BNK-AEND-ANZ-ANTEILE-KZ PIC  X(001).
      *            J = Änderung
               05  BNK-GUTHABEN            PIC  9(009)V99.
      *            Rücknahmepreis gesamt
               05  BNK-AEND-GUTHABEN-KZ    PIC  X(001).
      *            J = Änderung
               05  BNK-DAT-GUTHABEN        PIC  9(008).
      *            Datum des Rücknahmewertes 'JJJJMMTT'
               05  BNK-AEND-DAT-GUTHABEN-KZ  PIC  X(001).
      *            J = Änderung
               05  BNK-SPARBETRAG          PIC  9(009)V99.
      *            Sparbetrag
               05  BNK-AEND-SPARBETRAG-KZ  PIC  X(001).
      *            J = Änderung
               05  BNK-ZAHLWEISE           PIC  9(002).
      *            Zahlweise des Sparbetrags
               05  BNK-AEND-ZAHLWEISE-KZ   PIC  X(001).
      *            J = Änderung
               05  BNK-RISIKOKLASSE        PIC  9(001).
      *            Risikoklasse
               05  BNK-AEND-RISIKOKLASSE-KZ  PIC  X(001).
      *            J = Änderung
               05  BNK-WIEDERANLAGE        PIC  9(008).
      *            'JJJJMMTT' Wiederanlagedatum
               05  BNK-AEND-WIEDERANLAGE-KZ PIC  X(001).
      *            J = Änderung

               05  BNK-VERARB-KZ           PIC X(01).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
      *                D = Delete


               05  FILLER                  PIC  X(087).
      *            RESERVE




