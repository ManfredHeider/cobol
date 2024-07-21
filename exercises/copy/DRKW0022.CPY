      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0022.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKW0022.CPv  $
      *
      *   Rev 2.4   Oct 25 2010 11:11:12   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.3   Oct 13 2010 11:03:40   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.2   Oct 11 2010 13:41:50   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.1   Sep 28 2010 13:34:38   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.0   Aug 23 2010 11:08:44   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aufbereitungsbestand DVAG Wien Kreditsparkonto          *
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
      * -------------------------------------------------------------*
      * AKI00966!11.10.10! 2.1! Satzart von C auf D       ! SWZ      *
      * -------------------------------------------------------------*
      * AKI00966!13.10.10! 2.2! Neu: Schließungsdatum     ! SWZ      *
      * -------------------------------------------------------------*
      * AKI00966!25.10.10! 2.3! Filler war zu groß        ! SWZ      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0022.
      *
               05 BNK-SA                       PIC  X.
      *           'D'

               05 BNK-KEY.
                  10 BNK-GES                   PIC  9(002).
      *           Produktgeber
                  10 BNK-VERTRAGSNUMMER        PIC  9(015).
      *           Vertragsnummer
                  10 BNK-PROD-SPARTE           PIC  9(002).
      *            SCHLUESSEL DES PRODUKTS

               05 BNK-PRODDETAILNAME           PIC  X(050).
      *            GENAUE BEZEICHNUNG DES PRODUKTS
               05 BNK-AEND-PRODDETAILNAME-KZ       PIC  X(001).
      *            J = Änderung
               05 BNK-DAT-PRODUKTEROEFF        PIC  9(008).
      *            'JJJJMMTT'
               05 BNK-AEND-DAT-PRODUKTEROEFF-KZ PIC  X(001).
      *            J = Änderung
131010         05 BNK-DAT-PRODUKTSCHLIESS      PIC  9(008).
131010*            'JJJJMMTT'
131010         05 BNK-AEND-DAT-PRODUKTSCHL-KZ PIC  X(001).
131010*            J = Änderung
               05 BNK-URSPR-BETRAG             PIC  9(009)V99.
      *            URSPRUENGLICHER DARLEHENSBETRAG BEI KREDIT
      *            URSPRUENGLICHER SPARBETRAG BEI SPARPRODUKT
      *            SONST LEER
               05 BNK-AEND-URSPR-BETRAG-KZ     PIC  X(001).
      *            J = Änderung
               05 BNK-AKT-KTOSTAND             PIC  9(009)V99.
      *            AKTUELLES GUTHABEN/DARLEHENSALDO
               05 BNK-AEND-AKT-KTOSTAND-KZ     PIC  X(001).
      *            J = Änderung
               05 BNK-DAT-VER-LFZT-ENDE        PIC  9(008).
      *            ENDEDATUM DER VERTRAGSLAUFZEIT BEI BAUSPAREN
      *            ENDEDATUM DER ZINSBINDUNG BEI SPARPRODUKT
      *            SONST LEER
               05 BNK-AEND-DAT-VER-LFZT-ENDE-KZ PIC  X(001).
      *            J = Änderung
               05 BNK-BEITRAG                  PIC  9(009)V99.
      *            AKTUELLER BEITRAG
      *            AKTUELLE  KREDITRATE
               05 BNK-AEND-BEITRAG-KZ          PIC  X(001).
      *            J = Änderung
               05 BNK-ZAHLWEISE                PIC  9(002).
      *            ZAHLWEISE DES BEITRAGS/RATE
               05 BNK-AEND-ZAHLWEISE-KZ        PIC  X(001).
      *            J = Änderung
               05 BNK-ZINSSATZ                 PIC  9(004)V9999.
      *            GUTHABENSZINS P.A. BEI BAUSPAREN/SPARPRODUKT
      *            NORMINALZINSSATZ P.A. BEI DARLEHEN
      *            SONST LEER
               05 BNK-AEND-ZINSSATZ-KZ         PIC  X(001).
      *            J = Änderung

               05 BNK-VERARB-KZ                PIC X(01).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
      *                D = Delete


               05 BNK-FILLER                   PIC  X(053).




