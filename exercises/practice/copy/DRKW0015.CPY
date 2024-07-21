      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0015.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKW0015.CPv  $
      *
      *   Rev 2.4   Nov 11 2010 11:16:04   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *
      *   Rev 2.3   Oct 11 2010 13:41:38   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.2   Oct 08 2010 08:58:26   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.1   Sep 28 2010 13:34:24   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.0   Aug 12 2010 14:51:32   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KREDITSPARKONTO  BNK (DTA)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = DTA                                             *
      *    Länge = 400 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *18.08.10!NEUERSTELLUNG FÜR DVAG WIEN             !SWZ      ! 01 *
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *07.10.10!DVW-BNK geändert in DTA-BNK-            !SWZ      ! 2.1*
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*

       01  DRKW0015.

           05  DTA-BNK-VORSATZ.

               10 DTA-BNK-V-SATZART                   PIC  X.
      *           'A'
               10 DTA-BNK-ABSENDER                    PIC  X(005).
      *           'DWIEN'
               10 DTA-BNK-DTADATUM                    PIC  9(008).
      *           JJJJMMTT  (alle DTAs von einem Tag müssen in
      *           diesem Feld den gleichen Wert haben)
               10 DTA-BNK-EMPFAENGER                  PIC  X(004).
      *           'DVAG'
               10 DTA-BNK-DATENTEXT                   PIC  X(025).
      *           'KREDITSPARKONTO'
               10 DTA-BNK-VERABKZ                     PIC  X(004).
      *           'INIT'
               10 FILLER                              PIC  X(353).



           05  DTA-BNK-DATEN REDEFINES DTA-BNK-VORSATZ.

               10 DTA-BNK-SATZART                  PIC  X.
      *           'D'
               10 DTA-BNK-KEY.
                  15 DTA-BNK-GESELLSCHAFT          PIC  9(002).
      *              Produktgeber
                  15 DTA-BNK-VERTRAGSNUMMER        PIC  9(015).
      *              Vertragsnummer
                  15 DTA-BNK-PRODSPARTE            PIC  9(002).
      *              SCHLUESSEL DES PRODUKTS
               10 DTA-BNK-PRODDETAILNAME           PIC  X(050).
      *            GENAUE BEZEICHNUNG DES PRODUKTS
               10 DTA-BNK-DAT-PRODUKTEROEFF        PIC  9(008).
      *            'JJJJMMTT'
               10 DTA-BNK-URSPR-BETRAG             PIC  9(009)V99.
      *            URSPRUENGLICHER DARLEHENSBETRAG BEI KREDIT
      *            URSPRUENGLICHER SPARBETRAG BEI SPARPRODUKT
      *            SONST LEER
               10 DTA-BNK-AKT-KTOSTAND             PIC  9(009)V99.
      *            AKTUELLES GUTHABEN/DARLEHENSALDO
               10 DTA-BNK-DAT-VER-LFZT-ENDE        PIC  9(008).
      *            ENDEDATUM DER VERTRAGSLAUFZEIT BEI BAUSPAREN
      *            ENDEDATUM DER ZINSBINDUNG BEI SPARPRODUKT
      *            SONST LEER
               10 DTA-BNK-BEITRAG                  PIC  9(009)V99.
      *            AKTUELLER BEITRAG
      *            AKTUELLE  KREDITRATE
               10 DTA-BNK-ZAHLWEISE                PIC  9(002).
      *            ZAHLWEISE DES BEITRAGS/RATE
               10 DTA-BNK-ZINSSATZ                 PIC  9(004)V9999.
      *            GUTHABENSZINS P.A. BEI BAUSPAREN/SPARPRODUKT
      *            NORMINALZINSSATZ P.A. BEI DARLEHEN
      *            SONST LEER
               10 DTA-BNK-FILLER                   PIC  X(271).


           05  DTA-BNK-NACHSATZ REDEFINES DTA-BNK-VORSATZ.

               10 DTA-BNK-N-SATZART             PIC X(001).
      *           'E'
               10 DTA-BNK-ANZ                   PIC 9(009).
      *           Satzanzahl incl. Vor- und Nachsatz
               10 FILLER                        PIC X(390).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0015                       *
      ******************************************************************




