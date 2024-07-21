      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0021.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKW0021.CPv  $
      *
      *   Rev 2.3   Oct 13 2010 11:03:34   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.2   Oct 11 2010 13:41:44   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.1   Sep 28 2010 13:34:34   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.0   Aug 23 2010 11:08:36   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KREDITSPARKONTO BNK (KSDS)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    Länge = 250 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *18.08.10!NEUERSTELLUNG FÜR DVAG WIEN             !SWZ      ! 01 *
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *11.10.10!Satzart von C auf D                     !SWZ      ! 2.1*
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *13.10.10!Einfügen Schließungsdatum               !SWZ      ! 2.2*
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKW0021.


      *
               05 KOMPL-BNK-SATZART                  PIC  X.
      *           'D'
               05 KOMPL-KSDS-KONTOK-KEY.
                  10 KOMPL-BNK-GESELLSCHAFT          PIC  9(002).
      *              Produktgeber
                  10 KOMPL-BNK-VERTRAGSNUMMER        PIC  9(015).
      *              Vertragsnummer
                  10 KOMPL-BNK-PRODSPARTE            PIC  9(002).
      *              SCHLUESSEL DES PRODUKTS

               05 KOMPL-BNK-PRODDETAILNAME           PIC  X(050).
      *            GENAUE BEZEICHNUNG DES PRODUKTS
               05 KOMPL-BNK-DAT-PRODUKTEROEFF        PIC  9(008).
      *            'JJJJMMTT'
               05 KOMPL-BNK-URSPR-BETRAG             PIC  9(009)V99.
      *            URSPRUENGLICHER DARLEHENSBETRAG BEI KREDIT
      *            URSPRUENGLICHER SPARBETRAG BEI SPARPRODUKT
      *            SONST LEER
               05 KOMPL-BNK-AKT-KTOSTAND             PIC  9(009)V99.
      *            AKTUELLES GUTHABEN/DARLEHENSALDO
               05 KOMPL-BNK-DAT-VER-LFZT-ENDE        PIC  9(008).
      *            ENDEDATUM DER VERTRAGSLAUFZEIT BEI BAUSPAREN
      *            ENDEDATUM DER ZINSBINDUNG BEI SPARPRODUKT
      *            SONST LEER
               05 KOMPL-BNK-BEITRAG                  PIC  9(009)V99.
      *            AKTUELLER BEITRAG
      *            AKTUELLE  KREDITRATE
               05 KOMPL-BNK-ZAHLWEISE                PIC  9(002).
      *            ZAHLWEISE DES BEITRAGS/RATE
               05 KOMPL-BNK-ZINSSATZ                 PIC  9(004)V9999.
      *            GUTHABENSZINS P.A. BEI BAUSPAREN/SPARPRODUKT
      *            NORMINALZINSSATZ P.A. BEI DARLEHEN
      *            SONST LEER
131010         05 KOMPL-BNK-DAT-PRODUKTSCHLIESS      PIC  9(008).
131010*            'JJJJMMTT'
               05 KOMPL-BNK-FILLER                   PIC  X(113).



