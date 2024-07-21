      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2369.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2369.CPv  $
      *
      *   Rev 2.1   Nov 13 2009 11:02:20   t13ex50
      *AKI00867: Programmübergreifende Copy-Strecken
      *
      *   Rev 2.0   Nov 12 2009 14:50:06   t13ex50
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR Abgeltungssteuer AGI (BNK-Satz)             *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    LÄNGE: 300 ZEICHEN                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *12.11.09! NEUERSTELLUNG                          ! PSC     !2.00*
      *AKI00867!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI2369.
               05  BNK-SA                             PIC  X(001).
      *            SATZART        = 'C'
               05  BNK-GES                            PIC  9(002).
      *            GESELLSCHAFT   = '40'
               05  BNK-KEY.
                   10  BNK-VNR                        PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   10 FILLER REDEFINES BNK-VNR.
                      15  BNK-VNR-1-13                PIC  9(013).
                      15  BNK-UNTER-NR                PIC  9(002).
      *            UNTERDEPOTNUMMER
      *
               05  BNK-VERARB-KZ                      PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = Insert
      *                U = Update

               05  BNK-FSA-GUELTIGAB                  PIC  9(008).
      *            DATUM, AB DEM FREISTELLUNGSAUFTRAG GILT (JHJJMMTT)
               05  BNK-FSA-GUELTIGAB-KZ               PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-FSA-GUELTIGAB
      *
               05  BNK-FSA-GUELTIGBIS                 PIC  9(008).
      *            DATUM, BIS ZU DEM FREISTELLUNGSAUFTRAG GILT
               05  BNK-FSA-GUELTIGBIS-KZ              PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-FSA-GUELTIGBIS
      *
               05  BNK-FSA-ERTEILT                    PIC  9(005)V99.
      *            ERTEILTER FREISTELLUNGSAUFTRAG
               05  BNK-FSA-ERTEILT-KZ                 PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-FSA-ERTEILT
      *
               05  BNK-FSA-VERBRAUCHT                 PIC  9(005)V99.
      *            VERBRAUCHTER BETRAG
               05  BNK-FSA-VERBRAUCHT-KZ              PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-FSA-VERBRAUCHT
      *
               05  BNK-DEPOTSTATUS-KZ                 PIC  X(001).
      *            DEPOT IST LOGISCH GELOESCHT                    'D'
      *            VERMITTLERAUSKUNFTSPERRE/WECHSEL VON DVAG WEG  'I'
      *            INFOSPERRE AUFGEHOBEN/WECHSEL ZUR DVAG         'S'
      *            REAKTIVIERUNG LOGISCH GELOESCHTEN DEPOTS       'R'
               05  BNK-DEPOTSTATUS-KZ-KZ              PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-DEPOTSTATUS-KZ
      *
               05  BNK-STEUER-AUSL-KZ                 PIC  X(001).
      *            KENNZEICHEN STEUERAUSLAENDER, WERTE: 'J'/'N'/LEER
               05  BNK-STEUER-AUSL-KZ-KZ              PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-STEUER-AUSL-KZ
      *
               05  BNK-NV-GUELTIGBIS                  PIC  9(008).
      *            DATUM, BIS ZU DEM NICHTVERANL.-BESCHEINIGUNG GILT
               05  BNK-NV-GUELTIGBIS-KZ               PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-NV-GUELTIGBIS
      *
               05  BNK-FSA-VERBRAUCHT-VJ              PIC  9(005)V99.
      *            VERBRAUCHTER BETRAG VORJAHR
               05  BNK-FSA-VERBRAUCHT-VJ-KZ           PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-FSA-VERBRAUCHT-VJ
      *
               05  BNK-GEWINN-UEBER-FSA-VJ            PIC  9(010)V99.
               05  BNK-GEWINN-UEBER-FSA-VJ-KZ         PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-GEWINN-UEBER-FSA-VJ
      *
               05  BNK-GEWINN-UEBER-FSA-LFDJ          PIC  9(010)V99.
      *            GEWINNE UEBER FSA IM VORJAHR/LFD. JAHR
               05  BNK-GEWINN-UEBER-FSA-LFDJ-KZ       PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-GEWINN-UEBER-FSA-LFDJ
      *
               05  BNK-AST-BEZ                        PIC  9(010)V99.
      *            GEZAHLTE ABGELTUNGSSTEUER INCL. SOLI LFD. JAHR
               05  BNK-AST-BEZ-KZ                     PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-AST-BEZ
      *
               05  BNK-KIST-BEZ                       PIC  9(010)V99.
      *            EINBEHALTENE KIRCHENSTEUER LFD. JAHR
               05  BNK-KIST-BEZ-KZ                    PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KIST-BEZ
      *
               05  BNK-SALDO-VERL-VERR-AKTIEN         PIC  9(010)V99.
      *            SALDO VERLUSTVERRECHNUNGSTOPF AKTIEN
               05  BNK-SALDO-VERL-VERR-AKTIEN-KZ      PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-SALDO-VERL-VERR-AKTIEN
      *
               05  BNK-SALDO-VERL-VERR                PIC  9(010)V99.
      *            SALDO VERLUSTVERRECHNUNGSTOPF
               05  BNK-SALDO-VERL-VERR-KZ             PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-SALDO-VERL-VERR
      *
               05  BNK-SALDO-QUELLST                  PIC  9(010)V99.
      *            SALDO QUELLENSTEUERTOPF
               05  BNK-SALDO-QUELLST-KZ               PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-SALDO-QUELLST
      *
               05  BNK-VERL-VERR-KZ                   PIC  X(001).
      *            KENNZEICHEN VERLUSTVERRECHNUNGSTOPF BESCHEINIGEN J/N
               05  BNK-VERL-VERR-KZ-KZ                PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-VERL-VERR-KZ
      *
               05  BNK-KONFESSION-INH1                PIC  X(002).
      *            KONFESSIONSSCHLUESSEL DEPOTINHABER 1
               05  BNK-KONFESSION-INH1-KZ             PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KONFESSION-INH1
      *
               05  BNK-KONFESSION-INH2                PIC  X(002).
      *            KONFESSIONSSCHLUESSEL DEPOTINHABER 2
               05  BNK-KONFESSION-INH2-KZ             PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KONFESSION-INH2
      *
               05  BNK-KIST-SATZ-INH1                 PIC  9(002)V99.
      *            KIRCHENSTEUERSATZ DEPOTINHABER 1
               05  BNK-KIST-SATZ-INH1-KZ              PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KIST-SATZ-INH1
      *
               05  BNK-KIST-SATZ-INH2                 PIC  9(002)V99.
      *            KIRCHENSTEUERSATZ DEPOTINHABER 2
               05  BNK-KIST-SATZ-INH2-KZ              PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KIST-SATZ-INH2
      *
               05  BNK-KIST-ANTRAG-GUELTIGAB          PIC  9(008).
      *            DATUM, AB DEM KIRCHENSTEUERANTRAG GILT (JHJJMMTT)
               05  BNK-KIST-ANTRAG-GUELTIGAB-KZ       PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KIST-ANTRAG-GUELTIGAB
      *
               05  BNK-KIST-ANTEIL1                   PIC  9(003)V99.
      *            PROZENTUALER ANTEIL KIRCHENSTEUER DEPOTINHABER 1
               05  BNK-KIST-ANTEIL1-KZ                PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KIST-ANTEIL1
      *
               05  BNK-KIST-ANTEIL2                   PIC  9(003)V99.
      *            PROZENTUALER ANTEIL KIRCHENSTEUER DEPOTINHABER 2
               05  BNK-KIST-ANTEIL2-KZ                PIC  X(001).
      *            KENNZEICHN VERÄNDERTES BNK-KIST-ANTEIL2
      *
               05  FILLER                             PIC  X(096).
      *---------------------------------------------------------------*
