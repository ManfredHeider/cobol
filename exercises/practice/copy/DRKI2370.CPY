      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2370.CPv  $
      *
      *   Rev 2.1   Dec 14 2009 15:37:16   t13ex50
      *AKI00867: ASt Ermittlungsprogramm
      *
      *   Rev 2.0   Nov 17 2009 14:54:54   t13ex50
      *ARCHIVERSTELLUNG
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR ABGELTUNGSSTEUER AGI (KSDS)                 *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    LÄNGE: 250 STELLEN                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                       !NAME      !VER-  *
      *AUFTRAG !                                     !          !SION  *
      *--------!-------------------------------------!----------!------*
      *20.11.09! NEUERSTELLUNG                       !SCHLEICHER!2.00  *
      *AKI00867!                                     !          !      *
      *--------!-------------------------------------!----------!------*
      *        !                                     !          !      *
      *--------!-------------------------------------!----------!------*
      *
       01  DRKI2370.
           10  KSDS-AST-SATZ.
               20  KSDS-SA                      PIC  X(001).
      *            SATZART        = 'C'
               20  KSDS-GES                     PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  KSDS-KEY.
                   25  KSDS-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES KSDS-VNR.
                      30  KSDS-VNR-1-13         PIC  9(013).
                      30  KSDS-UNTER-NR         PIC  9(002).
      *            UNTERDEPOTNUMMER
               20  KSDS-FSA-GUELTIGAB           PIC  9(008).
      *            DATUM, AB DEM FREISTELLUNGSAUFTRAG GILT (JHJJMMTT)
               20  KSDS-FSA-GUELTIGBIS          PIC  9(008).
      *            DATUM, BIS ZU DEM FREISTELLUNGSAUFTRAG GILT
               20  KSDS-FSA-ERTEILT             PIC  9(005)V99.
      *            ERTEILTER FREISTELLUNGSAUFTRAG
               20  KSDS-FSA-VERBRAUCHT          PIC  9(005)V99.
      *            VERBRAUCHTER BETRAG
               20  KSDS-DEPOTSTATUS-KZ          PIC  X(001).
      *            DEPOT IST LOGISCH GELOESCHT                    'D'
      *            VERMITTLERAUSKUNFTSPERRE/WECHSEL VON DVAG WEG  'I'
      *            INFOSPERRE AUFGEHOBEN/WECHSEL ZUR DVAG         'S'
      *            REAKTIVIERUNG LOGISCH GELOESCHTEN DEPOTS       'R'
               20  KSDS-STEUER-AUSL-KZ          PIC  X(001).
      *            KENNZEICHEN STEUERAUSLAENDER, WERTE: 'J'/'N'/LEER
               20  KSDS-NV-GUELTIGBIS           PIC  9(008).
      *            DATUM, BIS ZU DEM NICHTVERANL.-BESCHEINIGUNG GILT
               20  KSDS-FSA-VERBRAUCHT-VJ       PIC  9(005)V99.
      *            VERBRAUCHTER BETRAG VORJAHR
               20  KSDS-GEWINN-UEBER-FSA-VJ     PIC  9(010)V99.
               20  KSDS-GEWINN-UEBER-FSA-LFDJ   PIC  9(010)V99.
      *            GEWINNE UEBER FSA IM VORJAHR/LFD. JAHR
               20  KSDS-AST-BEZ                 PIC  9(010)V99.
      *            GEZAHLTE ABGELTUNGSSTEUER INCL. SOLI LFD. JAHR
               20  KSDS-KIST-BEZ                PIC  9(010)V99.
      *            EINBEHALTENE KIRCHENSTEUER LFD. JAHR
               20  KSDS-SALDO-VERL-VERR-AKTIEN  PIC  9(010)V99.
      *            SALDO VERLUSTVERRECHNUNGSTOPF AKTIEN
               20  KSDS-SALDO-VERL-VERR         PIC  9(010)V99.
      *            SALDO VERLUSTVERRECHNUNGSTOPF
               20  KSDS-SALDO-QUELLST           PIC  9(010)V99.
      *            SALDO QUELLENSTEUERTOPF
               20  KSDS-VERL-VERR-KZ            PIC  X(001).
      *            KENNZEICHEN VERLUSTVERRECHNUNGSTOPF BESCHEINIGEN J/N
               20  KSDS-KONFESSION-INH1         PIC  X(002).
      *            KONFESSIONSSCHLUESSEL DEPOTINHABER 1
               20  KSDS-KONFESSION-INH2         PIC  X(002).
      *            KONFESSIONSSCHLUESSEL DEPOTINHABER 2
               20  KSDS-KIST-SATZ-INH1          PIC  9(002)V99.
      *            KIRCHENSTEUERSATZ DEPOTINHABER 1
               20  KSDS-KIST-SATZ-INH2          PIC  9(002)V99.
      *            KIRCHENSTEUERSATZ DEPOTINHABER 2
               20  KSDS-KIST-ANTRAG-GUELTIGAB   PIC  9(008).
      *            DATUM, AB DEM KIRCHENSTEUERANTRAG GILT (JHJJMMTT)
               20  KSDS-KIST-ANTEIL1            PIC  9(003)V99.
      *            PROZENTUALER ANTEIL KIRCHENSTEUER DEPOTINHABER 1
               20  KSDS-KIST-ANTEIL2            PIC  9(003)V99.
      *            PROZENTUALER ANTEIL KIRCHENSTEUER DEPOTINHABER 2
               20  FILLER                       PIC  X(070).
      *
      *---------------------------------------------------------------*
