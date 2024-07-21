      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2367.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2367.CPv  $
      *
      *   Rev 2.6   Mar 27 2017 09:29:04   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.5   Nov 25 2009 08:48:36   t13ex50
      *AKI00867: Programmübergreifende Copy-Strecken
      *
      *   Rev 2.4   Nov 20 2009 08:51:18   t13ex50
      *AKI00867: Programmübergreifende Copy-Strecken
      *
      *   Rev 2.3   Nov 13 2009 11:02:12   t13ex50
      *AKI00867: Programmübergreifende Copy-Strecken
      *
      *   Rev 2.2   Nov 12 2009 11:40:50   t13ex14
      *AKI00867: AllianzGI/FodB: MDTA-Kontrolle täglich
      *
      *   Rev 2.1   Oct 30 2009 10:43:50   t13ex48
      *AKI00867: Abgeltungssteuer für AllianzGI/FodB: MDTA-Kontrolle
      *
      *   Rev 2.0   Oct 27 2009 17:32:56   t13ex48
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR Abgeltungssteuer AGI (DTA)                  *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = ABGSTxxyy                                       *
      *    (xx: Gesellschaftskuerzel, yy: optional, 'KP' bei Kompl.Bst.*
      *                                                                *
      *    LÄNGE: 250 STELLEN                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME      !VER-*
      *AUFTRAG !                                       !          !SION*
      *--------!---------------------------------------!----------!----*
      *29.10.09! NEUERSTELLUNG                         ! HSE      ! 01 *
      *AKI00867!                                       !          !    *
      *--------!---------------------------------------!----------!----*
      *20.11.09! FELDLÄNGE AGI-N-ANZ GEÄNDERT          !SCHLEICHER!2.03*
      *--------!---------------------------------------!----------!----*
      *24.03.17! FVD entfernt                          !HHU       !    *
      *--------!---------------------------------------!----------!----*
      *
       01  DRKI2367.
           10  AGI-AST-VORSATZ.
               20  AGI-V-SA                           PIC  X(001).
      *            SATZART = 'A'
               20  AGI-V-ABSENDER                     PIC  X(008).
      *            ABSENDER = 'FODB,HOF'
               20  AGI-V-DATUM                        PIC  9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  AGI-V-EMPFAENGER                   PIC  X(004).
      *            EMPFAENGER = 'DVAG'
               20  AGI-V-INHALT                       PIC  X(010).
      *            INHALTSANGABE:
      *            TÄGLICHE LIEFERUNG DVAG              'ABGSTDE'
      *            KOMPLETTBESTAND DVAG                 'ABGSTDEKP'
      *            TÄGLICHE LIEFERUNG ALLFINANZ         'ABGSTAF'
      *            KOMPLETTBESTAND ALLFINANZ            'ABGSTAFKP'
               20  FILLER                             PIC  X(219).
      *            RESERVE
 
      *
           10  AGI-AST-SATZ  REDEFINES  AGI-AST-VORSATZ.
               20  AGI-SA                             PIC  X(001).
      *            SATZART        = 'C'
               20  AGI-GES                            PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  AGI-KEY.
                   25  AGI-VNR                        PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES AGI-VNR.
                      30  AGI-VNR-1-13                PIC  9(013).
                      30  AGI-UNTER-NR                PIC  9(002).
      *            UNTERDEPOTNUMMER
               20  AGI-FSA-GUELTIGAB                  PIC  9(008).
      *            DATUM, AB DEM FREISTELLUNGSAUFTRAG GILT (JHJJMMTT)
               20  AGI-FSA-GUELTIGBIS                 PIC  9(008).
      *            DATUM, BIS ZU DEM FREISTELLUNGSAUFTRAG GILT
               20  AGI-FSA-ERTEILT                    PIC  9(005)V99.
      *            ERTEILTER FREISTELLUNGSAUFTRAG
               20  AGI-FSA-VERBRAUCHT                 PIC  9(005)V99.
      *            VERBRAUCHTER BETRAG
               20  AGI-DEPOTSTATUS-KZ                 PIC  X(001).
      *            DEPOT IST LOGISCH GELOESCHT                    'D'
      *            VERMITTLERAUSKUNFTSPERRE/WECHSEL VON DVAG WEG  'I'
      *            INFOSPERRE AUFGEHOBEN/WECHSEL ZUR DVAG         'S'
      *            REAKTIVIERUNG LOGISCH GELOESCHTEN DEPOTS       'R'
               20  AGI-STEUER-AUSL-KZ                 PIC  X(001).
      *            KENNZEICHEN STEUERAUSLAENDER, WERTE: 'J'/'N'/LEER
               20  AGI-NV-GUELTIGBIS                  PIC  9(008).
      *            DATUM, BIS ZU DEM NICHTVERANL.-BESCHEINIGUNG GILT
               20  AGI-FSA-VERBRAUCHT-VJ              PIC  9(005)V99.
      *            VERBRAUCHTER BETRAG VORJAHR
               20  AGI-GEWINN-UEBER-FSA-VJ            PIC  9(010)V99.
               20  AGI-GEWINN-UEBER-FSA-LFDJ          PIC  9(010)V99.
      *            GEWINNE UEBER FSA IM VORJAHR/LFD. JAHR
               20  AGI-AST-BEZ                        PIC  9(010)V99.
      *            GEZAHLTE ABGELTUNGSSTEUER INCL. SOLI LFD. JAHR
               20  AGI-KIST-BEZ                       PIC  9(010)V99.
      *            EINBEHALTENE KIRCHENSTEUER LFD. JAHR
               20  AGI-SALDO-VERL-VERR-AKTIEN         PIC  9(010)V99.
      *            SALDO VERLUSTVERRECHNUNGSTOPF AKTIEN
               20  AGI-SALDO-VERL-VERR                PIC  9(010)V99.
      *            SALDO VERLUSTVERRECHNUNGSTOPF
               20  AGI-SALDO-QUELLST                  PIC  9(010)V99.
      *            SALDO QUELLENSTEUERTOPF
               20  AGI-VERL-VERR-KZ                   PIC  X(001).
      *            KENNZEICHEN VERLUSTVERRECHNUNGSTOPF BESCHEINIGEN J/N
               20  AGI-KONFESSION-INH1                PIC  X(002).
      *            KONFESSIONSSCHLUESSEL DEPOTINHABER 1
               20  AGI-KONFESSION-INH2                PIC  X(002).
      *            KONFESSIONSSCHLUESSEL DEPOTINHABER 2
               20  AGI-KIST-SATZ-INH1                 PIC  9(002)V99.
      *            KIRCHENSTEUERSATZ DEPOTINHABER 1
               20  AGI-KIST-SATZ-INH2                 PIC  9(002)V99.
      *            KIRCHENSTEUERSATZ DEPOTINHABER 2
               20  AGI-KIST-ANTRAG-GUELTIGAB          PIC  9(008).
      *            DATUM, AB DEM KIRCHENSTEUERANTRAG GILT (JHJJMMTT)
               20  AGI-KIST-ANTEIL1                   PIC  9(003)V99.
      *            PROZENTUALER ANTEIL KIRCHENSTEUER DEPOTINHABER 1
               20  AGI-KIST-ANTEIL2                   PIC  9(003)V99.
      *            PROZENTUALER ANTEIL KIRCHENSTEUER DEPOTINHABER 2
               20  FILLER                             PIC  X(070).
      *
           10  AGI-AST-NACHSATZ    REDEFINES     AGI-AST-VORSATZ.
      *
               20  AGI-N-SA                           PIC  X(001).
      *            SATZART             'E'
               20  AGI-N-ANZ                          PIC  9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER                             PIC  X(240).
      *            RESERVE
      *---------------------------------------------------------------*

