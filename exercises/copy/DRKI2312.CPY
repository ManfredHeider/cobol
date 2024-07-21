      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2312.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2312.CPv  $
      *
      *   Rev 2.4   Aug 30 2011 12:50:28   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.3   Aug 13 2010 09:23:56   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB PGM-Änderung wg. Zusä
      *tzliche Felder
      *
      *   Rev 2.2   Jun 18 2009 10:37:00   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:50   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 18 2009 10:23:30   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - AUSGABEDATEI G66 Aufbau wie DTA-Eingabe, erweitert       *
      *       mit einem Feld fuer das Aenderungskennzeichen je Attribut*
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.03.09! NEU                                  ! HSL     !      *
140610*14.10.06! Neue Felder Nominalzins und Darl.Art ! SWZ     !      *
      *        ! Nominalzins steht in Sollzins        !         !      *
110811*11.08.11! Umstellung auf Neutral               ! SWZ     !2.4   *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *         G66 - Bank Baufinanzierungsdatensatz BAUFIK            *
      *                 A U S G A B E  -  S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   250 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
110811*01 'XX'-BAUFIK-AENDERUNG.
110811 01 'XX'-SATZ.
      *
         05 'XX'-VERARB-KNZ          PIC X(001).
      *     Verarbeitungskennzeichen
      *     I = Insert U = Update D = Delete
      *
         05 'XX'-BAUFIK-KEY.
           10 'XX'-VKEY-VTRGNR        PIC 9(015).
           10 'XX'-VKEY-ANZ-KONTEN    PIC 9(004).
      *
         05  'XX'-SA                 PIC  X(002).
      *      Satzart             = '07'
         05  'XX'-GES                PIC  9(002).
      *      Gesellschafts-Nr.   = '66'
         05  'XX'-VTRGNR             PIC  9(015).
      *      VertragsNr. = '0000' fix
      *                  = Stellen 1-4 der BLZ
      *                  = Stellen 1-7 der StammNr.
         05  'XX'-BLZ                PIC  9(008).
      *      BLZ des Girokontos
         05  'XX'-BLZ-UPD            PIC  X.
      *      'J' = Aenderung 'XX'-BLZ

         05  'XX'-KONTONR            PIC  9(010).
      *      Kontonummer der Baufinanzierung.
      *      Kontoart              Stelle 1
      *      Stammnummer           Stelle 2-8
      *      Anzahl der Konten     Stelle 9-10
         05  'XX'-KONTONR-UPD        PIC  X.
      *      'J' = Aenderung 'XX'-KONTONR

         05  'XX'-DATUM-EROEFFNUNG   PIC  9(008).
      *      Datum Produkteroeffnung i.d.F. JHJJMMTT
         05  'XX'-OPENING-DATE-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-DATUM-EROEFFNUNG

         05  'XX'-DATUM-SCHLIESSUNG  PIC  9(008).
      *      Datum Produktschliessung i.d.F. JHJJMMTT
         05  'XX'-LOCKING-DATE-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-DATUM-SCHLIESSUNG

         05  'XX'-DARLEHNSSUMME      PIC  9(012)V99.
      *      Ursprungskreditbetrag
         05  'XX'-DARLEHNSSUMME-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-DARLEHNSSUMME

         05  'XX'-DAT-ENDE-ZINSFEST      PIC  9(008).
      *      Datum Ende der Zinsfestschreibung i.F.JHJJMMTT
         05  'XX'-DAT-ENDE-ZINSFEST-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-DAT-ENDE-ZINSFEST

         05  'XX'-AKT-DARLEHNBETRG       PIC  S9(012)V99.
      *      aktueller Darlehnsstand
         05  'XX'-AKT-DARLEHNBETRG-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-AKT-DARLEHNBETRG

         05  'XX'-BEGINN-TILGUNG         PIC  9(008).
      *      Ende Zeitraum Freijahre, Beginn der Tilgung i.d.F. JHJJMMTT
         05  'XX'-BEGINN-TILGUNG-UPD     PIC  X.
      *      'J' = Aenderung 'XX'-BEGINN-TILGUNG

         05  'XX'-TILGUNGSART        PIC  X(001).
      *      A =Annuitätisch
      *      E=Endfällig
      *      G66 setzt numerischen Wert in 'A' bzw. 'E' um.
         05  'XX'-TILGUNGSART-UPD    PIC  X.
      *      'J' = Aenderung 'XX'-BEGINN-TILGUNG

         05  'XX'-START-TILGUNGSSATZ     PIC  9(002)V9999.
      *      Anfaengliche Tilgung in Prozent.
         05  'XX'-START-TILGUNGSSATZ-UPD PIC  X.
      *      'J' = Aenderung 'XX'-START-TILGUNGSSATZ

      *  05  'XX'-SOLL-ZINSSATZ      PIC  9(002)V9999.
140610   05  'XX'-EFFEKTIV-ZINSSATZ  PIC  9(002)V9999.
      *      Effektivzins
         05  'XX'-EFFEKTIV-ZINSSATZ-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-SOLL-ZINSSATZ

         05  'XX'-ZAHLWEISE          PIC  9(001).
      *      1 = Jaehrlich
      *      2 = halbjaehrlich
      *      4 = vierteljaehrlich
      *      5 = zweimonatlich
      *      6 = monatlich
      *      9 = Forward Darlehen
      *      0 = geloeschte Konten
         05  'XX'-ZAHLWEISE-UPD      PIC  X.
      *      'J' = Aenderung 'XX'-ZAHLWEISE

         05  'XX'-RATE-IN-EURO       PIC  9(012)V99.
      *      Annuitaet in Euro
         05  'XX'-RATE-IN-EURO-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-RATE-IN-EURO

         05  'XX'-DEL-KZ             PIC  X(001).
      *      DEL-Kennzeichen  ('D' oder Blank)
         05  'XX'-DEL-KZ-UPD         PIC  X.
      *      'J' = Aenderung 'XX'-DEL-KZ
140610   05  'XX'-DARLEHENSART       PIC  X(001).
140610*      Baufinanzierungsart
140610*      A = KFW-Darlehen
140610*      B = Zwischenfinanzierung
140610*      C = Forward-Darlehen
140610*      D = Baudarlehen
140610   05  'XX'-DARLEHENSART-UPD   PIC  X.
140610*      'J' = Aenderung 'XX'-DARLEHENSART
         05  'XX'-SOLL-ZINSSATZ      PIC  9(002)V9999.
140610*      Nominalzins in %
         05  'XX'-SOLL-ZINSSATZ-UPD  PIC  X.
140610*      'J' = Aenderung 'XX'-ZINS-NOMINAL


      *  05  'XX'-RESERVE            PIC  X(090).
140610   05  'XX'-RESERVE            PIC  X(081).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2312                       *
      ******************************************************************
