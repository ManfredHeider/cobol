      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2305.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2305.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:49:58   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Jun 18 2009 10:36:46   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:32   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 12 2009 17:10:12   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *    - Zwischenbereich DTA fuer die Aufbereitung des KSDS-Satzes *
      *      Baufinanzierungs-Datensatz BAUFIK                         *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.03.09! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
      *07.04.09! Zusaetzlichen Key integriert.        ! HSL     !      *
      *        !                                      !         !      *
      *11.08.11! Umstellung auf Neutral               ! SWZ     ! 2.3  *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *    Santander- Bank Baufinanzierungs-Datensatz BAUFIK           *
      *                  ZWSP - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   250 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
      **
       01 'XX'-ZWSP-BAUFIK.
      *
070409   05 'XX'-ZWSP-BAUFIK-KEY.
    >>     10 'XX'-ZWSP-KEY-VTRGNR        PIC 9(015).
070409     10 'XX'-ZWSP-KEY-ANZ-KONTEN    PIC 9(002).
      *
         05 'XX'-ZWSP-BAUFIK-DATEN.
      *
           10  'XX'-ZWSP-SA                 PIC  X(002).
      *        Satzart             = '07'
           10  'XX'-ZWSP-GES                PIC  9(002).
      *        Gesellschafts-Nr.   = '66'
      ***
      ***  ** VertragsNr. = Schluessel in der KSDS-Datei.
           10  'XX'-ZWSP-VTRGNR             PIC  9(015).
      *        VertragsNr. = '0000' fix
      *                    = Stellen 1-4 der BLZ
      *                    = Stellen 1-7 der StammNr.
           10  'XX'-ZWSP-BLZ                PIC  9(008).
      *        BLZ des Girokontos
           10  'XX'-ZWSP-KONTONR            PIC  9(010).
      *        Kontonummer der Baufinanzierung.
      *        Kontoart              Stelle 1
      *        Stammnummer           Stelle 2-8
      *        Anzahl der Konten     Stelle 9-10
           10  'XX'-ZWSP-DATUM-EROEFFNUNG   PIC  9(008).
      *        Datum Produkteroeffnung i.d.F. JHJJMMTT
           10  'XX'-ZWSP-DATUM-SCHLIESSUNG  PIC  9(008).
      *        Datum Produktschliessung i.d.F. JHJJMMTT
           10  'XX'-ZWSP-DARLEHNSSUMME      PIC  9(012)V99.
      *        Ursprungskreditbetrag
           10  'XX'-ZWSP-DAT-ENDE-ZINSFEST  PIC  9(008).
      *        Datum Ende der Zinsfestschreibung i.F.JHJJMMTT
           10  'XX'-ZWSP-AKT-DARLEHNBETRG
                               PIC S9(11)V99 SIGN LEADING SEPARATE.
      *        aktueller Darlehnsstand
           10  'XX'-ZWSP-BEGINN-TILGUNG     PIC  9(008).
      *        Ende Zeitraum Freijahre, Beginn der Tilgung
      *                                       i.d.F. JHJJMMTT
           10  'XX'-ZWSP-TILGUNGSART        PIC  X(001).
      *        A =Annuitätisch
      *        E=Endfällig
      *        G66 setzt numerischen Wert in 'A' bzw. 'E' um.
           10  'XX'-ZWSP-START-TILGUNGSSATZ PIC  9(002)V9999.
      *        Anfaengliche Tilgung in Prozent.
           10  'XX'-ZWSP-SOLL-ZINSSATZ      PIC  9(002)V9999.
      *        Effektivzins
           10  'XX'-ZWSP-ZAHLWEISE          PIC  9(001).
      *         1 = Jaehrlich
      *         2 = halbjaehrlich
      *         4 = vierteljaehrlich
      *         5 = zweimonatlich
      *         6 = monatlich
      *         9 = Forward Darlehen (wird in unser System K-ZAHLWEISE-
      *                               UNBEKANNT(-2) uebernommen.)
      *         0 = geloeschte Konten (wird in unser System K-ZAHLWEISE-
      *                                UNBEKANNT(-2) uebernommen.)
           10  'XX'-ZWSP-RATE-IN-EURO       PIC  9(012)V99.
      *        Annuitaet in Euro
           10  'XX'-ZWSP-DEL-KZ             PIC  X(001).
      *        DEL-Kennzeichen  ('D' oder Blank)
           10  'XX'-ZWSP-RESERVE            PIC  X(107).

      ******************************************************************
      *                 E N D E    COPY-MEMBER DRKI2305                *
      ******************************************************************
