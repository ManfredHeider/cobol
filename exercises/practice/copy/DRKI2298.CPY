      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2298.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2298.CPv  $
      *
      *   Rev 2.5   Aug 30 2011 12:49:32   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.4   Jul 11 2011 09:41:38   t13ex15
      *FKI01344: Santander Fehlerbereinigung PGM PMKI0979
      *
      *   Rev 2.3   Aug 13 2010 09:23:52   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB PGM-Änderung wg. Zusä
      *tzliche Felder
      *
      *   Rev 2.2   Jun 18 2009 10:36:32   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:16   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 06 2009 11:42:06   t13ex52
      *Archiverstellung
      *
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - Ein- / AusgabeDatei KSDS Komplettbestand                 *
      *       Baufinanzierungs-Datensatz                               *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *10.03.09! NEU                                  ! HSL     !      *
      *--------!--------------------------------------!---------!----- *
      *07.04.09! Zusaetzlichen Key integriert.        ! HSL     !      *
      *--------!--------------------------------------!---------!----- *
      *14.06.10! "Nominalzinssatz"                    ! SWZ     !      *
      *        ! "Darlehensart" mit  Inhalt           !         !      *
      *        ! A=KFW-Darlehen, B=Zwischenfinanzier.,!         !      *
      *        ! C=Forward-Darlehen, D=Baudarlehen    !         !      *
      *--------!--------------------------------------!---------!----- *
      *08.07.11! Sollzins ist an der falschen Stelle  ! SWZ     !      *
      *        ! Filler auf 76 reduziert              !         !      *
      *--------!--------------------------------------!---------!----- *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
110811*              Ges. 66 -  Baufinanzierungs-Datensatz             *
      *                  KSDS - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   250 BYTES                 STAND: 10.03.2009      *
      *                                                                *
      **************************************************************** *
      **
       01 'XX'-KSDS-BAUFIK.
      *
070409   05 'XX'-KSDS-BAUFIK-KEY.
    >>     10 'XX'-KSDS-VKEY-VTRGNR        PIC 9(015).
070409     10 'XX'-KSDS-VKEY-ANZ-KONTEN    PIC 9(004).

         05 'XX'-KSDS-BAUFIK-DATEN.
      *
           10  'XX'-KSDS-SA                 PIC  X(002).
      *        Satzart             = '07'
           10  'XX'-KSDS-GES                PIC  9(002).
      *        Gesellschafts-Nr.   = '66'
      ***
      ***  ** VertragsNr. = Schluessel in der KSDS-Datei.
           10  'XX'-KSDS-VTRGNR             PIC  9(015).
      *        VertragsNr. = '0000' fix
      *                    = Stellen 1-4 der BLZ
      *                    = Stellen 1-7 der StammNr.
           10  'XX'-KSDS-BLZ                PIC  9(008).
      *        BLZ des Girokontos
           10  'XX'-KSDS-KONTONR            PIC  9(010).
      *        Kontonummer der Baufinanzierung.
      *        Kontoart              Stelle 1
      *        Stammnummer           Stelle 2-8
      *        Anzahl der Konten     Stelle 9-10
           10  'XX'-KSDS-DATUM-EROEFFNUNG   PIC  9(008).
      *        Datum Produkteroeffnung i.d.F. JHJJMMTT
           10  'XX'-KSDS-DATUM-SCHLIESSUNG  PIC  9(008).
      *        Datum Produktschliessung i.d.F. JHJJMMTT
           10  'XX'-KSDS-DARLEHNSSUMME      PIC  9(012)V99.
      *        Ursprungskreditbetrag
           10  'XX'-KSDS-DAT-ENDE-ZINSFEST  PIC  9(008).
      *        Datum Ende der Zinsfestschreibung i.F.JHJJMMTT
           10  'XX'-KSDS-AKT-DARLEHNBETRG
                                   PIC S9(11)V99 SIGN LEADING SEPARATE.
      *        aktueller Darlehnsstand
           10  'XX'-KSDS-BEGINN-TILGUNG     PIC  9(008).
      *      Ende Zeitraum Freijahre, Beginn der Tilgung i.d.F. JHJJMMTT
           10  'XX'-KSDS-TILGUNGSART        PIC  X(001).
      *        A =Annuitätisch
      *        E=Endfällig
      *        G66 setzt numerischen Wert in 'A' bzw. 'E' um.
           10  'XX'-KSDS-START-TILGUNGSSATZ PIC  9(002)V9999.
      *        Anfaengliche Tilgung in Prozent.
140610*    10  'XX'-KSDS-SOLL-ZINSSATZ      PIC  9(002)V9999.
           10  'XX'-KSDS-EFFEKTIV-ZINSSATZ  PIC  9(002)V9999.
      *        Effektivzins
           10  'XX'-KSDS-ZAHLWEISE          PIC  9(001).
      *        1 = Jaehrlich
      *        2 = halbjaehrlich
      *        3 = Alle-3-Jahre
      *        4 = vierteljaehrlich
      *        5 = zweimonatlich
      *        6 = monatlich
      *        7 = Einmalzahlung
      *        9 = Forward Darlehen
      *        0 = geloeschte Konten
           10  'XX'-KSDS-RATE-IN-EURO       PIC  9(012)V99.
      *        Annuitaet in Euro
           10  'XX'-KSDS-KEY-KZ             PIC  X(003).
      *         fixer Wert + letzte beiden Stellen der Kontonummer
      *         SAP 9 + letzte 2 Stellen  Kontonr.,
      *         GEK 1 + letzte 2 Stellen  Kontonr.
           10  'XX'-KSDS-DEL-KZ             PIC  X(001).
      *        DEL-Kennzeichen  ('D' oder Blank)
140610     10  'XX'-KSDS-DARLEHENSART       PIC  X(001).
140610*         Baufinanzierungsart
140610*         A = KFW-Darlehen
140610*         B = Zwischenfinanzierung
140610*         C = Forward-Darlehen
140610*         D = Baudarlehen
080711*    hier ist ein leeres Feld mittendrinn
080711     10  'XX'-KSDS-FELD-NICHT-GENUTZT PIC  X(019).
140610     10  'XX'-KSDS-SOLL-ZINSSATZ      PIC  9(002)V9999.
140610*        Sollzins (Nominalzins in %)
140610*    10  'XX'-KSDS-RESERVE            PIC  X(102).
140610*    10  'XX'-KSDS-RESERVE            PIC  X(095).
080711     10  'XX'-KSDS-RESERVE            PIC  X(076).

      ******************************************************************
      *                 E N D E    COPY-MEMBER DRKI2298                *
      ******************************************************************
