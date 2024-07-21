      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0001.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRRW0001.CPv  $
      *
      *   Rev 2.6   Aug 05 2016 13:25:48   t13ae35
      *AVB01400: Projekt Lucy2 - DRRW0001 (jhe)
      *
      *   Rev 2.5   Jun 06 2016 15:26:52   t13ae35
      *AVB01400: Projekt Lucy2 - DRRW0001
      *
      *   Rev 2.4   May 19 2016 10:39:30   t13ae35
      *AVB01400: Projekt Lucy2 - DRRW0001
      *
      *   Rev 2.3   May 04 2016 17:38:22   t13ae35
      *AVB01400: Projekt Lucy2 - Copy DRRW0001 für FIBU-SST
      *
      *   Rev 2.2   07 Nov 2007 13:12:32   t13ae35
      *ARW00048: Zwischensummen im Buchungszeilen der VB-Abrechnung
      *
      *   Rev 2.1   17 May 2000 12:10:34   T13AE26
      *ASL00046 Förderbonus Auszahlung
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:23:22   T13AE35
      *AIS00009 - Neuerstellung, SST Generalimport
      *
      *   Rev 1.0   10 Mar 1998 17:18:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *                                                               *
      *            COPY-MEMBER  AllData Generalimport                 *
      *            DATEINUMMER:                                       *
      *            COPYMEMBER:  DRRW0001                              *
      *            SATZLAENGE:  1025 Bytes                            *
      *                                                               *
      *  Siehe:  ALLIS FIN/CO                                         *
      *          Schnittstellenbeschreibung Generalimport             *
      *                                                               *
      *  Änderungen an diesem Copymember bitte unbedingt nach Aachen  *
      *  (Ansprechpartner Hr. O.Wilhelm) melden!                      *
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00030!19.03.98!01.1! NEU                       ! Moll     *
      * -------------------------------------------------------------*
      *         !22.04.98!01.2! VB-Nr incl. Prüfz. UniTex1!          *
      *         !        !    ! Kontokennz.        UniTex2!          *
      *         !        !    ! Susis-Kz           UniTex3!          *
      *         !        !    ! Betrag  jetzt PIC 9(16)   ! T. Moll  *
      * -------------------------------------------------------------*
      * AIS00009!15.06.98!01.3! Verarbeitungstag für      !          *
      *         !        !    ! Finanzstatus     UniDatum3! J.Hering *
      * -------------------------------------------------------------*
      *         !01.07.98!01.4! Filler aus UniversalText  !          *
      *         !        !    ! ersetzt, damit freier     !          *
      *         !        !    ! Bereich statt X'0' mit    !          *
      *         !        !    ! Blanks gefüllt wird       ! T.Moll   *
      * -------------------------------------------------------------*
      * ASL00046!12.05.00!02.1! Förderbonus: Zur Info wird!          *
      *         !        !    ! VB-Nr des Untst. benötigt !          *
      *         !        !    ! FÜR den der (angebuchte)  !          *
      *         !        !    ! Betreuer FBonus erhält;   !          *
      *         !        !    ! Feld 77 umsatzArt geändert! T. Moll  *
      * -------------------------------------------------------------*
      * ARW00048!05.11.07!02.2! KTY/KNR-BUCH-TYP für      !          *
      *         !        !    ! courtageBasisBetrag       !          *
      *         !        !    ! (547-554)                 ! J.Hering *
      * -------------------------------------------------------------*
140416* AVB01400!14.04.16! 2.3! Lucy2-Felder hinzu        ! J.Hering *
      * -------------------------------------------------------------*
190516* AVB01400!19.05.16! 2.4! Filler umbenannt          !          *
      *         !        !    ! damit freier  mit Blanc   !          *
      *         !        !    ! und nicht X'0' init. wird ! J.Hering *
      * -------------------------------------------------------------*
060616* AVB01400!06.06.16! 2.5! abrechungsNr nach         !          *
      *         !        !    ! abrechnungsNr             ! J.Hering *
      * -------------------------------------------------------------*
270716* AVB01400!27.07.16! 2.6! nettoBetrag als eigenes   !          *
      *         !        !    ! Feld                      ! J.Hering *
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
 
       01  AFC-GI-SATZ.
           05  firma                   PIC  X(03).
           05  tz-01                   PIC  X(01).
           05  statusKz                PIC  X(01).
           05  tz-02                   PIC  X(01).
           05  waehrungsKz             PIC  X(03).
           05  tz-03                   PIC  X(01).
           05  kontoRolle              PIC  X(03).
           05  tz-04                   PIC  X(01).
           05  konto                   PIC  X(12).
           05  tz-05                   PIC  X(01).
           05  buchPeriode             PIC  X(06).
           05  tz-06                   PIC  X(01).
           05  belegDatum              PIC  X(08).
           05  tz-07                   PIC  X(01).
           05  buchDatum               PIC  X(08).
           05  tz-08                   PIC  X(01).
           05  belegArt                PIC  X(03).
           05  tz-09                   PIC  X(01).
           05  belegNr                 PIC  X(07).
           05  tz-10                   PIC  X(01).
           05  betrag                  PIC  9(16).
           05  tz-11                   PIC  X(01).
           05  shkz                    PIC  X(01).
           05  tz-12                   PIC  X(01).
           05  textSchluessel          PIC  X(03).
           05  tz-13                   PIC  X(01).
           05  buchText                PIC  X(40).
           05  tz-14                   PIC  X(01).
           05  gegenKontoRolle         PIC  X(03).
           05  tz-15                   PIC  X(01).
           05  gegenKonto              PIC  X(12).
           05  tz-16                   PIC  X(01).
           05  aussenStelle            PIC  X(08).
           05  tz-17                   PIC  X(01).
           05  kostenStelle            PIC  X(06).
           05  tz-18                   PIC  X(01).
           05  sparte                  PIC  X(08).
           05  tz-19                   PIC  X(01).
           05  projekt                 PIC  X(08).
           05  tz-20                   PIC  X(01).
           05  zeichnungsJahr          PIC  X(04).
           05  tz-21                   PIC  X(01).
           05  fremdWaehrung           PIC  X(03).
           05  tz-22                   PIC  X(01).
270716*--- Bruttobetrag für alle Sätze gefüllt ------------------------
           05  betragHw                PIC  9(16).
           05  tz-23                   PIC  X(01).
           05  extKonto                PIC  X(12).
           05  tz-24                   PIC  X(01).
140416*    05  extGegenKonto           PIC  X(12).
140416*    Aufwandskontoinformation aus dem Gegenkonto der org. Buchung-
140416*    zur Kategorisierung gleichartiger Leistungen für die VB-Abr.-
140416     05  aufwandsKontoInfo       PIC  X(12).
           05  tz-25                   PIC  X(01).
           05  faelligAm               PIC  X(08).
           05  tz-26                   PIC  X(01).
           05  versNr                  PIC  X(17).
           05  tz-27                   PIC  X(01).
           05  rechnungsNr             PIC  X(15).
           05  tz-28                   PIC  X(01).
           05  schadenNr               PIC  X(20).
           05  tz-29                   PIC  X(01).
           05  erhebungVon             PIC  X(08).
           05  tz-30                   PIC  X(01).
           05  erhebungBis             PIC  X(08).
           05  tz-31                   PIC  X(01).
           05  vermittler              PIC  X(12).
           05  tz-32                   PIC  X(01).
           05  fuehrenderVersicherer   PIC  X(12).
           05  tz-33                   PIC  X(01).
           05  versicherer             PIC  X(12).
           05  tz-34                   PIC  X(01).
           05  vrAnteil                PIC  X(05).
           05  tz-35                   PIC  X(01).
           05  referenz                PIC  X(17).
           05  tz-36                   PIC  X(01).
           05  kzKfz                   PIC  X(12).
           05  tz-37                   PIC  X(01).
           05  inkassoArt              PIC  X(03).
           05  tz-38                   PIC  X(01).
           05  zahlweise               PIC  X(03).
           05  tz-39                   PIC  X(01).
           05  versSteuerSchluessel    PIC  X(01).
           05  tz-40                   PIC  X(01).
           05  versSteuerSatz          PIC  X(05).
           05  tz-41                   PIC  X(01).
           05  versSteuer              PIC  9(16).
           05  tz-42                   PIC  X(01).
           05  feuerSchutzSteuerSatz   PIC  X(05).
           05  tz-43                   PIC  X(01).
           05  feuerSchutzSteuer       PIC  9(16).
           05  tz-44                   PIC  X(01).
           05  nettoPraemie            PIC  9(16).
           05  tz-45                   PIC  X(01).
           05  gebuehr                 PIC  9(16).
           05  tz-46                   PIC  X(01).
           05  provisionsSatz          PIC  X(05).
           05  tz-47                   PIC  X(01).
           05  provision               PIC  9(16).
           05  tz-48                   PIC  X(01).
           05  provisionsBasis         PIC  X(01).
           05  tz-49                   PIC  X(01).
           05  provisionsBasisBetrag   PIC  9(16).
           05  tz-50                   PIC  X(01).
           05  courtageSatz            PIC  X(05).
           05  tz-51                   PIC  X(01).
280716*    05  courtage                PIC  9(16).
280716*----UST-Betrag wie er aus Diamant kommt (gefüllt per PMIS0004)---
280716     05  ustBetrag2              PIC  9(16).
           05  tz-52                   PIC  X(01).
           05  courtageBasis           PIC  X(01).
           05  tz-53                   PIC  X(01).
051107*    05  courtageBasisBetrag     PIC  9(16).
051107     05  KtyBuchTyp              PIC  9(04).
051107     05  KnrBuchTyp              PIC  9(04).
051107*    05  courtageBasisBetragRest PIC  9(08).
140416*----kennzeichnet was mit der Buchung bisher in der STAB geschah -
140416*----siehe SF14 KTY=401   ----------------------------------------
140416     05  KtyVerArbTyp            PIC  9(04).
140416     05  KnrVerArbTyp            PIC  9(04).
           05  tz-54                   PIC  X(01).
           05  provisionSatzFuehrende  PIC  X(05).
           05  tz-55                   PIC  X(01).
140416*    05  provisionFuehrende      PIC  9(16).
140416*----lfdNummer der orginären Buchung auf die sich die ------------
140416*----Korrekturbuchung in der Schattentabelle bezieht  ------------
140416     05  bezugsLfd               PIC  9(16).
           05  tz-56                   PIC  X(01).
140416*    05  ratenZuschlag           PIC  9(16).
140416*    zum Zusammenhalt gleichartiger Leistungen in der VB-Abrechn.-
140416*    abgeleitet aus aufwandsKontoInfo siehe SF14 KTY=403  --------
140416     05  KtyLeistArt             PIC  9(04).
140416     05  KnrLeistArt             PIC  9(04).
140416*    aktuell gültiger Regelumsatzsteuersatz ----------------------
140416*    nur für umsatzsteuerpflichtige Leistungen gefüllt, sonst 0 --
140416*    01900 -> 19% , 02000 -> 20% ---------------------------------
140416     05  steuerSatz              PIC  9(05).
140416     05  ratenZuschlagRest       PIC  9(03).
           05  tz-57                   PIC  X(01).
           05  buchungsKzIntern        PIC  X(02).
           05  tz-58                   PIC  X(01).
           05  buchungsVermerk         PIC  X(02).
           05  tz-59                   PIC  X(01).
           05  belegDatumAlt           PIC  X(08).
           05  tz-60                   PIC  X(01).
           05  offenerPosten           PIC  X(07).
           05  tz-61                   PIC  X(01).
           05  postenZuordnung         PIC  X(03).
           05  tz-62                   PIC  X(01).
           05  intLfdNummer            PIC  X(25).
           05  tz-63                   PIC  X(01).
           05  lfdNummer               PIC  X(25).
           05  tz-64                   PIC  X(01).
           05  versNehmer              PIC  X(12).
           05  tz-65                   PIC  X(01).
           05  beitragsZahler          PIC  X(12).
           05  tz-66                   PIC  X(01).
           05  provisionsArt           PIC  X(01).
           05  tz-67                   PIC  X(01).
           05  sollStellungsArt        PIC  X(01).
           05  tz-68                   PIC  X(01).
           05  herkunftsKennung        PIC  X(02).
           05  tz-69                   PIC  X(01).
           05  levStop                 PIC  9(01).
           05  tz-70                   PIC  X(01).
           05  zahlStop                PIC  9(01).
           05  tz-71                   PIC  X(01).
           05  mahnStop                PIC  9(01).
           05  tz-72                   PIC  X(01).
140416*    05  allgVertragsInfo        PIC  X(40).
140416*----falls der Buchungstext größer als 40 Byte ist, wird der Rest-
140416*----in Buchtext2 gefüllt, für VB-Abr. werden beide konkatiniert--
140416     05  buchText2               PIC  X(40).
           05  tz-73                   PIC  X(01).
           05  anzahlMitarbeiter       PIC  X(07).
           05  tz-74                   PIC  X(01).
           05  herkunftsLand           PIC  X(08).
           05  tz-75                   PIC  X(01).
           05  gesellschaft            PIC  X(08).
           05  tz-76                   PIC  X(01).
120500*    * Feld 77: VB-Nummer (incl. Prüfziffer; 8-stellig)
120500*    *          des Unterstellten, der die Buchung für den
120500*    *          (angebuchten) Betreuer verursacht hat
120500*    05  umsatzArt               PIC  X(08).
120500     05  partnerNr               PIC  X(08).
 
           05  tz-77                   PIC  X(01).
           05  externBelNr             PIC  X(07).
           05  tz-78                   PIC  X(01).
140416*    05  universalBetrag1        PIC  9(16).
140416* ---Betrag incl. Umsatzsteuer nur für U-Buchungen gefüllt ---
280716* ---für Überweisungen aber immer leer (PMIS0004) -----
140416     05  bruttoBetrag            PIC  9(16).
           05  tz-79                   PIC  X(01).
140416*    05  universalBetrag2        PIC  9(16).
140416* ---Umsatzsteuerbetrag nur für U-Buchungen gefüllt ---
280716* ---für Überweisungen aber immer leer (PMIS0004) -----
140416     05  ustBetrag               PIC  9(16).
           05  tz-80                   PIC  X(01).
270716*    05  universalBetrag3        PIC  9(16).
270716* ---nettoBetrag für alle Sätze gefüllt (PMIS0004) -----
270716     05  nettoBetrag             PIC  9(16).
           05  tz-81                   PIC  X(01).
           05  universalDatum1         PIC  X(08).
           05  tz-82                   PIC  X(01).
           05  universalDatum2         PIC  X(08).
           05  tz-83                   PIC  X(01).
      *    * Feld 84: Tag der Verarbeitung im Finanzstatus
      *    05  universalDatum3         PIC  X(08).
           05  FinStatVerarbDatum      PIC  X(08).
           05  tz-84                   PIC  X(01).
 
      *    * Feld 85: VB-Nummer in Feld universalText1
      *    *          8-stellig (incl. Prüfziffer)
      *    05  universalText1          PIC  X(40).
           05  VBNr                    PIC  X(08).
010798*    05  FILLER                  PIC  X(32).
010798     05  universalText1Rest      PIC  X(32).
 
           05  tz-85                   PIC  X(01).
 
      *    * Feld 86: Kontokennzeichen in Feld universalText2
      *    * 0..Diskont, 1..Rückst., 2...Vorschuss, 3..Abschreib. --*
      *    05  universalText2          PIC  X(40).
           05  KontoKz                 PIC  X(01).
010798*    05  FILLER                  PIC  X(39).
010798     05  universalText2Rest      PIC  X(39).
 
           05  tz-86                   PIC  X(01).
 
      *    * Feld 87: SUSIS-KZ (wird mit Einführung Diamant nicht ben.)*
      *    05  universalText3          PIC  X(40).
           05  SusisKz                 PIC  X(01).
140416* KZ ob UST berechnet wurde (J...Ja, N...Nein) ------------*
140416     05  ustBerechnet            PIC  X(01).
140416* steuerl. Status (U...Unternehmer, K..Kleinuntern.) ------------*
140416* nur für Buchungen aus STAB gefüllt sonst leer      ------------*
140416     05  zielStatus              PIC  X(01).
140416* J..Ja es ist Korrekturbuchung aus STAB, sonst nein ------------*
140416     05  korrBuchung             PIC  X(01).
140416* J..Ja es ist eine Umsatzsteuerpflicht. Buchung für U bzw KU ---*
140416     05  ustPflichtig            PIC  X(01).
140416* SteuerJahr (Jahr in dem die Buchung auf der Steuererkl. steht)-*
140416* kann nur bei Korrekturen aus STAB vom Turnus-Jahr abweichen ---*
140416     05  steuerJahr              PIC  X(04).
190516     05  steuerJahrRest          PIC  X(04).
140416* Bezugs-Jahr(Jahr auf die sich die Orginäre Buchung bezieht)----*
140416* kann bei manuelle Korrekturen vom Turnus-Jahr abweichen -------*
140416     05  bezugsJahr              PIC  X(04).
190516     05  bezugsJahrRest          PIC  X(04).
140416* die AbrechnungsNummer der Abrechnung auf der die Buchung ------*
140416* ausgewiesen wird (nur für STAB-Buchungen gefüllt) -------------*
060616     05  abrechnungsNr           PIC  X(15).
140416     05  universalText3Rest      PIC  X(04).
 
           05  tz-87                   PIC  X(01).
           05  anlagedatum             PIC  X(08).
           05  tz-88                   PIC  X(01).
           05  anlageSachbearbeiter    PIC  X(10).
 
 
 

