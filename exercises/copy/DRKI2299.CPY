      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2299.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2299.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:49:38   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Jun 18 2009 10:36:36   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:20   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 06 2009 11:42:14   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - Ein- / AusgabeDatei KSDS Komplettbestand                 *
      *       Privatkredit-Datensatz                                   *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *10.03.09! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
      *15.04.09! Zusaetzlichen Key integriert.        ! HSL     !      *
      *        !                                      !         !      *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !2.2   *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *         Santander- Bank Privatkredit-Datensatz                 *
      *                  KSDS - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 10.03.2009      *
      *                                                                *
      **************************************************************** *
       01 'XX'-KSDS-PKK.
      *
150409   05 'XX'-KSDS-PKK-KEY.
    >>     10 'XX'-KSDS-VKEY-VTRGNR        PIC 9(015).
150409     10 'XX'-KSDS-VKEY-ANZ-KONTEN    PIC 9(004).

         05 'XX'-KSDS-PKK-DATEN.
      *
           10  'XX'-KSDS-SA                 PIC  X(002).
      *        Satzart             = '08'
           10  'XX'-KSDS-GES                PIC  9(002).
      *        Gesellschafts-Nr.   = '66'
           10  'XX'-KSDS-VTRGNR             PIC  9(015).
      *        VertragsNr. = '0000' fix
      *                    = Stellen 1-4 der BLZ
      *                    = Stellen 1-7 der StammNr.
           10  'XX'-KSDS-BLZ                PIC  9(008).
      *        BLZ des Girokontos
           10  'XX'-KSDS-KONTONR            PIC  9(010).
      *        Kontonummer der PK-Darlehens.
      *        Kontoart              Stelle 1
      *        Stammnummer           Stelle 2-8
      *        Anzahl der Konten     Stelle 9-10
           10  'XX'-KSDS-DATUM-EROEFFNUNG   PIC  9(008).
      *        Datum Produkteroeffnung i.d.F. JHJJMMTT
           10  'XX'-KSDS-DATUM-SCHLIESSUNG  PIC  9(008).
      *        Datum Produktschliessung i.d.F. JHJJMMTT
           10  'XX'-KSDS-DARLEHNSSUMME      PIC  9(012)V99.
      *        Ursprungskreditbetrag
           10  'XX'-KSDS-DAT-KREDITZUSAGE   PIC  9(008).
      *        Datum der Kreditzusage i.F.JHJJMMTT
           10  'XX'-KSDS-DAT-KREDITAUSZHLG  PIC  9(008).
      *        Datum der Darlehnsauszahlung i.d.F. JHJJMMTT = 0
           10  'XX'-KSDS-KREDITART          PIC  X(006).
      *        immer Blank.
           10  'XX'-KSDS-AKT-DARLEHENSBTRG
                                  PIC S9(11)V99 SIGN LEADING SEPARATE.
      *        Saldo (Aktueller Darlehnsstand)
           10  'XX'-KSDS-SOLL-ZINSSATZ      PIC  9(002)V9999.
      *        Sollzinssatz in Prozent p.a.
           10  'XX'-KSDS-RATE-EURO          PIC 9(012)V99.
      *        Rate in Euro.
           10  'XX'-KSDS-DAT-FAELLIG        PIC 9(008).
      *        Datum letzte Rate i.d.F. JHJJMMTT.
           10  'XX'-KSDS-DEL-KZ             PIC  X(001).
      *        DEL-Kennzeichen  ('D' oder Blank)
           10  'XX'-KSDS-RESERVE            PIC  X(049).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2299                       *
      ******************************************************************
