      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2313.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2313.CPv  $
      *
      *   Rev 2.4   Aug 30 2011 12:50:34   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.3   Aug 04 2009 15:11:32   t13ex52
      *AKI00764: SEB-Bank Mandantenwechsel durchführen
      *
      *   Rev 2.2   Jun 18 2009 10:37:06   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:54   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 18 2009 10:23:38   t13ex52
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
      *31.07.09! Feld SEB-AUS-AKT-DARLEHENSBTRG mit   ! HSL     !      *
      *        ! Vorzeichen versehen.                 !         !      *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *           G66 - Bank Personalkreditdatensatz PKK               *
      *                 A U S G A B E  -  S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
110811*01 'XX'-PKK-AENDERUNG.
110811 01 'XX'-SATZ.
      *
         05 'XX'-VERARB-KNZ          PIC X(001).
      *     Verarbeitungskennzeichen
      *     I = Insert U = Update D = Delete

         05 'XX'-PKK-KEY.
           10 'XX'-VKEY-VTRGNR        PIC 9(015).
           10 'XX'-VKEY-ANZ-KONTEN    PIC 9(004).
      *
         05  'XX'-SA                 PIC  X(002).
      *      Satzart             = '08'
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
      *      Kontonummer der PK-Darlehens.
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
      *      'J' = Aenderung 'XX'-DATUM-SCHLIESSUNG

         05  'XX'-DAT-KREDITZUSAGE       PIC  9(008).
      *      Datum der Kreditzusage i.F.JHJJMMTT
         05  'XX'-DAT-KREDITZUSAGE-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-DAT-KREDITZUSAGE

         05  'XX'-DAT-KREDITAUSZHLG      PIC  9(008).
      *      Datum der Darlehnsauszahlung i.d.F. JHJJMMTT = 0
         05  'XX'-DAT-KREDITAUSZHLG-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-DAT-KREDITAUSZHLG

         05  'XX'-KREDITART          PIC  X(006).
      *      immer Blank.
         05  'XX'-KREDITART-UPD      PIC  X.
      *      'J' = Aenderung 'XX'-KREDITART

310709***05  'XX'-AKT-DARLEHENSBTRG      PIC  9(012)V99.
310709   05  'XX'-AKT-DARLEHENSBTRG      PIC  S9(012)V99.
      *      Saldo (Aktueller Darlehnsstand)
         05  'XX'-AKT-DARLEHENSBTRG-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-AKT-DARLEHENSBTRG

         05  'XX'-SOLL-ZINSSATZ      PIC  9(002)V9999.
      *      Sollzinssatz in Prozent p.a.
         05  'XX'-SOLL-ZINSSATZ-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-SOLL-ZINSSATZ

         05  'XX'-RATE-EURO          PIC 9(012)V99.
      *      Rate in Euro.
         05  'XX'-RATE-EURO-UPD      PIC X.
      *      'J' = Aenderung 'XX'-RATE-EURO

         05  'XX'-DAT-FAELLIG        PIC 9(008).
      *      Datum letzte Rate i.d.F. JHJJMMTT.
         05  'XX'-DAT-FAELLIG-UPD    PIC X.
      *      'J' = Aenderung 'XX'-DAT-FAELLIG

         05  'XX'-DEL-KZ             PIC  X(001).
      *      DEL-Kennzeichen  ('D' oder Blank)
         05  'XX'-DEL-KZ-UPD         PIC  X.
      *      'J' = Aenderung 'XX'-DEL-KZ

         05  'XX'-RESERVE            PIC  X(035).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2313                       *
      ******************************************************************
