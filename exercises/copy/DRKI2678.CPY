      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2678.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2678.CPv  $
      *
      *   Rev 2.5   Oct 02 2017 11:04:22   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.4   Sep 14 2017 14:02:04   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.3   Sep 06 2017 14:14:54   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.2   Aug 08 2013 09:26:22   t13ex52
      *AKI01226: G50 CENTRAL BAP Tarifumstellung 2013
      *
      *   Rev 2.1   Oct 18 2012 10:14:54   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Sep 06 2012 11:01:08   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 6 7 8   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Uebernahme der Daten nach Abloesung des BORDERO-Tools         *
      *  bei der CKV.                                                  *
      *    Tabelle CKVPERSUMSTTARIF(TCKI0453)                          *
      *            KRANKEN_VORSCHLAG_PERS_UMSTELL                      *
      *                                                                *
      *    SATZLÄNGE  80                                               *
180713*    Neu: Satzlänge 100 nach Integration Feld "Selbstbehalt"     *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !     AENDERUNG                         !NAME       !VER*
      *AUFTRAG !                                       !           !   *
      *--------!---------------------------------------!-----------!---*
      *17.09.12!NEUERSTELLUNG                          !Schlenther !01 *
      *AKI01147!                                       !           !   *
      *--------!---------------------------------------!-----------!---*
      *08.10.12!Auf Wunsch von HHU Uebernahme Vertrags-!Schlenther !   *
      *        !nummer in Eingabedatei                 !           !   *
      *        !                                       !           !   *
      *--------!---------------------------------------!---------------*
      *18.07.13! Integration zusätzliches Feld         ! Schlenther    *
      *AKI01226! "Selbstbehalt" in der Satzart D400    !               *
      *   UE001!                                       !               *
      *--------!---------------------------------------!---------------*
      *--------!---------------------------------------!---------------*
      *05.09.17! Neues Feld LEISTUNG für Beitragsre-   ! tfl           *
      *AKI01544! duktionstarife EBE                    !               *
      *--------!---------------------------------------!---------------*
 
 
 
      ******************************************************************
       01  DRKI2678.
           05 PUM-AKTUAL-KZ                  PIC X(01).
      *        I = INSERT (nur dieser Wert ist moeglich)
081012     05 PUM-VERTRAGSNUMMER             PIC X(15).
      ** Vertragsnummer der Partner
           05 PUM-VERTRAGSID                 PIC X(13).
      ** kuenstlicher Schluessel
           05 PUM-PERSONENNR                 PIC 9(04).
      ** laufende PersonenenNr. fuer die Person
           05 PUM-TARIFUMSTELLUNG            PIC X(12).
      ** Umstellungstarif der Person (VP)
           05 PUM-BEITRAG                    PIC S9(07)V99.
      ** Umstellungs-Beitrag in Euro-Betrag
           05 PUM-RISIKOZUSCHLAG             PIC S9(07)V99.
      ** Umstellung Risiko Zuschlag in Euro-Betrag
           05 PUM-GUTSCHRIFTBEF              PIC S9(07)V99.
      ** Umstellung Zeitlich befristete Gutschrift in Euro-Betrag
      ** (Lieferung erst ab 2013-01)
180713     05 PUM-SELBSTBEHALT               PIC S9(07)V99.
      ** Selbstbehalt
050917     05 PUM-LEISTUNG                   PIC S9(07)V99.
050917     05 PUM-FILLER                     PIC X(10).
      *
      ******************************************************************
 
 

