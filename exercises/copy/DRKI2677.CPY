      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2677.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2677.CPv  $
      *
      *   Rev 2.1   Oct 18 2012 10:14:50   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Sep 06 2012 11:01:02   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 6 7 7   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Uebernahme der Daten nach Abloesung des BORDERO-Tools         *
      *  bei der CKV.                                                  *
      *    Tabelle CKVPERSHISTORIE(TCKI0452)                           *
      *            KRANKEN_VORSCHLAG_PERSON_HIST                       *
      *                                                                *
      *    SATZLÄNGE  110                                              *
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
      ******************************************************************
       01  DRKI2677.
           05 PHI-AKTUAL-KZ                  PIC X(01).
      *        I = INSERT (nur dieser Wert ist moeglich)
      **
081012     05 PHI-VERTRAGSNUMMER             PIC X(15).
      ** Vertragsnummer der Partner
           05 PHI-VERTRAGSID                 PIC X(13).
      ** kuenstlicher Schluessel
           05 PHI-PERSONENNR                 PIC 9(04).
      ** laufende PersonenenNr. fuer die Person
           05 PHI-DATUMHISTORIE              PIC 9(06).
      ** Datum der Historie in der Form JJJJMM
           05 PHI-TARIFHISTORIE              PIC X(12).
      ** Historientarif zu dem Historiendatum
           05 PHI-TAGESSATZ                  PIC S9(07)V99.
      **
           05 PHI-BEITRAG                    PIC S9(07)V99.
      ** Euro-Betrag für diese Person und das Historiedatum
           05 PHI-GESETZZUSCHLAG             PIC S9(07)V99.
      ** Gesetzlicher Zuschlag in Euro-Betrag
           05 PHI-RISIKOZUSCHLAG             PIC S9(07)V99.
      ** Risiko-Zuschlag in Euro-Betrag
           05 PHI-SELBSTBEHALT               PIC S9(07)V99.
      ** Selbstbehalt in Euro-Betrag
           05 PHI-GUTSCHRIFTBEF              PIC S9(07)V99.
      ** Zeitlich befristete Gutschrift.
           05 PHI-FILLER                     PIC X(05).
      *
      ******************************************************************
