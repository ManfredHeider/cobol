      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2676.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2676.CPv  $
      *
      *   Rev 2.2   Oct 22 2018 08:20:56   t13ae31
      *AKI01630: CKV Bordero neues Feld Programme
      *
      *   Rev 2.1   Oct 18 2012 10:14:44   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Sep 06 2012 11:00:56   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 6 7 6   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Uebernahme der Daten nach Abloesung des BORDERO-Tools         *
      *  bei der CKV.                                                  *
      *    Tabelle CKVPERSVORSCHLAG(TCKI0451)/KRANKEN_VORSCHLAG_PERSON *
      *                                                                *
      *    SATZLÄNGE  70                                               *
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
      *--------!---------------------------------------!---------------*
110918*11.09.18! Neues Feld Beitragsvorteil SA200      ! HHU           *
      *AKI01630!                                       !               *
      ******************************************************************
       01  DRKI2676.
           05 VPS-AKTUAL-KZ                  PIC X(01).
      *        I = INSERT (nur dieser Wert ist moeglich)
      **
081012     05 VPS-VERTRAGSNUMMER             PIC X(15).
      ** Vertragsnummer der Partner
           05 VPS-VERTRAGSID                 PIC X(13).
      ** kuenstlicher Schluessel
           05 VPS-PERSONENNR                 PIC 9(04).
      ** laufende PersonenenNr. fuer die Person
           05 VPS-PERSERHOEHPRZ              PIC S9(07)V99.
      ** Erhoehung in Prozent fuer die Person
           05 VPS-PERSRUECKSTLG              PIC S9(07)V99.
      ** Altersrueckstellung fuer die Person
           05 VPS-PERSBEITRRUEK              PIC S9(07)V99.
      ** Beitragsrueckerstattung fuer die Person
           05 VPS-KUNDBINDPERS               PIC X.
      ** Kundenbindung fuer die Person
110918     05 VPS-BEITRAGSVORTEIL            PIC S9(07)V99.
      ** Beitragsvorteil einer Person
110918***  05 VPS-FILLER                     PIC X(09).
      *
      ******************************************************************

