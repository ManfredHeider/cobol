      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2577.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2577.CPv  $
      *
      *   Rev 2.3   Sep 12 2016 11:04:20   t13ex78
      *AKI01463: Programme, Copies ect.
      *
      *   Rev 2.2   Sep 20 2012 11:06:00   t13ae31
      *AKI01166: CKV Daten zu mitversicherten Personen
      *
      *   Rev 2.1   Jun 26 2012 17:03:42   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 13 2012 19:24:36   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *            ########    D R K I 2 5 7 7   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Umstellung der CKV-Verarbeitung woechentlich Komplettbestand  *
      *  auf die CKV-Verarbeitung taeglich Deltabestand.               *
      *    AUFBEREITEN der Daten für versicherte Personen              *
      *                                                                *
      *    SATZLÄNGE   250                                             *
      *                                                                *
      *                                                                *
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !     AENDERUNG                       !NAME       !VERS.*
      *AUFTRAG !                                     !           !     *
      *--------!-------------------------------------!-----------!-----*
      *25.04.12!NEUERSTELLUNG                        !Schlenther ! 1.0 *
      *AKI01100!                                     !           !     *
      *--------!-------------------------------------!-----------!-----*
      *09.05.12!Integration Nachname                 !Schlenther ! 1.1 *
      *AKI01100!                                     !           !     *
      * ----------------------------------------------------------------
100912*AKI01166! Adressdaten VP                      !P. Hübner  !     *
      *--------!-------------------------------------!-----------!-----*
250816*AKI01463! Korrespondenzmakler und Fremdver-   ! tfl       !     *
      *        ! sichererdaten                       !           !     *
      *        ! Satzlaenge von 227 auf 250 erhoeht  !           !     *
      *        !                                     !           !     *
      ******************************************************************
      *** Versicherte Person
         01 DRKI2577.
           05  VP-AKTUAL-KZ                PIC X(01).
      *
      *      I = INSERT
      *      U = UPDATE
      **
      ** ALLGEMEINE DATEN
           05  VP-GESELLSCHAFT             PIC 9(02).
      ** Wird vom ErmittlungsPGM immer mit "50" (CENTRAL) gefuellt.
           05  VP-VTRGNR                   PIC 9(15).
           05  VP-PERSONENNR               PIC 9(04).
 
alt   ** VERSTARVERSOBJEKT, VERSOBJEKT & VERPERSOND
neu   **  Daten für DRKI0170 NKDA
           05  VP-VNAME                    PIC X(25).
           05  VP-VNAME-AENDKZ             PIC X(01).
090512     05  VP-NACHNAME                 PIC X(40).
090512     05  VP-NACHNAME-AENDKZ          PIC X(01).
           05  VP-GEBURTSDATUM             PIC 9(08).
           05  VP-GEBURTSDATUM-AENDKZ      PIC X(01).
           05  VP-BEGINN-DATUM             PIC 9(08).
           05  VP-BEGINN-DATUM-AENDKZ      PIC X(01).
           05  VP-ABLAUF-DATUM             PIC 9(08).
           05  VP-ABLAUF-DATUM-AENDKZ      PIC X(01).
100912     05  VP-ANREDE                   PIC X(02).
           05  VP-ANREDE-AENDKZ            PIC X(01).
           05  VP-STRASSE                  PIC X(40).
           05  VP-STRASSE-AENDKZ           PIC X(01).
           05  VP-LANDKNZ                  PIC X(03).
           05  VP-LANDKNZ-AENDKZ           PIC X(01).
           05  VP-PLZ                      PIC X(06).
           05  VP-PLZ-AENDKZ               PIC X(01).
           05  VP-ORT                      PIC X(40).
           05  VP-ORT-AENDKZ               PIC X(01).
           05  VP-GESCHL                   PIC X(01).
           05  VP-GESCHL-AENDKZ            PIC X(01).
250816     05  VP-VORVERSSCHL              PIC 9(03).
250816     05  VP-VORVERSSCHL-AENDKZ       PIC X(01).
250816     05  VP-VORVERSDATUM             PIC 9(08).
250816     05  VP-VORVERSDATUM-AENDKZ      PIC X(01).
250816     05  VP-VORVERSKUENDKZ           PIC X(01).
250816     05  VP-VORVERSKUENDKZ-AENDKZ    PIC X(01).
250816     05  VP-RESERVE                  PIC X(21).
100912*    05  VP-RESERVE                  PIC X(13).
      ******************************************************************
      *               E N D E  Copy-Buch   DRKI2577                    *
      ******************************************************************

