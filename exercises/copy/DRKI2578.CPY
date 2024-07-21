      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2578.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2578.CPv  $
      *
      *   Rev 2.1   Jun 26 2012 17:03:50   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 13 2012 19:24:44   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 5 7 8   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Umstellung der CKV-Verarbeitung woechentlich Komplettbestand  *
      *  auf die CKV-Verarbeitung taeglich Deltabestand.               *
      *    AUFBEREITUNG DER Daten fuer die DB2/Tabelle                 *
      *                                    VersicherterTarif(TCKI0348) *
      *        - AUSGABE: VersicherterTarif fuer alle Gesellschaften   *
      *                                                                *
      *    SATZLAENGE   70                                             *
      *                                                                *
      *                                                                *
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME      !VERS.*
      *AUFTRAG !                                      !          !     *
      *--------!--------------------------------------!----------!-----*
      *24.04.12!NEUERSTELLUNG (Kopie von DRKI0605 aus !Schlenther! 1.0 *
      *AKI01100!AE-Umgebung bereits umgestellt fuer   !          !     *
      *        !die NKDA)                             !          !     *
      *--------!--------------------------------------!----------!-----*
      *        !                                      !          !     *
      *        !                                      !          !     *
      *        !                                      !          !     *
      *        !                                      !          !     *
      *        !                                      !          !     *
      ******************************************************************
         01 DRKI2578.
           05 VTAR-AKTUAL-KZ                PIC X(01).
      *
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      ** ALLGEMEINE DATEN
           05 VTAR-GESELLSCHAFT             PIC 9(02).
      ** Wird vom ErmittlungsPGM immer mit "50" (CENTRAL) gefuellt.
           05 VTAR-VTRGNR                   PIC 9(15).
           05 VTAR-PERSONENNR               PIC 9(04).
           05 VTAR-PERSONENNR-VN            PIC 9(04).

      ** VERSICHERTERTARIF
,
           05 VTAR-PRODUKTID                PIC 9(02).
      ** Voll- oder Zusatztarif (siehe Stelle ?? Nr. 7 oder 8)
           05 VTAR-PRODUKTID-AENDKZ         PIC X(01).
           05 VTAR-SUBTYPE                  PIC X(01).
           05 VTAR-FILLER                   PIC X(40).
      ******************************************************************
      *               E N D E  Copy-Buch  DRKI2578                     *
      ******************************************************************
