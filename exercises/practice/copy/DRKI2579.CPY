      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2579.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2579.CPv  $
      *
      *   Rev 2.1   Jun 26 2012 17:04:00   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 13 2012 19:24:52   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *            ########    D R K I 2 5 7 9   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Umstellung der CKV-Verarbeitung woechentlich Komplettbestand  *
      *  auf die CKV-Verarbeitung taeglich Deltabestand.               *
      *    AUFBEREITEN DER VersKrankenTarif  (TCKI0345)                *
      *     - Ausgabe CKV DB2-Tabelle VERSKRANKENTARIF                 *
      *                                                                *
      *    SATZLÄNGE   60                                              *
      *                                                                *
      *                                                                *
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !     AENDERUNG                       !NAME       !VERS.*
      *AUFTRAG !                                     !           !     *
      *--------!-------------------------------------!-----------!-----*
      *24.04.12!NEUERSTELLUNG (Kopie von DRKI0608 aus!Schlenther ! 1.0 *
      *AKI01100!Produktion vor Umstellung NKDA)      !           !     *
      *--------!-------------------------------------!-----------!-----*
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      ******************************************************************
         01 DRKI2579.
      *
           05 VKR-AKTUAL-KZ                 PIC X(01).
      *
      *        I = INSERT
      *        U = UPDATE
      **
           05 VKR-GESELLSCHAFT              PIC 9(02).
      ** Wird vom ErmittlungsPGM immer mit "50" (CENTRAL) gefuellt.
           05 VKR-VTRGNR                    PIC 9(15).
           05 VKR-PERSONENNR                PIC 9(04).
           05 VKR-GESCHAEFTSID              PIC X(03).
           05 VKR-FILLER                    PIC X(35).
      ******************************************************************
      *        E N D E  COPY-BUCH   DRKI2579  VERSKRANKENTARIF         *
      ******************************************************************
