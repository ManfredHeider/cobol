      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2576.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2576.CPv  $
      *
      *   Rev 2.1   Jun 26 2012 17:03:32   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 13 2012 19:24:28   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 5 7 6   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Umstellung der CKV-Verarbeitung woechentlich Komplettbestand  *
      *  auf die CKV-Verarbeitung taeglich Deltabestand.               *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: PERSONENDATEN FUER ALLE GESELLSCHAFTEN       *
      *                                                                *
      *    SATZLÄNGE   450                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !     AENDERUNG                         !NAME       !VER*
      *AUFTRAG !                                       !           !   *
      *--------!---------------------------------------!-----------!---*
      *24.04.12!NEUERSTELLUNG (Kopie von DRKI0170 aus  !Schlenther !01 *
      *AKI01100!Produktion vor Umstellung NKDA)        !           !   *
      *--------!---------------------------------------!-----------!---*
      *        !                                       !           !   *
      ******************************************************************
       01  DRKI2576.
           05  PERS-AKTUAL-KZ                PIC  X(01).
      *        I = INSERT
      *        U = UPDATE
      *        V = Versicherungsnehmerwechsel
      **
           05  PERS-GESELLSCHAFT             PIC  9(02).
      *    05  PERS-GESELLSCHAFT-AENDKZ      PIC  X(01).
      ** Wird im ErmittlungsPGM mit dem Wert '50' (CENTRAL) gefuellt.
           05  PERS-VTRGNR                   PIC  9(15).
           05  PERS-PERSONENNR               PIC  9(04).

           05  PERS-ROLLE-VERTRAG            PIC  X(01).
           05  PERS-ROLLE-TARIF              PIC  X(01).

           05  PERS-NNAME                    PIC  X(40).
           05  PERS-NNAME-AENDKZ             PIC  X(01).
      *        J WENN NACHNAMEN GEAENDERT

           05  PERS-VNAME                    PIC  X(20).
           05  PERS-VNAME-AENDKZ             PIC  X(01).
      *        J WENN VORNAMEN GEAENDERT

           05  PERS-GEBDAT                   PIC  9(08).
           05  PERS-GEBDAT-AENDKZ            PIC  X(01).
      *        J WENN GEBURTSDATUM GEAENDERT

           05  PERS-ANREDE                   PIC  X(02).
           05  PERS-ANREDE-AENDKZ            PIC  X(01).
      *        J WENN ANREDE GEAENDERT

           05  PERS-STR                      PIC  X(40).
           05  PERS-STR-AENDKZ               PIC  X(01).
      *        J WENN STRASSE GEAENDERT

           05  PERS-PLZ                      PIC  X(09).
           05  PERS-PLZ-AENDKZ               PIC  X(01).
      *        J WENN PLZ GEAENDERT

           05  PERS-ORT                      PIC  X(40).
           05  PERS-ORT-AENDKZ               PIC  X(01).
      *        J WENN ORT GEAENDERT

           05  PERS-LAND-KZ                  PIC  X(03).
           05  PERS-LAND-KZ-AENDKZ           PIC  X(01).
      *        J WENN AUSLAND-KENNZEICHEN GEAENDERT

           05  PERS-NNAME-ALT                PIC  X(40).
           05  PERS-VNAME-ALT                PIC  X(20).
      *        Namensfelder-alt

           05  PERS-FILLER                   PIC  X(196).
      *
      ******************************************************************
