      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2575.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2575.CPv  $
      *
      *   Rev 2.3   Feb 25 2019 14:55:34   t13ac13
      *AKI01663: G50 - Wechsel von Maklermandat auf Haustarif
      *
      *   Rev 2.2   Sep 20 2012 11:06:06   t13ae31
      *AKI01166: CKV Daten zu mitversicherten Personen
      *
      *   Rev 2.1   Jun 26 2012 17:03:24   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 13 2012 19:24:20   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *            ########    D R K I 2 5 7 5   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Umstellung der CKV-Verarbeitung woechentlich Komplettbestand  *
      *  auf die CKV-Verarbeitung taeglich Deltabestand.               *
      *    AUFBEREITUNG DER VertragsDaten.                             *
      *        - AUSGABE: VertragsDaten fuer alle Gesellschaften.      *
      *                                                                *
      *    SATZLÄNGE   265                                             *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !     AENDERUNG                       !NAME       !VERS.*
      *AUFTRAG !                                     !           !     *
      *--------!-------------------------------------!-----------!-----*
      *24.04.12!NEUERSTELLUNG (Kopie von DRKI0605 aus!Schlenther ! 1.0 *
      *AKI01100!Produktion vor Umstellung NKDA)      !           !     *
      *--------!-------------------------------------!-----------!-----*
100912*AKI01166! VZUST fuer WIK eingebaut            !Huebner    !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *        !                                     !           !     *
      *--------!-------------------------------------!-----------!-----*
      *20.02.19!Schalter für Makleränderung eingebaut!Boost      !     *
      *AKI01663!                                     !           !     *
      ******************************************************************
       01  DRKI2575.
      *
           05  VTRG-AKTUAL-KZ                      PIC X(01).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      **
           05  VTRG-KEY.
               10  VTRG-GESELLSCHAFT-NR            PIC 9(02).
      ** Wird im ErmittlungsPGM mit dem Wert '50' (CENTRAL) gefuellt.
               10  VTRG-VTRGNR                     PIC 9(15).
      *
           05  VTRG-VERTRAGSDATEN.
               10  VTRG-PART-INFO                  PIC X(15).
               10  VTRG-PART-INFO-AENDKZ           PIC X(01).
      *
               10  VTRG-ZAHLUNGSWEISE              PIC X(01).
               10  VTRG-ZAHLUNGSWEISE-AENDKZ       PIC X(01).
      * X=nimmt am LastSchriftVerfahren teil.
      * SPACE=nimmt nicht am LastSchriftVerfahren teil.
               10  VTRG-LSV-J-N                    PIC X(01).
               10  VTRG-LSV-J-N-AENDKZ             PIC X(01).
      *
               10  VTRG-BEITRAG-GESAMT             PIC S9(11)V99.
               10  VTRG-BEITRAG-GESAMT-AENDKZ      PIC X(01).
               10  VTRG-BEITRAG-GESAMT-WHR         PIC X(03).
               10  VTRG-BEITRAG-GESAMT-WHR-AENDKZ  PIC X(01).
               10  VTRG-GESCH-STELLE-NR            PIC X(03).
               10  VTRG-GESCH-STELLE-NR-AENDKZ     PIC X(01).
      *
               10  VTRG-VBEGINN                    PIC 9(08).
               10  VTRG-VBEGINN-AENDKZ             PIC X(01).
               10  VTRG-VABLAUF                    PIC 9(08).
               10  VTRG-VABLAUF-AENDKZ             PIC X(01).
               10  VTRG-VAEND-DAT                  PIC 9(08).
      ** Aenderungs-/ErstellungsDatum des Vertragssatzes(Wird von der
      ** CKV geliefert) .
               10  VTRG-VAEND-DAT-AENDKZ           PIC X(01).
100912         10  VTRG-VZUST                      PIC 9(02).
100912         10  VTRG-VZUST-AENDKZ               PIC X(01).
               10  VTRG-MAKLER-AEND                PIC X(01).
100912*        10  VTRG-FILLER                     PIC X(175).
               10  VTRG-FILLER                     PIC X(174).
      **
      ******************************************************************
      *           E N D E  COPY-BUCH   DRKI2575  VERTRAG               *
      ******************************************************************

