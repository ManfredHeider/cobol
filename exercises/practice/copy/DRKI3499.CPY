      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  19.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3499.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3499.CPv  $
      *
      *   Rev 2.2   Oct 31 2018 16:51:04   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.1   Oct 30 2018 10:58:32   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.0   Sep 19 2018 10:14:48   t13ae52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR FSA / ABGELTUNSSTEUER VON GENERALI INVEST   *
      *    AUSGABE DER ÄNDERUNGEN AUS DER ERMITTLUNG                   *
      *                                                                *
      *    SATZLÄNGE   260                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!19.09.18! NEUERSTELLUNG                    !SCHUMACHER *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI3499.

           10  'XX'-AEND-SATZ.
      *
               20  'XX'-AEND-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  'XX'-AEND-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '42'
               20  'XX'-AEND-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER

               20 'XX'-AEND-VERARB-KZ            PIC X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *            I = INSERT
      *            U = UPDATE
      *
 
               20  'XX'-AEND-GUELTIG-AB-DAT      PIC  9(008).
      *            FSA GÜLTIG AB
               20  'XX'-AEND-GUELTIG-AB-DAT-KZ   PIC  X(001).
      *            J = ÄNDERUNG FSA GÜLTIG AB
 
               20  'XX'-AEND-GUELTIG-BIS-DAT     PIC  9(008).
      *            FSA GÜLTIG BIS
               20  'XX'-AEND-GUELTIG-BIS-DAT-KZ  PIC  X(001).
      *            J = ÄNDERUNG GÜLTIG BIS
 
               20  'XX'-AEND-FSA-GEMELDET        PIC  9(005)V99.
      *            FSA GEMELDET
               20  'XX'-AEND-FSA-GEMELDET-KZ     PIC  X(001).
      *            J = ÄNDERUNG FSA GEMELDET
      *
               20  'XX'-AEND-FSA-VERBRAUCHT      PIC  9(005)V99.
      *            FSA VERBRAUCHT
               20  'XX'-AEND-FSA-VERBRAUCHT-KZ   PIC  X(001).
      *            J = ÄNDERUNG FSA VERBRAUCHT
 
               20  'XX'-AEND-STUECKZINSEN-LFD-J  PIC  S9(010)V99.
      *            STÜCKZINSEN LFD. JAHR
               20  'XX'-AEND-STUECKZINSEN-LFD-J-KZ
                                                 PIC  X(001).
      *            J = ÄNDERUNG STÜCKZINSEN LFD. JAHR
 
               20  'XX'-AEND-STEUERAUSL          PIC  X(001).
      *             STEUERAUSLÄNDERKENNZEICHEN DER KUNDENSTAMMNUMMER
      *             J/N
               20  'XX'-AEND-STEUERAUSL-KZ       PIC  X(001).
      *             J = ÄNDERUNG STEUERAUSLÄNDERKENNZEICHEN
 
               20  'XX'-AEND-NIVERANL-BIS-DAT    PIC  9(008).
      *             NICHTVERANLAGUNGSBESCHEINIGUNG - GÜLTIG BIS
      *             JHJJMMTT
               20  'XX'-AEND-NIVERANL-BIS-DAT-KZ PIC  X(001).
      *            J = ÄNDERUNG NICHTVERANLAGUNGSBESCHEINIGUNG
 
               20  'XX'-AEND-AUSG-FSA-IM-VJ      PIC  9(005)V99.
      *             AUSGENUTZTER FREISTELLUNGSBETRAG IM VORJAHR IN EUR
               20  'XX'-AEND-AUSG-FSA-IM-VJ-KZ   PIC  X(001).
      *             J = ÄNDERUNG AUSGENUTZTER FREISTELLUNGSBETRAG
 
               20  'XX'-AEND-AUFGEL-ERTRAG       PIC  9(007)V99.
      *             AUFGELAUFENER ERTRAG GESAMT AKTUELLES JAHR
      *             (KUMULIERT)
               20  'XX'-AEND-AUFGEL-ERTRAG-KZ    PIC  X(001).
      *             J = ÄNDERUNG AUFGELAUFENER ERTRAG
 
               20  'XX'-AEND-EINBEH-KEST         PIC  9(005)V99.
      *             EINBEHALTENE KEST AKTUELLES JAHR (KUMULIERT)
               20  'XX'-AEND-EINBEH-KEST-KZ      PIC  X(001).
      *             J = ÄNDERUNG EINBEHALTENE KEST
 
               20  'XX'-AEND-EINBEH-SOLI         PIC  9(005)V99.
      *             EINBEHALTENER SOLI AKTUELLES JAHR (KUMULIERT)
               20  'XX'-AEND-EINBEH-SOLI-KZ      PIC  X(001).
      *             J = ÄNDERUNG EINBEHALTENER SOLI
 
               20  'XX'-AEND-EINBEH-KIST         PIC  9(005)V99.
      *             EINBEHALTENE KIRCHENSTEUER AKTUELLES JAHR
      *             (KUMULIERT)
               20  'XX'-AEND-EINBEH-KIST-KZ      PIC  X(001).
      *             J = ÄNDERUNG EINBEHALTENE KIRCHENSTEUER
 
               20  'XX'-AEND-VERLUST-AKTIEN      PIC  9(007)V99.
      *             VERLUSTTOPF AKTIEN AKTUELLER BALANCE-WERT
               20  'XX'-AEND-VERLUST-AKTIEN-KZ   PIC  X(001).
      *             J = ÄNDERUNG VERLUSTTOPF AKTIEN
 
               20  'XX'-AEND-VERLUST-SONSTIGE    PIC  9(007)V99.
      *             VERLUSTTOPF SONSTIGE AKTUELLER BALANCE-WERT
               20  'XX'-AEND-VERLUST-SONSTIGE-KZ PIC  X(001).
      *             J = ÄNDERUNG VERLUSTTOPF SONSTIGE
 
               20  'XX'-AEND-ANRECHEN-QUELLENST  PIC  9(007)V99.
      *             TOPF ANRECHENBARE QUELLEN-STEUER AKTUELLER
      *             BALANCE-WERT
               20  'XX'-AEND-ANRECHEN-QUELLENST-KZ
                                                 PIC  X(001).
      *             J = ÄNDERUNG TOPF ANRECHENBARE QUELLEN-STEUER
 
               20  'XX'-AEND-VERL-BESCH-AKTIEN   PIC  X(001).
      *             VERLUSTBESCHEINIGUNG ZUM AKTIENTOPF FÜR
      *             AKTUELLES JAHR ANGEFORDERT J/N
               20  'XX'-AEND-VERL-BESCH-AKTIEN-KZ
                                                 PIC  X(001).
      *             J = ÄNDERUNG VERLUSTBESCHEINIGUNG ZUM AKTIENTOPF
 
               20  'XX'-AEND-VERL-BESCH-SONSTIGE PIC  X(001).
      *             VERLUSTBESCHEINIGUNG ZUM SONSTIGE FÜR
      *             AKTUELLES JAHR ANGEFORDERT J/N
               20  'XX'-AEND-VERL-BESCH-SONST-KZ PIC  X(001).
      *             J = ÄNDERUNG VERLUSTBESCHEINIGUNG ZUM SONSTIGE
 
               20  'XX'-AEND-ZUG-KONFESSION-1    PIC  X(002).
      *             ZUGEHÖRIGKEIT KONFESSION 1
               20  'XX'-AEND-ZUG-KONFESSION-1-KZ PIC  X(001).
      *             J = ÄNDERUNG ZUGEHÖRIGKEIT KONFESSION 1
 
               20  'XX'-AEND-ZUG-KONFESSION-2    PIC  X(002).
      *             ZUGEHÖRIGKEIT KONFESSION 2
               20  'XX'-AEND-ZUG-KONFESSION-2-KZ PIC  X(001).
      *             J = ÄNDERUNG ZUGEHÖRIGKEIT KONFESSION 2
 
               20  'XX'-AEND-KIST-SATZ1          PIC  9(002)V99.
      *             KIRCHENSTEUER-SATZ 1
               20  'XX'-AEND-KIST-SATZ1-KZ       PIC  X(001).
      *             J = ÄNDERUNG KIRCHENSTEUER-SATZ 1
 
               20  'XX'-AEND-KIST-SATZ2          PIC  9(002)V99.
      *             KIRCHENSTEUER-SATZ 2
               20  'XX'-AEND-KIST-SATZ2-KZ       PIC  X(001).
      *             J = ÄNDERUNG KIRCHENSTEUER-SATZ 2
 
               20  'XX'-AEND-EINB-KIST-AB        PIC  9(008).
      *             EINBEHALT DER KIRCHENSTEUER AB JHJJMMTT
               20  'XX'-AEND-EINB-KIST-AB-KZ     PIC  X(001).
      *             J = ÄNDERUNG EINBEHALT DER KIRCHENSTEUER
 
               20  'XX'-AEND-AUFT-MASSST-KONF1   PIC  9(003).
      *             AUFTEILUNGSMASSSTAB
      *             KONFESSIONSZUGEHÖRIGKEIT 1 IN %
               20  'XX'-AEND-AUFT-MASSST-KONF1-KZ
                                                 PIC  X(001).
      *             J = ÄNDERUNG AUFTEILUNGSMASSSTAB
      *                 KONFESSIONSZUGEHÖRIGKEIT 1
               20  'XX'-AEND-AUFT-MASSST-KONF2   PIC  9(003).
      *             AUFTEILUNGSMASSSTAB
      *             KONFESSIONSZUGEHÖRIGKEIT 2 IN %
               20  'XX'-AEND-AUFT-MASSST-KONF2-KZ
                                                 PIC  X(001).
      *             J = ÄNDERUNG AUFTEILUNGSMASSSTAB
      *                 KONFESSIONSZUGEHÖRIGKEIT 2
 
               20  'XX'-AEND-FSA-ID              PIC  X(008).
               20  'XX'-AEND-FSA-ID-KZ           PIC  X(001).
      *             J = ÄNDERUNG FSA-ID
 
               20  FILLER                        PIC  X(065).

