      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0170.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0170.CPv  $
      *
      *   Rev 2.2   Aug 18 2014 15:53:46   t13ex19
      *AKI01322: SEPA - Restarbeiten - PMKI1167
      *
      *   Rev 2.1   Sep 20 2012 10:47:22   t13ae49
      *AKI00886: Host - neues Design Kundenaktualisierung - Hauptpr
      *ogramm
      *
      *   Rev 2.0   Feb 27 1999 12:53:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 11:04:16   T13EX12
      *AKI00014 - Aufbereitete Kundendaten
      *
      *   Rev 1.0   29 Sep 1997 11:54:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 0    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: PERSONENDATEN FUER ALLE GESELLSCHAFTEN       *
      *                                                                *
      *    SATZLÄNGE   413                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *29.05.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *22.09.98!Feldtypen (gemäß Support-Tabellen) von !K-HMüller! 02  *
      *        !   - ANREDE-KZ                         !         !     *
      *        !   - KOMM-ART-1                        !         !     *
      *        !   - KOMM-ART-2                        !         !     *
      *        !   - KONTOART                          !         !     *
      *        !in S9(4) BINARY geändert.              !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.10.98!Alle Felder oben zurückgeändert in     !v.d.Goltz! 03  *
      *        !orig. Länge, mit Vorzeichen. Auch      !         !     *
      *        !   - KOMM-ART-1-ALT                    !         !     *
      *        !   - KOMM-ART-2-ALT                    !         !     *
      *        !mit Vorzeichen.                        !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.01.12!Redefine um eine Verschiebung der      ! Wehlitz ! 04  *
      *        !Stellenanzahl fuer Vor- und Nachname   !         !     *
      *        !zu erreichen: 40St NN -> 30St          !         !     *
      *        !              20St VN -> 30St          !         !     *
      *        !Nicht mehr genutzte Felder durch Filler!         !     *
      *        !ersetzt                                !         !     *
      *--------!---------------------------------------!---------!-----*
      *18.08.14!Nicht mehr genutzte Felder durch Filler! Stoldt  ! 05  *
      *        !ersetzt- Bankverbindung wg. SEPA       !         !     *
      *--------!---------------------------------------!---------!-----*
      *24.06.21!Änderung von zuvor war nur bis FA.     ! U130685 ! 06  *
      *        !Jetzt nach Prod.                       !         !     *
      *--------!---------------------------------------!---------!-----*
      ******************************************************************
       01  DRKI0170.
         02  P-ANFANG-GRP.
           05  P-GES                     PIC  9(002).
           05  P-VNR                     PIC  9(015).

           05  P-ROLLE-VERTRAG           PIC  X(001).
           05  P-ROLLE-TARIF             PIC  X(001).
           05  P-VTRBEZ-ARTKZ            PIC  X(001).

190112   02  P-NAME-GRP.
           05  P-NNAME                   PIC  X(040).
           05  P-AEND-NNAME-KZ           PIC  X(001).
      *        J WENN NACHNAMEN GEAENDERT

           05  P-VNAME                   PIC  X(020).
           05  P-AEND-VNAME-KZ           PIC  X(001).
      *        J WENN VORNAMEN GEAENDERT
190112   02  P-NAME-GRP-RD REDEFINES P-NAME-GRP.
190112     05  P-NNAME-RD                PIC  X(030).
190112     05  P-AEND-NNAME-KZ-RD        PIC  X(001).
190112*        J WENN NACHNAMEN GEAENDERT

190112     05  P-VNAME-RD                PIC  X(030).
190112     05  P-AEND-VNAME-KZ-RD        PIC  X(001).
190112*        J WENN VORNAMEN GEAENDERT

190112   02  P-MITT-GRP.
           05  P-GEBDAT                  PIC  X(010).
           05  P-AEND-GEBDAT-KZ          PIC  X(001).
      *        J WENN GEBURTSDATUM GEAENDERT

           05  P-ANREDE                  PIC S9(002).
           05  P-AEND-ANREDE-KZ          PIC  X(001).
      *        J WENN ANREDE GEAENDERT

           05  P-GESCHL                  PIC  X(001).
           05  P-AEND-GESCHL-KZ          PIC  X(001).
      *        J WENN GESCHLECHT GEAENDERT

           05  P-STR                     PIC  X(040).
           05  P-AEND-STR-KZ             PIC  X(001).
      *        J WENN STRASSE GEAENDERT

           05  P-PLZ                     PIC  X(009).
           05  P-AEND-PLZ-KZ             PIC  X(001).
      *        J WENN PLZ GEAENDERT

           05  P-ORT                     PIC  X(040).
           05  P-AEND-ORT-KZ             PIC  X(001).
      *        J WENN ORT GEAENDERT

           05  P-AUSL-KZ                 PIC  X(003).
           05  P-AEND-AUSL-KZ            PIC  X(001).
      *        J WENN AUSLAND-KENNZEICHEN GEAENDERT

190112* ****** Felder bis Filler entfernt und durch diesen ersetzt ****
190112*    05  P-BERUF                   PIC  X(006).
190112*    05  P-AEND-BERUF-KZ           PIC  X(001).
190112*        J WENN BERUFS-KENNZEICHEN GEAENDERT
190112     05  FILLER                    PIC  X(007).

190112* ****** Feld durch neues Feld mit neuer Funktion ersetzt    ****
190112*    05  P-KOMM-ART-1              PIC S9(002).
           05  P-CKV-PERS-LFDNR          PIC  9(002).
190112*        Fuer CKV wird in diesem Feld die PersonLFDNR uebergeben

190112* ****** Felder bis Filler entfernt und durch diesen ersetzt ****
190112*    05  P-AEND-KOMM-ART-1-KZ      PIC  X(001).
      *        J WENN ERSTER KOMMUNIKATIONSART-KENNZEICHEN GEAENDERT
190112*    05  P-KOMM-1                  PIC  X(015).
190112*    05  P-AEND-KOMM-1-KZ          PIC  X(001).
190112*        J WENN ERSTE KOMMUNIKATION GEAENDERT
190112*    05  P-KOMM-ART-2              PIC S9(002).
190112     05  FILLER                    PIC  X(019).

190112* ****** Feld durch neues Feld mit neuer Funktion ersetzt    ****
190112*    05  P-AEND-KOMM-ART-2-KZ      PIC  X(001).
190112     05  P-EINGABE-30-KZ           PIC  X(001).
190112*        3 Wenn Vor- und Nachname 30 stellig sind

190112* ****** Felder bis Filler entfernt und durch diesen ersetzt ****
190112*    05  P-KOMM-2                  PIC  X(015).
190112*    05  P-AEND-KOMM-2-KZ          PIC  X(001).
      *        J WENN ZWEITE KOMMUNIKATION GEAENDERT
190112     05  FILLER                    PIC  X(016).

180814* ****** Felder für Bankverbindung entfernt und durch Filler ****
180814* ****** ersetzt
180814*    05  P-INSTITUT-NAME           PIC  X(050).
180814*    05  P-BLZ                     PIC  9(009).
180814*    05  P-KONTONR                 PIC  X(015).
180814*    05  P-KONTOART                PIC S9(001).
180814*    05  P-AEND-BANK-KZ            PIC  X(001).
      *        J WENN EIN BANKVERBINDUNGSFELD GEAENDERT
180814     05  FILLER                    PIC  X(076).

190112   02  P-NAME-ALT-GRP.
           05  P-NNAME-ALT               PIC  X(040).
           05  P-VNAME-ALT               PIC  X(020).
      *        Namensfelder-alt
190112   02  P-NAME-ALT-GRP-RD REDEFINES P-NAME-ALT-GRP.
190112     05  P-NNAME-ALT-RD            PIC  X(030).
190112     05  P-VNAME-ALT-RD            PIC  X(030).
190112*        Namensfelder-alt

190112   02  P-REST-GRP.
190112* ****** Felder bis Filler entfernt und durch diesen ersetzt ****
190112*    05  P-KOMM-1-ALT              PIC  X(015).
190112*    05  P-KOMM-ART-1-ALT          PIC S9(002).
190112*    05  P-KOMM-2-ALT              PIC  X(015).
190112*    05  P-KOMM-ART-2-ALT          PIC S9(002).
      *        Tel-, Fax-Nummer-alt
190112*    05  P-NAMESAEND-GES           PIC  X(001).
      *        J wenn Partnergesellschaft Namensaenderungs-KZ liefert
190112*    05  P-ADRESSAEND-GES          PIC  X(001).
      *        J wenn Partnergesellschaft Adressenderungs-KZ liefert
190112*    05  P-KUNDENAEND-GES          PIC  X(001).
      *        J wenn Partnergesellschaft KZ sonstige KD-Datenaenderung
      *        liefert
190112     05  FILLER                    PIC  X(037).
      *
           05  P-AKTUAL-KZ               PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *        V = Versicherungsnehmerwechsel
      ******************************************************************