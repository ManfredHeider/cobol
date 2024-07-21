      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                   ! NAME     *
      * ---------------------------------------------------------------*
      * AKI00556!08.02.06!    ! NEU                         !I. Frenzel*
      *         !        !    !                             !          *
210807* AKI00657!21.08.07!    ! Feldlängenänderung          !W. Misgeld*
      *         !        !    !                             !          *
021207* AKL00664!02.12.07!    ! VDZ-Nr. als Feld f. Mandant !W. Misgeld*
      *         !        !    !                             !          *
300508* AKI00722!30.05.08!    ! OSTA-Feldlängenerweiterung  !W. Misgeld*
      *         !        !    !                             !          *
100620* AKI01788!10.06.20!    ! MOBIL EINGEFÜGT             ! ATAR,    *
      *         !        !    !                             ! KAYA     *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER FÜR 'DER VERMÖGENSBERATER' - DTA FÜR BURDA      *
      *    ------------------------------------------------------      *
      *                                                                *
      *    DTA AN BURDA. MIT DIESEM DTA WERDEN DIE ANSCHRIFTEN UND     *
      *    ANZAHL DER 'DER VERMÖGENSBERATER' FESTGELEGT UND VON BURDA  *
      *    BEARBEITET UND VERSCHICKT.                                  *
      *                                                                *
      *                      DATEINAME  = OKxxxxxx                     *
      *                      DATEILAENGE= 743 Byte                     *
300508*                      DATEILAENGE= 801 Byte                     *
300613*                      DATEILAENGE= 831 Byte                     *
170620*                      DATEILAENGE= 857 BYTE                     *
      ******************************************************************

       01  DROK0025.
021207     05  DROK0025-VDZ-NR            PIC 9(008).
021207*    VDZ-Nr. 00014523 = DVAG
021207*    VDZ-Nr. 00076942 = Allfinanz
021207*    VDZ-Nr. 00076943 = FVD
           05  DROK0025-SATZART           PIC X(001).
      *    Hauptsatz

      *        SATZART KONSTANTE "V"     KUNDE
      *        SATZART KONSTANTE "H"     EIGENBESTAND
      *        SATZART KONSTANTE "W"     RECHNUNGSSATZ

      *    Vor- und Nachsatz

      *        SATZART KONSTANTE "A"
      *        SATZART KONSTANTE "Z"

300508*    05  DROK0025-DATEN              PIC X(774).
300508**    05  DROK0025-DATEN              PIC X(792).
170620     05  DROK0025-DATEN              PIC X(848).

           05  DROK0025-HAUPTSATZ        REDEFINES DROK0025-DATEN.
               10  DROK0025-K-PLZ          PIC X(005).
      *        PLZ DES EMPFÄNGERS
               10  DROK0025-K-ANREDE       PIC X(036).
      *        ANREDE
               10  DROK0025-K-NAME         PIC X(030).
      *        EMPFAENGERNAME
               10  DROK0025-K-NAME2        PIC X(030).
      *        EMPFAENGERNAME2
               10  DROK0025-K-STRASSE      PIC X(030).
      *        STRASSE ODER POSTFACH
               10  DROK0025-K-ORT          PIC X(024).
      *        WOHNORT
               10  DROK0025-K-AUSL         PIC X(003).
      *        AUSLANDSKENNZEICHEN
               10  DROK0025-V-ANREDE       PIC X(036).
      *        ANREDE VB
               10  DROK0025-V-NAME         PIC X(030).
      *        NAME1 VB
               10  DROK0025-V-NAME2        PIC X(030).
      *        NAME2 VB
300508*        10  DROK0025-V-STASSE       PIC X(030).
300508         10  DROK0025-V-STASSE       PIC X(040).
      *        STRASSE ODER POSTFACH VB
               10  DROK0025-V-PLZ          PIC X(005).
      *        POSTLEITZAHL VB
300508*        10  DROK0025-V-ORT          PIC X(024).
300508         10  DROK0025-V-ORT          PIC X(030).
      *        WOHNORT VB
               10  DROK0025-V-AUSL         PIC X(003).
      *        AUSLANDS-KZ VB
210807*        10  DROK0025-V-TEL          PIC X(014).
300508*        10  DROK0025-V-TEL          PIC X(025).
300508         10  DROK0025-V-TEL          PIC X(026).
100620         10  DROK0025-V-MOBIL        PIC X(026).
      *        TELEFONNUMMER VB
210807*        10  DROK0025-V-FAX          PIC X(014).
300508*        10  DROK0025-V-FAX          PIC X(025).
300508         10  DROK0025-V-FAX          PIC X(026).
      *        FAXNUMMER VB
               10  DROK0025-V-VBNR         PIC 9(007).
      *        VERMOEGENSBERATERNUMMER
               10  DROK0025-V-DIR          PIC 9(003).
      *        DIREKTION
               10  DROK0025-V-HAND-BESTAND PIC 9(006).
      *        ANZAHL EIGENBESTAND IN STUECKZAHL
               10  DROK0025-V-DIREKT-VERSAND PIC 9(006).
      *        ANZAHL ZEITSCHRIFTEN AN KUNDEN IN STUECKZAHL
300613         10  DROK0025-V-IBAN         PIC X(034).
      *        IBAN VB
300613         10  DROK0025-V-BIC          PIC X(011).
      *        BLZ VB
     ***        10  DROK0025-V-KTO          PIC X(018).
      *        KONTONUMMER VB
     ***        10  DROK0025-V-BLZ          PIC X(015).
      *        BLZ VB
210807*        10  DROK0025-V-BANK-BEZ     PIC X(030).
210807**        10  DROK0025-V-BANK-BEZ     PIC X(040).
300613         10  DROK0025-V-BANK-BEZ     PIC X(058).
      *        BANKBEZEICHNUNG VB
               10  DROK0025-V-BLZ-AUSL-KZ  PIC X(003).
      *        SITZ DER BANK (AUSLANDS-KZ)
               10  DROK0025-V-STUFE        PIC X(030).
      *        VERMOEGENSBERATERSTUFE TEXT 1
               10  DROK0025-V-STUFE-2      PIC X(030).
      *        VERMOEGENSBERATERSTUFE TEXT 2
               10  DROK0025-V-STUFE-3      PIC X(030).
      *        VERMOEGENSBERATERSTUFE TEXT 3
               10  DROK0025-V-KZ-VB-KUNDE  PIC X(001).
      *        KENNZEICHEN KUNDE/ VB GILT NUR FUER UNBETREUTE
      *        K = KUNDE
      *        V = VB
               10  DROK0025-V-KNR          PIC 9(009).
      *        KUNDENNUMMER
               10  DROK0025-V-KTO-INH      PIC X(030).
      *        KONTOINHABER
               10  DROK0025-KONTOINHABER.
                   15  FILLER-1                PIC 9(008) VALUE ZERO.
                   15  DROK0025-V-BESTELLERID  PIC 9(018).
      *        EINDEUTIGER SCHLÜSSEL DES BESTELLERS(VERMÖGENSBERATER)
               10  DROK0025-BESTELLER.
                   15  FILLER-2                PIC 9(008) VALUE ZERO.
                   15  DROK0025-V-KUNDENID     PIC 9(018).
      *        EINDEUTIGER SCHLÜSSEL DES KUNDEN
               10  DROK0025-V-EMAIL        PIC X(60).
      *        EMAIL-Adresse des VB
               10  DROK0025-V-URL          PIC X(60).
      *        HOMEPAGE-Adresse des VB (nur wenn veroeffentlicht)
               10  DROK0025-V-JAHR         PIC 9(004).
      *        BESTELL JAHR
               10  DROK0025-V-HEFTNUMMER   PIC 9(004).
      *        HEFTAUSGABE NUMMER

           05  DROK0025-VORSATZ          REDEFINES DROK0025-DATEN.

               10  DROK0025-A-ABSEND       PIC X(004).
      *        KONSTANTE 'DVAG'
               10  DROK0025-A-ERST         PIC X(010).
      *        ERSTELLUNGSDATUM IM FORMAT JJJJMMTT
               10  DROK0025-A-EMPF         PIC X(005).
      *        KONSTANTE 'BURDA'

           05  DROK0025-NACHSATZ        REDEFINES DROK0025-DATEN.

               10  DROK0025-Z-ANZAHL       PIC 9(009).
      *        ANZAHL SAETZE MIT VOR- UND NACHSATZ

      *------------------------ ENDE DROK0025 --------------------------
