      ****************************************************************
      *    COPY-MEMBER: DRAT0098                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE UPAT0012  PRÜFEN UND AUFBEREITEN DATENTYP   *
      *                            BETRAG S9(9)V99                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: SCHÖLLHAMMER                                    *
      *    DATUM:    15.11.1995                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0226!15.11.95! 01 ! NEU                       ! SCHÖLLH. *
130722* VLA-5045!13.07.22!2.2 ! Num. gepacktes Feld       ! HEIM      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    FUNKTION DES UPAT0012:
      *    - UPAT0012-FKT-PRF-FORMAL-AUFBER
      *      - PRÜFEN BETRAG AUF FORMALE KORREKTHEIT
      *        - IGNORIEREN FÜHRENDER BLANKS
      *        - ALLE ZEICHEN BIS ZUM NÄCHSTEN BLANK ZÄHLEN, WOBEI EIN
      *          ODER MEHRERE BLANKS ZWISCHEN EINEM MINUS-ZEICHEN UND
      *          EINER ZIFFER NICHT ZÄHLEN; ZEICHEN HINTER DEM
      *          'NÄCHSTEN BLANK' WERDEN IGNORIERT
      *        - EIN MINUS-ZEICHEN KANN NUR VOR DER ZAHL EINGEGEBEN
      *          WERDEN; BLANKS ZWISCHEN MINUS-ZEICHEN UND ZIFFERN SIND
      *          ERLAUBT
      *        - EIN BETRAG KANN OHNE KOMMA EINGEGEBEN WERDEN. ES WERDEN
      *          IMMER DIE ALS PARAMETER ANGEFORDERTEN NACHKOMMASTELLEN
      *          ZURÜCKGELIEFERT.
      *        - DIE ANZAHL DER VORKOMMASTELLEN WIRD ÜBER EINEN PARA-
      *          METER BESTIMMT. MEHR STELLEN IN DER EINGABE WERDEN
      *          ALS FEHLER ABGEWIESEN. ES WERDEN NUR DIE TATSÄCHLICH
      *          GEFÜLLTEN VORKOMMASTELLEN ZURÜCKGELIEFERT.
      *        - EINE ZAHL OHNE VORKOMMASTELLEN WIRD AKZEPTIERT (,99)
      *        - BEI EINEM LEEREN FELD WIRD DER BETRAG '0' ZURÜCKGEGEBEN
      *        - DIE NUMERIC-PRÜFUNG BETRIFFT NICHT DAS MINUS-ZEICHEN
      *        - DIE LÄNGE AUSGABE-STRINGS ERRECHNET SICH WIE FOLGT:
      *          1 BYTE VORZEICHEN + N BYTES VORKOMMASTELLEN +
      *          1 BYTE KOMMA + N BYTES NACHKOMMASTELLEN. WURDEN KEINE
      *          NACHKOMMASTELLEN ANGEFORDERT, ENFÄLLT DAS KOMMA.
      *
      *      - KONVERTIEREN DER EINGABE IN
      *          - NUMERISCHE FORM
      *          - NORMALISIERTE ALPHANUMERISCHE FORM
      *
      *    EINGABE:
      *      IMMER:
      *        - PI-UPAT0012-FKT
      *        - PI-UPAT0012-BETRAG-X
      *        - PI-UPAT0012-VORKOMMA-ST
      *        - PI-UPAT0012-NACHKOMMA-ST
      *
      *    AUSGABE:
      *      IMMER:
      *        - PO-UPAT0012-RC
      *      FALLS BETRAG KORREKT (RC-OK):
      *        - PO-UPAT0012-BETRAG-NUM
      *        - PO-UPAT0012-BETRAG-X
      *
      *
       01  PI-UPAT0012.
           05  PI-UPAT0012-FKT              PIC 9(002).
               88  UPAT0012-FKT-PRF-FORMAL-AUFBER          VALUE 1.
           05  PI-UPAT0012-BETRAG-X         PIC X(016).
      *        * MAX. 12 VORKOMMASTELLEN
           05  PI-UPAT0012-VORKOMMA-ST      PIC 9(002).
      *        * MAX. 2 NACHKOMMASTELLEN
           05  PI-UPAT0012-NACHKOMMA-ST     PIC 9(001).
           05  FILLER                       PIC X(027).
      *
       01  PO-UPAT0012.
           05  PO-UPAT0012-RC               PIC 9(002).
               88  PO-AT12-RC-OK                           VALUE 01.
               88  PO-AT12-RC-NICHT-NUM                    VALUE 02.
      *            * ZUVIELE STELLEN IN DER EINGABE
               88  PO-AT12-RC-ZUV-VK-EING                  VALUE 03.
               88  PO-AT12-RC-ZUV-NK-EING                  VALUE 04.
      *            * ZUVIELE STELLEN ALS PARAMETER ANGEFORDERT
      *            * MAX. 12 VORKOMMASTELLEN UND 2 NACHKOMMASTELLEN
               88  PO-AT12-RC-ERR-VK-PARM                  VALUE 05.
               88  PO-AT12-RC-ERR-NK-PARM                  VALUE 06.

130722     05  PO-UPAT0012-BETRAG-NUM       PIC S9(13)V99 COMP-3.
           05  PO-UPAT0012-BETRAG-X         PIC X(016).
           05  FILLER                       PIC X(022).
      *
      ******** ENDE COPY-MEMBER (DRAT0098) *******************

