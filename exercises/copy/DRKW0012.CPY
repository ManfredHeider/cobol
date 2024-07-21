      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0012.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKW0012.CPv  $
      *
      *   Rev 2.2   May 21 2013 15:58:52   t13ex14
      *AKI01230: DVAG Wien: neues Feld Zession im Leben-DTA
      *
      *   Rev 2.1   Nov 11 2010 11:39:46   t13ex14
      *AKI00966: DVAG Österreich: Leben-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 12 2010 14:51:06   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Komplettbestand KSDS DVAG Wien Lebendaten               *
      *                                                               *
      *    SATZLÄNGE   740                                            *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!12.08.10! 01 ! Neu                       ! Misgeld  *
130513* AKI01230!13.05.13!    ! neues Feld ABTRETUNGKZ /  ! WMI      *
      *         !        !    ! ZESSION                   !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0012.

           05  KOMPL-LEB-AEND                           PIC  9(002).

           05  KOMPL-LEB-VERTRAGSDATEN.

               10 KOMPL-LEB-SATZART                     PIC  X.
      *           'D'
               10 KOMPL-LEB-KEY.
                  15 KOMPL-LEB-GESELLSCHAFT             PIC  9(002).
      *              Produktgeber
                  15 KOMPL-LEB-VERTRAGSNUMMER           PIC  9(015).
      *
               10 KOMPL-LEB-PRODUKT                     PIC  9(002).
      *
               10 KOMPL-LEB-TARIF                       PIC  X(015).
      *
               10 KOMPL-LEB-PRZAHLENDE                  PIC  9(008).
      *           Ende der Prämienzahlung (JJJJMMTT)
               10 KOMPL-LEB-VERS-SUMME                  PIC  9(011)V99.
      *           Versicherungssumme
               10 KOMPL-LEB-MIND-RISIKO-SUMME           PIC  9(011)V99.
      *           Mindestrisikosumme (Todesfallsumme)
               10 KOMPL-LEB-BEI-ZUST                    PIC  9(002).
      *           Beitragszustand
      *           01 = BEITRAGSPFLICHTIG
      *           02 = BEITRAGSFREI
      *           03 = GESTUNDET
      *           04 = TEILGESTUNDET
      *           05 = beitragsfrei wg. Invalidität
      *           06 = beitragsfrei wg. Ablauf Beitragszahler
      *           07 = beitragsfrei wg. Todesfall
      *           08 = beitragsfrei wg. Kündigung
      *           09 = beitragsfrei wg. Einmalzahlung
      *           10 = beitragsfrei wg. Sonderzahlung
      *           11 = ZULAGENGESPEIST
      *           99 = UNBEKANNT
               10 KOMPL-LEB-DIREKT-VERS-KZ              PIC  X(002).
      *           BETRIEBSVERSICHERUNG
      *           00 = keine Direktversicherung
      *           09 = GEHALTSUMWANDLUNGS-DIREKTVERSICHERUNG
      *           69 = ARBEITGEBER FINANZIERTE DIREKTVERSICHERUNG
               10 KOMPL-LEB-ZAHLWEISE-RENTE             PIC  9.
      *           Rentenzahlweise bzw. Zahlweise der Ablaufleistung
      *           0 = UNBEKANNTE ZAHLWEISE
      *           1 = JÄHRLICHE ZAHLWEISE
      *           2 = HALBJÄHRLICHE ZAHLWEISE
      *           4 = VIERTELJÄHRLICHE ZAHLWEISE
      *           6 = MONATLICHE ZAHLWEISE
      *           8 = EINMALZAHLUNG
               10 KOMPL-LEB-ABLAUFRENTE                 PIC  9(011)V99.
      *
               10 KOMPL-LEB-GARANTIEZEIT                PIC  9(008).
      *
               10 KOMPL-LEB-GESAMTBEITRAG               PIC  9(011)V99.
      *           Jahresbruttoprämie
               10 KOMPL-LEB-RATENBEITRAG                PIC  9(011)V99.
      *           Bruttoprämie lt. Zahlwesie
               10 KOMPL-LEB-ABLAUF                      PIC  9(011)V99.
      *           Ablaufleistung
               10 KOMPL-LEB-DEPBETR                     PIC  9(011)V99.
      *           Fondswert
               10 KOMPL-LEB-BUZRENTELEIST               PIC  9(011)V99.
      *           BUZ-Rentenleistung
               10 KOMPL-LEB-GARANTERLLEIST              PIC  9(011)V99.
      *           Garantierte Erlebensleistung
               10 KOMPL-LEB-TAZTERM     OCCURS 9 TIMES  PIC  9(008).
      *           Teilauszahlungstermin 1 - 9
               10 KOMPL-LEB-TAZRENTE    OCCURS 9 TIMES  PIC  9(011)V99.
      *           Garantierte einmalige Teilauszahlung bzw.
      *           Garantierte jährliche Teilpension  1 bis 9
               10 KOMPL-LEB-TAZKAPITAL  OCCURS 9 TIMES  PIC  9(011)V99.
      *
               10 KOMPL-LEB-DYNAMIK                     PIC  9(002).
      *
               10 KOMPL-LEB-RUECKKAUFWERT               PIC  9(011)V99.
      *
               10 KOMPL-LEB-NETTOGESBEITRAG             PIC 9(011)V99.
      *           Jahresnettoprämie

130513         10 KOMPL-LEB-ABTRETUNGKZ                 PIC X(001).
130513*           DVAG Wien-Terminus ZESSION J/N
130513*        10 FILLER-1                              PIC  X(193).
130513         10 FILLER-1                              PIC  X(192).
130513
             05 KOMPL-LEB-TIMESTAMP                     PIC X(026).
      *         JJJJ-MM-TT-hh.mm.ss.abcdef
             05 FILLER-2                                PIC X(012).
      ******************************************************************
      *          E n d e    Copy-Member DRKW0012                       *
      ******************************************************************
