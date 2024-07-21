      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0013.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKW0013.CPv  $
      *
      *   Rev 2.2   May 21 2013 16:00:36   t13ex14
      *AKI01230: DVAG Wien: neues Feld Zession im Leben-DTA
      *
      *   Rev 2.1   Nov 11 2010 11:39:54   t13ex14
      *AKI00966: DVAG Österreich: Leben-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 12 2010 14:51:16   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aufbereitungsbestand DVAG Wien Lebendaten               *
      *                                                               *
      *    SATZLÄNGE   720                                            *
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
       01  DRKW0013.

           05  AUFB-LEB-VERTRAGSDATEN.

               10 AUFB-LEB-VERARB-KZ                   PIC  X.
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update  D = Delete

               10 AUFB-LEB-KEY.
                  15 AUFB-LEB-GESELLSCHAFT             PIC  9(002).
      *              Produktgeber
                  15 AUFB-LEB-VERTRAGSNUMMER           PIC  9(015).
      *
               10 AUFB-LEB-PRODUKT                     PIC  9(002).
               10 AUFB-LEB-PRODUKT-AEND-KZ             PIC  X(001).
      *
               10 AUFB-LEB-TARIF                       PIC  X(015).
               10 AUFB-LEB-TARIF-AEND-KZ               PIC  X(001).
      *
               10 AUFB-LEB-PRZAHLENDE                  PIC  9(008).
      *           Ende der Prämienzahlung (JJJJMMTT)
               10 AUFB-LEB-PRZAHLENDE-AEND-KZ          PIC  X(001).

               10 AUFB-LEB-VERS-SUMME                  PIC  9(011)V99.
      *           Versicherungssumme
               10 AUFB-LEB-VERS-SUMME-AEND-KZ          PIC  X(001).

               10 AUFB-LEB-MIND-RISIKO-SUMME           PIC  9(011)V99.
      *           Mindestrisikosumme (Todesfallsumme)
               10 AUFB-LEB-MIND-RISK-SUM-AEND-KZ       PIC  X(001).

               10 AUFB-LEB-BEI-ZUST                    PIC  9(002).
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
               10 AUFB-LEB-BEI-ZUST-AEND-KZ            PIC  X(001).

               10 AUFB-LEB-DIREKT-VERS-KZ              PIC  X(002).
      *           BETRIEBSVERSICHERUNG
      *           00 = keine Direktversicherung
      *           09 = GEHALTSUMWANDLUNGS-DIREKTVERSICHERUNG
      *           69 = ARBEITGEBER FINANZIERTE DIREKTVERSICHERUNG
               10 AUFB-LEB-DIR-VERS-KZ-AEND-KZ         PIC  X(001).

               10 AUFB-LEB-ZAHLWEISE-RENTE             PIC  9.
      *           Rentenzahlweise bzw. Zahlweise der Ablaufleistung
      *           0 = UNBEKANNTE ZAHLWEISE
      *           1 = JÄHRLICHE ZAHLWEISE
      *           2 = HALBJÄHRLICHE ZAHLWEISE
      *           4 = VIERTELJÄHRLICHE ZAHLWEISE
      *           6 = MONATLICHE ZAHLWEISE
      *           8 = EINMALZAHLUNG
               10 AUFB-LEB-ZAHLWEI-RENTE-AEND-KZ       PIC  X(001).

               10 AUFB-LEB-ABLAUFRENTE                 PIC  9(011)V99.
               10 AUFB-LEB-ABLAUFRENTE-AEND-KZ         PIC  X(001).
      *
               10 AUFB-LEB-GARANTIEZEIT                PIC  9(008).
               10 AUFB-LEB-GARANTIEZEIT-AEND-KZ        PIC  X(001).
      *
               10 AUFB-LEB-GESAMTBEITRAG               PIC  9(011)V99.
      *           Jahresbruttoprämie
               10 AUFB-LEB-GESAMTBEITRAG-AEND-KZ       PIC  X(001).

               10 AUFB-LEB-RATENBEITRAG                PIC  9(011)V99.
      *           Bruttoprämie lt. Zahlweise
               10 AUFB-LEB-RATENBEITRAG-AEND-KZ        PIC  X(001).

               10 AUFB-LEB-ABLAUF                      PIC  9(011)V99.
      *           Ablaufleistung
               10 AUFB-LEB-ABLAUF-AEND-KZ              PIC  X(001).

               10 AUFB-LEB-DEPBETR                     PIC  9(011)V99.
      *           Fondswert
               10 AUFB-LEB-DEPBETR-AEND-KZ             PIC  X(001).

               10 AUFB-LEB-BUZRENTELEIST               PIC  9(011)V99.
      *           BUZ-Rentenleistung
               10 AUFB-LEB-BUZRENTELEIST-AEND-KZ       PIC  X(001).

               10 AUFB-LEB-GARANTERLLEIST              PIC  9(011)V99.
      *           Garantierte Erlebensleistung
               10 AUFB-LEB-GARANTERLLEIS-AEND-KZ       PIC  X(001).

               10 AUFB-LEB-TAZTERM     OCCURS 9 TIMES  PIC  9(008).
      *           Teilauszahlungstermin 1 - 9
               10 AUFB-LEB-TAZTERM-AEND-KZ             PIC  X(001).

               10 AUFB-LEB-TAZRENTE    OCCURS 9 TIMES  PIC  9(011)V99.
      *           Garantierte einmalige Teilauszahlung bzw.
      *           Garantierte jährliche Teilpension  1 bis 9
               10 AUFB-LEB-TAZRENTE-AEND-KZ            PIC  X(001).

               10 AUFB-LEB-TAZKAPITAL  OCCURS 9 TIMES  PIC  9(011)V99.
               10 AUFB-LEB-TAZKAPITAL-AEND-KZ          PIC  X(001).
      *
               10 AUFB-LEB-DYNAMIK                     PIC  9(002).
               10 AUFB-LEB-DYNAMIK-AEND-KZ             PIC  X(001).
      *
               10 AUFB-LEB-RUECKKAUFWERT              PIC  9(011)V99.
               10 AUFB-LEB-RUECKKAUFWERT-AEND-KZ      PIC  X(001).
      *
               10 AUFB-LEB-NETTOGESBEITRAG            PIC 9(011)V99.
      *           Jahresnettoprämie
               10 AUFB-LEB-NETTOGESBEITR-AEND-KZ      PIC  X(001).

130513         10 AUFB-LEB-ABTRETUNGKZ                PIC  X(001).
130513*           DVAG Wien-Terminus ZESSION J/N
130513         10 AUFB-LEB-ABTRETUNGKZ-AEND-KZ        PIC  X(001).
130513*        10 FILLER                              PIC  X(191).
130513         10 FILLER                              PIC  X(189).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0013                       *
      ******************************************************************
