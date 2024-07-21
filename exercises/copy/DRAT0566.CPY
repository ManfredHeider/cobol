      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0566.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0566.CPv  $
      *
      *   Rev 2.3   Feb 24 2015 14:05:08   t13ac20
      *AMA00193: Rückstellungsabrechnung: negatives Haftungsvolumen
      *: Meldungsverwaltung
      *
      *   Rev 2.2   Dec 05 2014 15:00:28   t13ac20
      *AMA00123: Auszahlung an geschlossenen ED-Konten bei tätigem
      *D-Leiter: Meldungsverwaltung
      *
      *   Rev 2.1   Jun 28 2013 15:09:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00123!24.11.14!2.2  ! 01110152 EINGEFÜGT        ! WOLFF
      * AMA00193!24.02.15!2.3  ! 01100146 EINGEFÜGT        ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      *---------------------------------------------------------------*
 
       01  TABELLE-ZENTR-FEHL-MELD.
           05 TABZFM-WERTE.
              10 .
                 15                    pic 9(8)  value 01100000.
                 15                    pic X(60) value
         '* ERSTE ALLGEMEINE TECHNISCHE MELDUNG (NICHT BENUTZEN!!!) *I'.
              10 .
                 15                    pic 9(8)  value 01100001.
                 15                    pic X(60) value
         'Datenbankfehler.                                           E'.
              10 .
                 15                    pic 9(8)  value 01100002.
                 15                    pic X(60) value
         'Systemfehler bei DB2-Zugriff.                              E'.
              10 .
                 15                    pic 9(8)  value 01100003.
                 15                    pic X(60) value
         'Systemüberlastung bei DB2-Zugriff.                         E'.
              10 .
                 15                    pic 9(8)  value 01100004.
                 15                    pic X(60) value
         'Programmfehler bei DB2-Zugriff # $.                        E'.
              10 .
                 15                    pic 9(8)  value 01100005.
                 15                    pic X(60) value
         'Benutzerkennung ungültig.                                  E'.
              10 .
                 15                    pic 9(8)  value 01100013.
                 15                    pic X(60) value
         'Auswahl-Funktion im Eingabeparameter ungültig.             E'.
              10 .
                 15                    pic 9(8)  value 01100014.
                 15                    pic X(60) value
         'Datum im Eingabeparameter ungültig.                        E'.
              10 .
                 15                    pic 9(8)  value 01100015.
                 15                    pic X(60) value
         'Anzahl im Eingabeparameter ungültig.                       E'.
              10 .
                 15                    pic 9(8)  value 01100017.
                 15                    pic X(60) value
         'Eingabeparameter ungültig.                                 E'.
              10 .
                 15                    pic 9(8)  value 01100019.
                 15                    pic X(60) value
         'Maximale Anzahl Ergebnisse überschritten.                  W'.
              10 .
                 15                    pic 9(8)  value 01100020.
                 15                    pic X(60) value
         'Ordnungsbegriff bereits vorhanden.                         E'.
              10 .
                 15                    pic 9(8)  value 01100021.
                 15                    pic X(60) value
         'Ordnungsbegriff nicht vorhanden.                           E'.
              10 .
                 15                    pic 9(8)  value 01100030.
                 15                    pic X(60) value
         'Worktabelle-Listdateneintrag nicht vorhanden.              E'.
              10 .
                 15                    pic 9(8)  value 01100031.
                 15                    pic X(60) value
         '# ungültig $.                                              E'.
              10 .
                 15                    pic 9(8)  value 01100045.
                 15                    pic X(60) value
         'Regel # nicht gefunden.                                    E'.
              10 .
                 15                    pic 9(8)  value 01100046.
                 15                    pic X(60) value
         'Regelparameter ungültig.                                   E'.
              10 .
                 15                    pic 9(8)  value 01100047.
                 15                    pic X(60) value
         'Benutzer hat keine Kompetenz.                              E'.
              10 .
                 15                    pic 9(8)  value 01100055.
                 15                    pic X(60) value
         'Start am # Uhr / Gener.-Datum $.                           I'.
              10 .
                 15                    pic 9(8)  value 01100056.
                 15                    pic X(60) value
         'Verarbeitungsende am # Uhr.                                I'.
              10 .
                 15                    pic 9(8)  value 01100057.
                 15                    pic X(60) value
         'PGM-Name der Vorlaufkarte falsch : #.                      E'.
              10 .
                 15                    pic 9(8)  value 01100058.
                 15                    pic X(60) value
         'Kein Datensatz auf # vorhanden.                            I'.
              10 .
                 15                    pic 9(8)  value 01100063.
                 15                    pic X(60) value
         'Tabellenüberlauf bei Tabelle #.                            E'.
              10 .
                 15                    pic 9(8)  value 01100064.
                 15                    pic X(60) value
         'Mehr als eine Ergebniszeile für #.                         E'.
              10 .
                 15                    pic 9(8)  value 01100065.
                 15                    pic X(60) value
         'Ungültiger Parameter in #.                                 E'.
              10 .
                 15                    pic 9(8)  value 01100066.
                 15                    pic X(60) value
         'Im Monat # ist kein $ . Arbeitstag vorhanden.              E'.
              10 .
                 15                    pic 9(8)  value 01100072.
                 15                    pic X(60) value
         'Der # ist $ Arbeitstag.                                    E'.
              10 .
                 15                    pic 9(8)  value 01100100.
                 15                    pic X(60) value
         'Ungültige Auswahl in IF- oder EVALUATE-Statement.          E'.
              10 .
                 15                    pic 9(8)  value 01100101.
                 15                    pic X(60) value
         '# Programmfehler im Programm: $.                           E'.
              10 .
                 15                    pic 9(8)  value 01100102.
                 15                    pic X(60) value
         'Work-Tabelle nicht gefunden.                               E'.
              10 .
                 15                    pic 9(8)  value 01100103.
                 15                    pic X(60) value
         'Fehler im Upro #.                                          E'.
              10 .
                 15                    pic 9(8)  value 01100104.
                 15                    pic X(60) value
         'Auswahlkennzeichen # für $ ungültig.                       E'.
              10 .
                 15                    pic 9(8)  value 01100105.
                 15                    pic X(60) value
         '# fehlt.                                                   E'.
              10 .
                 15                    pic 9(8)  value 01100106.
                 15                    pic X(60) value
         'Planname ungültig oder Berechtigung fehlt (SQLCODE -922).  E'.
              10 .
                 15                    pic 9(8)  value 01100107.
                 15                    pic X(60) value
         'Timestamp des Binds nicht aktuell (SQLCODE -818) #.        E'.
              10 .
                 15                    pic 9(8)  value 01100108.
                 15                    pic X(60) value
         'Mehr als # Ergebniszeilen für $.                           E'.
              10 .
                 15                    pic 9(8)  value 01100109.
                 15                    pic X(60) value
         'DBRM zu # fehlt im DB2-Plan.                               E'.
              10 .
                 15                    pic 9(8)  value 01100111.
                 15                    pic X(60) value
         'Fehler # beim Zugriff auf die $.                           E'.
              10 .
                 15                    pic 9(8)  value 01100112.
                 15                    pic X(60) value
         '# nicht verfügbar.                                         E'.
              10 .
                 15                    pic 9(8)  value 01100113.
                 15                    pic X(60) value
         'Keine Arbeitstage addiert (Datum ist ein Arbeitstag).      W'.
              10 .
                 15                    pic 9(8)  value 01100114.
                 15                    pic X(60) value
         'Keine Arbeitstage addiert (Datum ist kein Arbeitstag!).    W'.
              10 .
                 15                    pic 9(8)  value 01100115.
                 15                    pic X(60) value
         'Ungültiges Datumsformat beim DB2-Update in #.              E'.
              10 .
                 15                    pic 9(8)  value 01100116.
                 15                    pic X(60) value
         'Fehler beim Öffnen der Datei #.                            E'.
              10 .
                 15                    pic 9(8)  value 01100117.
                 15                    pic X(60) value
         'Fehler beim Schließen der Datei #.                         E'.
              10 .
                 15                    pic 9(8)  value 01100118.
                 15                    pic X(60) value
         'Fehler beim Verarbeiten der Datei #.                       E'.
              10 .
                 15                    pic 9(8)  value 01100119.
                 15                    pic X(60) value
         'Resource: # steht z. Zt. nicht zur Verfügung. $            E'.
              10 .
                 15                    pic 9(8)  value 01100128.
                 15                    pic X(60) value
         'Eingabeparameter sind unvollständig.                       E'.
              10 .
                 15                    pic 9(8)  value 01100129.
                 15                    pic X(60) value
         'Fehler beim Öffnen von # : $                               E'.
              10 .
                 15                    pic 9(8)  value 01100130.
                 15                    pic X(60) value
         'Fehler beim Schließen von # : $                            E'.
              10 .
                 15                    pic 9(8)  value 01100131.
                 15                    pic X(60) value
         'Fehler beim Verarbeiten von # : $                          E'.
              10 .
                 15                    pic 9(8)  value 01100146.
                 15                    pic X(60) value
         '# verarbeitet.                                             I'.
              10 .
                 15                    pic 9(8)  value 01109999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01110000.
                 15                    pic X(60) value
         '* ERSTE ALLGEMEINE FACHLICHE MELDUNG (NICHT BENUTZEN!!!)  *I'.
              10 .
                 15                    pic 9(8)  value 01110001.
                 15                    pic X(60) value
         'Datum ist ungültig                                         E'.
              10 .
                 15                    pic 9(8)  value 01110002.
                 15                    pic X(60) value
         'Datumvergleich unwahr                                      E'.
              10 .
                 15                    pic 9(8)  value 01110003.
                 15                    pic X(60) value
         'Datum ist kleiner/gleich Tagesdatum.                       E'.
              10 .
                 15                    pic 9(8)  value 01110004.
                 15                    pic X(60) value
         'Datum ist größer/gleich Tagesdatum.                        E'.
              10 .
                 15                    pic 9(8)  value 01110005.
                 15                    pic X(60) value
         'Datum ist ungleich Tagesdatum.                             E'.
              10 .
                 15                    pic 9(8)  value 01110006.
                 15                    pic X(60) value
         'Datum ist kleiner Tagesdatum.                              E'.
              10 .
                 15                    pic 9(8)  value 01110007.
                 15                    pic X(60) value
         'Datum ist größer Tagesdatum.                               E'.
              10 .
                 15                    pic 9(8)  value 01110008.
                 15                    pic X(60) value
         'Daten zur Zeit durch User # in Bearbeitung.                I'.
              10 .
                 15                    pic 9(8)  value 01110009.
                 15                    pic X(60) value
         'Keine Daten vorhanden.                                     I'.
              10 .
                 15                    pic 9(8)  value 01110010.
                 15                    pic X(60) value
         'Vorwärtsblättern nicht möglich.                            I'.
              10 .
                 15                    pic 9(8)  value 01110011.
                 15                    pic X(60) value
         'Rückwärtsblättern nicht möglich.                           I'.
              10 .
                 15                    pic 9(8)  value 01110024.
                 15                    pic X(60) value
         '# nicht vorhanden.                                         E'.
              10 .
                 15                    pic 9(8)  value 01110064.
                 15                    pic X(60) value
         'Datum befindet sich vor dem vorgegebenen Zeitraum.         E'.
              10 .
                 15                    pic 9(8)  value 01110066.
                 15                    pic X(60) value
         'Datum befindet sich nach dem vorgegebenen Zeitraum.        E'.
              10 .
                 15                    pic 9(8)  value 01110077.
                 15                    pic X(60) value
         '# $ nicht möglich.                                         E'.
              10 .
                 15                    pic 9(8)  value 01110110.
                 15                    pic X(60) value
         '# ist nicht zulässig.                                      E'.
              10 .
                 15                    pic 9(8)  value 01110112.
                 15                    pic X(60) value
         '# $ nicht vorhanden.                                       E'.
              10 .
                 15                    pic 9(8)  value 01110113.
                 15                    pic X(60) value
         'Format des Datums ungültig                                 E'.
              10 .
                 15                    pic 9(8)  value 01110123.
                 15                    pic X(60) value
         '# nicht ermittelbar.                                       E'.
              10 .
                 15                    pic 9(8)  value 01110145.
                 15                    pic X(60) value
         'Rechtsblättern nicht möglich.                              I'.
              10 .
                 15                    pic 9(8)  value 01110146.
                 15                    pic X(60) value
         'Linksblättern nicht möglich.                               I'.
              10 .
                 15                    pic 9(8)  value 01110152.
                 15                    pic X(60) value
         '# $ ungültig.                                              E'.
              10 .
                 15                    pic 9(8)  value 01119999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01200000.
                 15                    pic X(60) value
         '* ERSTE TECHNISCHE AMVS-MELDUNG (NICHT BENUTZEN!!!        *I'.
              10 .
                 15                    pic 9(8)  value 01201028.
                 15                    pic X(60) value
         'PT1028F: "#" ungültig                                      E'.
              10 .
                 15                    pic 9(8)  value 01209999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01210000.
                 15                    pic X(60) value
         '* ERSTE FACHLICHE AMVS-MELDUNG (NICHT BENUTZEN!!!)        *I'.
              10 .
                 15                    pic 9(8)  value 01210285.
                 15                    pic X(60) value
         '# wurde(n) geändert.                                       E'.
              10 .
                 15                    pic 9(8)  value 01369999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01360000.
                 15                    pic X(60) value
         '* ERSTE MELDUNG FÜR SYSTEMCODE OM/KL (NICHT BENUTZEN!!!)  *I'.
              10 .
                 15                    pic 9(8)  value 01360022.
                 15                    pic X(60) value
         'Ende der Gültigkeit vor Beginn der Gültigkeit unzulässig.  E'.
              10 .
                 15                    pic 9(8)  value 01419999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01410000.
                 15                    pic X(60) value
         '* ERSTE FACHLICHE VT-VWS-MELDUNG (NICHT BENUTZEN!!!)      *I'.
              10 .
                 15                    pic 9(8)  value 01410443.
                 15                    pic X(60) value
         'Das Datum ist kein Arbeitstag. Geben Sie ein gültiges ein. E'.
              10 .
                 15                    pic 9(8)  value 01410505.
                 15                    pic X(60) value
         '# $ ungültig.                                              E'.
              10 .
                 15                    pic 9(8)  value 01411022.
                 15                    pic X(60) value
         '# ungültig, nur $ möglich.                                 E'.
              10 .
                 15                    pic 9(8)  value 01411999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01455000.
                 15                    pic X(60) value
         '* ERSTE TECHNISCHE VVV-MELDUNG (NICHT BENUTZEN!!!)        *I'.
              10 .
                 15                    pic 9(8)  value 01455004.
                 15                    pic X(60) value
         'Collection-Bearbeitung fehlgeschlagen: Funktion=# und RC=$ I'.
              10 .
                 15                    pic 9(8)  value 01455999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01700000.
                 15                    pic X(60) value
         '* ERSTE TECHNISCHE MELDUNG MATHEMATIK (NICHT BENUTZEN!!!) *I'.
              10 .
                 15                    pic 9(8)  value 01700001.
                 15                    pic X(60) value
         'Fehler in der Vorlaufkarte: # $                            E'.
              10 .
                 15                    pic 9(8)  value 01700003.
                 15                    pic X(60) value
         'Falsche Eingabedatei: #                                    E'.
              10 .
                 15                    pic 9(8)  value 01709999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01800000.
                 15                    pic X(60) value
         '* ERSTE TECHNISCHE MELDUNG DVAG (NICHT BENUTZEN!!!)       *I'.
              10 .
                 15                    pic 9(8)  value 01800005.
                 15                    pic X(60) value
         'Buchungssatz zur VBNR # wurde für CPD-Konto erstellt       I'.
              10 .
                 15                    pic 9(8)  value 01800006.
                 15                    pic X(60) value
         'Rückbelastungsbetrag Erf.-Prov. fehlerhaft zur VBNR #      E'.
              10 .
                 15                    pic 9(8)  value 01800007.
                 15                    pic X(60) value
         '# nicht vorhanden: $                                       E'.
              10 .
                 15                    pic 9(8)  value 01800008.
                 15                    pic X(60) value
         'Werte stimmen mit letztem Lauf überein                     E'.
              10 .
                 15                    pic 9(8)  value 01800009.
                 15                    pic X(60) value
         'Analysedaten zu # / $ bereits vorhanden                    E'.
              10 .
                 15                    pic 9(8)  value 01809999.
                 15                    pic X(60) value
         '                                                           I'.
      ******************************************************************
              10 .
                 15                    pic 9(8)  value 01810000.
                 15                    pic X(60) value
         '* ERSTE FACHLICHE MELDUNG DVAG (NICHT BENUTZEN!!!)        *I'.
              10 .
                 15                    pic 9(8)  value 01810001.
                 15                    pic X(60) value
         'Da PE <= 2 , kein # vorhanden.                             I'.
              10 .
                 15                    pic 9(8)  value 01810002.
                 15                    pic X(60) value
         'Bei PE > 2 , kein # vorhanden.                             I'.
              10 .
                 15                    pic 9(8)  value 01810003.
                 15                    pic X(60) value
         'Deckblatt nicht zu allen Notizen: #                        I'.
              10 .
                 15                    pic 9(8)  value 01810004.
                 15                    pic X(60) value
         'Produkt gemäß Referenztabelle nicht rückstellungspflichtig.I'.
              10 .
                 15                    pic 9(8)  value 01810005.
                 15                    pic X(60) value
         'Bewegung # $ erkannt.                                      I'.
              10 .
                 15                    pic 9(8)  value 01810006.
                 15                    pic X(60) value
         'Vertrag nicht rückstellungspflichtig: # $                  I'.
              10 .
                 15                    pic 9(8)  value 01810007.
                 15                    pic X(60) value
         'Ausschlusskriterium # erfüllt: $                           I'.
              10 .
                 15                    pic 9(8)  value 01810008.
                 15                    pic X(60) value
         'VB-/Praxisstufe zu VBNR # nicht ermittelbar, VERTRAGS-ID $.E'.
              10 .
                 15                    pic 9(8)  value 01810009.
                 15                    pic X(60) value
         'Laufdatum passt nicht zu BUCHDAT: # / $                    I'.
              10 .
                 15                    pic 9(8)  value 01810010.
                 15                    pic X(60) value
         'VBNR # hat ungültigen Wertungszeitraum $.                  E'.
              10 .
                 15                    pic 9(8)  value 01810011.
                 15                    pic X(60) value
         'VBNR # zum Buchungsdatum $ nicht mehr aktiv.               E'.
              10 .
                 15                    pic 9(8)  value 01810012.
                 15                    pic X(60) value
         'VBNR # hat ungültige VB-Stufe zum Buch.datum $.            E'.
              10 .
                 15                    pic 9(8)  value 01810013.
                 15                    pic X(60) value
         'VBNR # zum Buchungsdatum $ nicht bekannt.                  E'.
              10 .
                 15                    pic 9(8)  value 01810014.
                 15                    pic X(60) value
         'Verfahren # in der Schnittstelle $ ungültig.               E'.
              10 .
                 15                    pic 9(8)  value 01810015.
                 15                    pic X(60) value
         'Verfahren # für VG $ ungültig.                             E'.
              10 .
                 15                    pic 9(8)  value 01810016.
                 15                    pic X(60) value
         'Zur VBNR # existieren zwei Einträge in der SST $.          E'.
              10 .
                 15                    pic 9(8)  value 01810017.
                 15                    pic X(60) value
         'VBNR # für VG $ ungültig.                                  E'.
              10 .
                 15                    pic 9(8)  value 01810018.
                 15                    pic X(60) value
         'Zum Kopie-Empfänger # existieren zwei Einträge in der SST. E'.
              10 .
                 15                    pic 9(8)  value 01810019.
                 15                    pic X(60) value
         'Kopie-Empfänger passt nicht zur VBNR: # / $                E'.
              10 .
                 15                    pic 9(8)  value 01810020.
                 15                    pic X(60) value
         'Neu berechnete Prov.beträge weichen von den bisherigen ab  I'.
              10 .
                 15                    pic 9(8)  value 01819999.
                 15                    pic X(60) value
         '                                                           I'.
           05 redefines TABZFM-WERTE.
              10 TABZFM                occurs 126
                                       indexed by TABZFM-IND.
                 15 TABZFM-MELDUNGS-NR pic 9(8).
                 15 TABZFM-MELDUNG     pic X(59).
                 15 TABZFM-TYP-KZN     pic X.

