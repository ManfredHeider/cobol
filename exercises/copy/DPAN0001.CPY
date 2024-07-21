      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  11.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAN0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAN0001.CPv  $
      *
      *   Rev 2.1   Jul 07 2020 11:27:20   u130377
      *AAN01460: Reorganisation/Umbau UWAN0001 auf Gesellschaftsmod
      *ule
      *
      *   Rev 2.0   Mar 11 2020 16:42:44   u130377
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01460!11.03.20!2.0 ! Erstellung                ! Töpfer *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Gesellschaftsunabhängige Standard-Routinen für alle
      *    UWAN-Module.
      *
      ****************************************************************
      *
      *-------------------------------------------------------------
       U09-ZWAT0008-DRAT0151 SECTION.
      *-------------------------------------------------------------
      * IN   :
      * OUT  :
      * VERAR: LESEN TBAT0008 ÜBER KTY UND KNR
      *-----------------------------------------------------------------
      *
           MOVE 'U09   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET PI-ZWAT0008-DRAT0151  TO  TRUE
      *
CCCPY1*    CALL K-ZWAT0008 USING BY REFERENCE PI-ZWAT0008
CCC004*                                       PI-DRAT0151
CCC004*                                       PO-DRAT0151
CCC004*                                     ERR-ERROR-BEREICH
CCC004*                                 STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-OK
             EVALUATE TRUE
      *
             WHEN PO-DRAT0151-RC-OK
             WHEN PO-DRAT0151-RC-NOT-FOUND
                  CONTINUE
      *
             WHEN OTHER
                 MOVE 'ZWAT0008/DRAT0151: RETURNCOCE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
                 MOVE PO-DRAT0151-RC    TO ERR-VAR-ZEILE02
                 MOVE D-ERR-VAR-ZEILE03
                   TO ERR-VAR-ZEILE03
                 MOVE D-ERR-VAR-ZEILE04
                   TO ERR-VAR-ZEILE04
                 MOVE K-47              TO ERR-ORT-LFD
                 PERFORM XR93-UPRO-PROG-FEHLER
                 GO TO XST-EXIT
             END-EVALUATE
           ELSE
             GO TO XST-EXIT
           END-IF
      *
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       U10-DATUM-PRUEF SECTION.
      *------------------------------------------------------------*
      * IN   : DATUM 6 STELLIG
      * OUT  : DATUM 10 STELLIG
      * VERAR: DATUM PRÜFEN
      *-----------------------------------------------------------------
           MOVE 'U10  ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET PI-UPPA0016-DRPA0131  TO  TRUE
 
CCCPY1*    CALL K-UPPA0016 USING BY CONTENT   PI-UPPA0016-FUNKTION
CCC004*                                       PI-DRPA0131
CCC004*                          BY REFERENCE PO-DRPA0131
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
 
           IF ERR-RC-SCHWERER-FEHLER
              EXIT PROGRAM
           END-IF
 
           EVALUATE TRUE
           WHEN PO-DRPA0131-RC-OK
                CONTINUE
           WHEN OTHER
                MOVE K-UPPA0016               TO ERR-VAR-ZEILE01
                MOVE '/'                      TO ERR-VAR-ZEILE01(09:01)
                MOVE PI-UPPA0016              TO ERR-VAR-ZEILE01(10:08) 
                MOVE 'DATUM UNGÜLTIG'         TO ERR-VAR-ZEILE01(18:14)
                MOVE PO-DRPA0131-RC           TO ERR-VAR-ZEILE02 
                MOVE 'U010-'                  TO ERR-ORT-SEC
                MOVE K-48                     TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                EXIT PROGRAM               
           END-EVALUATE
           
           .
       U10-EXIT.
           EXIT.

      *----------------------------------------------------------
       U11-TAGESDATUM-ERMITTELN SECTION.
       U11-ANF.
      *------------------------------------------------------------*
      * IN   :
      * OUT  : Z-TAGESDATUM-EUR, Z-DATUM-JJMMTT, Z-ZEIT-HHMMSS
      *        Z-TAGESDATUM-JHJJMMTT
      * VERAR: ERMITTELN AKTUELLES TAGESDATUM
      *----------------------------------------------------------
           SET PI-ZJPA0009-DRPA0253   TO TRUE
CCCPY1*    CALL K-ZJPA0009 USING BY REFERENCE PI-ZJPA0009-FUNKTION
CCC004*                                       PI-DRPA0253
CCC004*                                       PO-DRPA0253
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
           EVALUATE TRUE
               WHEN PO-DRPA0253-RC-OK
                    MOVE PO-DRPA0253-TAGESDATUM-EUR TO
                         Z-TAGESDATUM-EUR

      *             UMFORMATIEREN TT.MM.JHJJ NACH JJMMTT
      *             TT
                    MOVE Z-TAGESDATUM-EUR(01:02)
                      TO Z-DATUM-JJMMTT  (05:02)
      *             MM
                    MOVE Z-TAGESDATUM-EUR(04:02)
                      TO Z-DATUM-JJMMTT  (03:02)
      *             JJ
                    MOVE Z-TAGESDATUM-EUR(09:02)
                      TO Z-DATUM-JJMMTT  (01:02)

      *             UMFORMATIEREN HH.MM.SS NACH HHMMSS
      *             HH
                    MOVE PO-DRPA0253-UHRZEIT-ISO(01:02)
                      TO Z-ZEIT-HHMMSS          (01:02)
      *             MM
                    MOVE PO-DRPA0253-UHRZEIT-ISO(04:02)
                      TO Z-ZEIT-HHMMSS          (03:02)
      *             SS
                    MOVE PO-DRPA0253-UHRZEIT-ISO(07:02)
                      TO Z-ZEIT-HHMMSS          (05:02)
      *
      *             UMFORMATIEREN TT.MM.JHJJ NACH JHJJMMTT
      *             TT
                    MOVE Z-TAGESDATUM-EUR(01:02)
                      TO Z-TAGESDATUM-JHJJMMTT  (07:02)
      *             MM
                    MOVE Z-TAGESDATUM-EUR(04:02)
                      TO Z-TAGESDATUM-JHJJMMTT  (05:02)
      *             JJ
                    MOVE Z-TAGESDATUM-EUR(09:02)
                      TO Z-TAGESDATUM-JHJJMMTT  (03:02)
      *             JH
                    MOVE Z-TAGESDATUM-EUR(07:02)
                      TO Z-TAGESDATUM-JHJJMMTT  (01:02)
      *
               WHEN OTHER
                    MOVE K-49               TO ERR-ORT-LFD
                    MOVE 'PO-DRPA0253-RC UNGUELTIG' TO ERR-VAR-ZEILE01
                    MOVE 'PO-DRPA0253-RC: ' TO ERR-VAR-ZEILE02(1:16)
                    MOVE D-ERR-VAR-ZEILE03
                      TO ERR-VAR-ZEILE03
                    MOVE D-ERR-VAR-ZEILE04
                      TO ERR-VAR-ZEILE04
                    MOVE PO-DRPA0253-RC     TO ERR-VAR-ZEILE02(17:3)
                    MOVE D-ERR-VAR-ZEILE03
                      TO ERR-VAR-ZEILE03
                    MOVE D-ERR-VAR-ZEILE04
                      TO ERR-VAR-ZEILE04
                    PERFORM XR93-UPRO-PROG-FEHLER
                    GO TO XST-EXIT
           END-EVALUATE
           .
       AAA-EXIT.
           EXIT.
      *
      *----------------------------------------------------------
       U12-PE-S-ERMITTELN SECTION.
       U12-ANF.
      *------------------------------------------------------------*
      * IN   : Z-PI-PGEB-GES, Z-PI-AVERM, Z-STRUK-DAT
      * OUT  : PE-STRUKTUR, Z-RC-2
      * VERAR: ERMITTELN PE-STRUKTUR ZU ABSCHLUßVERMITTLER
      *----------------------------------------------------------

      ***  UFKT-2 = 'PE-STRUKTUR ERSTELLEN' WIRD AUS DAISY AUFGERUFEN
           SET  PI-DRPA0192-UFKT-2               TO  TRUE
           SET  PI-UWPA0012-DRPA0192             TO  TRUE
           MOVE Z-PI-AVERM                       TO
                PI-DRPA0192-AVERM
           MOVE Z-PI-PGEB-GES                    TO
                PI-DRPA0192-PGEB-GES
           MOVE Z-STRUK-DAT                      TO
                PI-DRPA0192-STICHTAG
 
      *    MANDANT DES VB ERMITTELN
           INITIALIZE PI-DRVB0180
           MOVE PI-DRPA0192-AVERM     TO PI-DRVB0180-ST-NR
           PERFORM U19-VB-VERTRIEB-GES-ERM
 
           EVALUATE TRUE
           WHEN PO-DRVB0180-RC-OK
                CONTINUE
           WHEN PO-DRVB0180-RC-NOT-FOUND
      ***       DIE FACHLICHEN FEHLER WAREN SO SCHWERWIEGEND, DASS DIE
      ***       NEUE PE-STRUKTUR NICHT KORREKT ERSTELLT WERDEN KONNTE
                MOVE K-RC-PE-S-NICHT-ERMITTELT TO Z-RC-2
                MOVE 'PE-STRUKTUR KONNTE NICHT ERMITTELT WERDEN' TO
                     Z-PO-HINWEISTEXT
                GO TO AAA-EXIT
 
           END-EVALUATE
 
           MOVE PO-DRVB0180-KNR-VERTRIEB-GES  TO
                PI-DRPA0192-MANDANT

CCCPY1*    CALL K-UWPA0012 USING BY REFERENCE PI-UWPA0012-FUNKTION
CCC004*                                       PI-DRPA0192
CCC004*                                       PO-DRPA0192
CCC004*                                       PE-S-NEU
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF

           EVALUATE TRUE

           WHEN PO-DRPA0192-RC-OK
      ***       NEUE PE-STRUKTUR NACH WF-NEU ÜBERTRAGEN.
                MOVE PE-S-NEU      TO WF-ANTRAG-PE-STRUKTUR-NEU

      ***       WEITERE FELDER IN WF-NEU BESTÜCKEN
                MOVE PO-DRPA0192-AVERM TO
                     ANTA-AVERM      IN WF-ANTRAG-NEU
                MOVE PO-DRPA0192-VM    TO
                     ANTA-VM         IN WF-ANTRAG-NEU
                MOVE LS-DIR            TO
                     ANTA-DIR        IN WF-ANTRAG-NEU

           WHEN PO-DRPA0192-RC-NOK
      *    WHEN PO-DRPA0192-RC-UNZ
      ***       DIE FACHLICHEN FEHLER WAREN SO SCHWERWIEGEND, DASS DIE
      ***       NEUE PE-STRUKTUR NICHT KORREKT ERSTELLT WERDEN KONNTE
                MOVE K-RC-PE-S-NICHT-ERMITTELT TO Z-RC-2

                MOVE 'PE-STRUKTUR KONNTE NICHT ERMITTELT WERDEN' TO
                     Z-PO-HINWEISTEXT

                GO TO AAA-EXIT

           WHEN OTHER
                MOVE 'PO-DRPA0192-RC UNBEKANNT:'
                                          TO ERR-VAR-ZEILE01(01:25)
                MOVE PO-DRPA0192-RC       TO ERR-VAR-ZEILE01(27:03)
                MOVE K-50                 TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT

           END-EVALUATE

      *    ERMITTELN PRÜFZIFFER ZU AVERM IN WF-NEU
           MOVE ANTA-AVERM IN WF-ANTRAG-NEU  TO
                PI-DRVB0118-ST-NR

           PERFORM U15-ZWVB0001-DRVB0118

           EVALUATE TRUE
           WHEN PO-DRVB0118-RC-OK

      *         ÜBERNAHME SCHLÜSSELBEGRIFF IN WF-ANTRAG-NEU
                MOVE PO-DRVB0118-ST-PRUEFZIFFER TO
                     ANTA-AVERM-PZ IN WF-ANTRAG-KOPF-NEU

           WHEN PO-DRVB0118-RC-NOT-FOUND
      *         UNGÜLTIGER RETURNCODE
                MOVE 'PO-DRVB0118-RC-NOT-FOUND HIER UNGÜLTIG:'
                                          TO ERR-VAR-ZEILE01
                MOVE PO-DRVB0118-RC       TO ERR-VAR-ZEILE02
                MOVE D-ERR-VAR-ZEILE03
                  TO ERR-VAR-ZEILE03
                MOVE D-ERR-VAR-ZEILE04
                  TO ERR-VAR-ZEILE04
                MOVE K-51                 TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT

           END-EVALUATE

      *    FALLS VM-GESCHÄFT, ERMITTELN PRÜFZIFFER ZU VM FÜR WF-NEU
           EVALUATE TRUE
           WHEN ANTA-VM IN WF-ANTRAG-NEU NOT = K-ZERO
      *              VM-GESCHÄFT
                MOVE ANTA-VM IN WF-ANTRAG-NEU  TO
                     PI-DRVB0118-ST-NR

                PERFORM U15-ZWVB0001-DRVB0118

                EVALUATE TRUE
                WHEN PO-DRVB0118-RC-OK

      *              ÜBERNAHME SCHLÜSSELBEGRIFF IN WF-ANTRAG-NEU
                     MOVE PO-DRVB0118-ST-PRUEFZIFFER TO
                          ANTA-VM-PZ IN WF-ANTRAG-KOPF-NEU

                WHEN PO-DRVB0118-RC-NOT-FOUND
      *              UNGÜLTIGER RETURNCODE
                     MOVE 'PO-DRVB0118-RC-NOT-FOUND HIER UNGÜLTIG:'
                                               TO ERR-VAR-ZEILE01
                     MOVE PO-DRVB0118-RC       TO ERR-VAR-ZEILE02
                     MOVE D-ERR-VAR-ZEILE03
                       TO ERR-VAR-ZEILE03
                     MOVE D-ERR-VAR-ZEILE04
                       TO ERR-VAR-ZEILE04
                     MOVE K-52                 TO ERR-ORT-LFD
                     PERFORM XR93-UPRO-PROG-FEHLER
                     GO TO XST-EXIT

                END-EVALUATE

           WHEN OTHER

                CONTINUE

           END-EVALUATE

      *
      ***  INHALTLICHE PRÜFUNG ABSCHLUßVERMITTLER
      ***  - ER DARF NICHT GESPERRT SEIN
      *
           MOVE K-KTY-GESPERRTE-VB TO
                PI-DRAT0143-KTY-NR

           MOVE ANTA-AVERM                IN WF-ANTRAG-NEU TO
                Z-MF-9-07
           MOVE Z-MF-X-07                                  TO
                PI-DRAT0143-KNR-BEZ10

           PERFORM U13-ZWAT0008-DRAT0143

           EVALUATE TRUE
           WHEN PO-DRAT0143-RC-OK

      *         VB IST GESPERRT
                MOVE K-RC-PE-S-NICHT-ERMITTELT                     TO
                     Z-RC-2
                MOVE PO-DRAT0143-KNR-BEZ  TO
                     Z-PO-HINWEISTEXT

                GO TO AAA-EXIT

           WHEN PO-DRAT0143-RC-NOT-FOUND

                CONTINUE

           END-EVALUATE

      *
      ***  INHALTL PRÜFUNG ANTRAGSPERRE BEI VERSICHERUNGSPRODUKTEN
      ***  1. SCHRITT: IST PRODUKTGEBER BETROFFEN?
      *
           MOVE K-KTY-AN-SPERRE-VPROD-PGEB
             TO PI-DRAT0151-KTY-NR
           MOVE Z-PI-PGEB-GES
             TO PI-DRAT0151-KNR-NR

           PERFORM U09-ZWAT0008-DRAT0151

           EVALUATE TRUE
           WHEN PO-DRAT0151-RC-OK
      *         VON DIESEM PRODUKTGEBER WERDEN VERSICHERUNGSPRODUKTE
      *         VERMITTELT
      *         2.SCHRITT: HAT VERMITTLER HINSICHTLICH VERS.PRODUKTEN
      *                    EINE BESCHRÄNKUNG?
      *
                INITIALIZE PI-DRVB0482
                           PO-DRVB0482

                MOVE ANTA-AVERM
                  IN WF-ANTRAG-NEU
                  TO PI-DRVB0482-BES-VB-NR
                MOVE K-KTY-BES-GRND
                  TO PI-DRVB0482-BES-KTY-NR
                MOVE K-KNR-ANTRAGSPERRE-VERS-PROD
                  TO PI-DRVB0482-BES-KNR-NR
                MOVE Z-TAGESDATUM-EUR
                  TO PI-DRVB0482-BES-ZEITPUNKT

                PERFORM U072-ZWVB0020-DRVB0482

                EVALUATE TRUE
                WHEN PO-DRVB0482-RC-OK
      *              VERMITTLER IST GESPERRT!
                     MOVE K-RC-NOK TO Z-RC

                     MOVE 'VB DARF KEINE VERSICHERUNGSPRODUKTE VERMITTEL
      -                   'N -> BESCHRÄNKUNG 248 IN VB21'
                       TO Z-PO-HINWEISTEXT

                     GO TO AAA-EXIT

                WHEN OTHER
      *              VERMITTLER IST NICHT GESPERRT
                     CONTINUE

                END-EVALUATE


           WHEN PO-DRAT0151-RC-NOT-FOUND
      *         VON DIESEM PRODUKTGEBER WERDEN KEINE VERSICHERUNGS-
      *         PRODUKTE VERMITTELT
                CONTINUE

           END-EVALUATE
      *
           MOVE K-RC-PE-S-ERMITTELT TO Z-RC-2
           .
       AAA-EXIT.
           EXIT.
      *
      *-------------------------------------------------------------
       U13-ZWAT0008-DRAT0143 SECTION.
      *----------------------------------------------------------
      * IN   :
      * OUT  :
      *-----------------------------------------------------------------
      *
           MOVE 'U13   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET PI-ZWAT0008-DRAT0143  TO  TRUE
      *
CCCPY1*    CALL K-ZWAT0008 USING BY REFERENCE PI-ZWAT0008
CCC004*                                       PI-DRAT0143
CCC004*                                       PO-DRAT0143
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                 STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-OK
             EVALUATE TRUE
      *
             WHEN PO-DRAT0143-RC-OK
             WHEN PO-DRAT0143-RC-NOT-FOUND
                  CONTINUE
      *
             WHEN OTHER
                 MOVE 'ZWAT0008/DRAT0143: RETURNCOCE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
                 MOVE PO-DRAT0143-RC    TO ERR-VAR-ZEILE02
                 MOVE D-ERR-VAR-ZEILE03
                   TO ERR-VAR-ZEILE03
                 MOVE D-ERR-VAR-ZEILE04
                   TO ERR-VAR-ZEILE04
                 MOVE K-53              TO ERR-ORT-LFD
                 PERFORM XR93-UPRO-PROG-FEHLER
                 GO TO XST-EXIT
             END-EVALUATE
           ELSE
             GO TO XST-EXIT
           END-IF
      *
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *-------------------------------------------------------------
       U15-ZWVB0001-DRVB0118 SECTION.
      *----------------------------------------------------------
      * IN   :
      * OUT  :
      * VERAR: LESEN TBVB0001 ÜBER STELLENNUMMER
      *-----------------------------------------------------------------
      *
           MOVE 'U15   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET PI-ZWVB0001-DRVB0118  TO  TRUE
      *
CCCPY1*    CALL K-ZWVB0001 USING BY REFERENCE PI-ZWVB0001
CCC004*                                       PI-DRVB0118
CCC004*                                       PO-DRVB0118
CCC004*                                     ERR-ERROR-BEREICH
CCC004*                                 STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-OK
             EVALUATE TRUE
      *
             WHEN PO-DRVB0118-RC-OK
             WHEN PO-DRVB0118-RC-NOT-FOUND
                  CONTINUE
      *
             WHEN OTHER
                 MOVE 'ZWVB0001/DRVB0118: RETURNCOCE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
                 MOVE PO-DRVB0118-RC    TO ERR-VAR-ZEILE02
                 MOVE D-ERR-VAR-ZEILE03
                   TO ERR-VAR-ZEILE03
                 MOVE D-ERR-VAR-ZEILE04
                   TO ERR-VAR-ZEILE04
                 MOVE K-56              TO ERR-ORT-LFD
                 PERFORM XR93-UPRO-PROG-FEHLER
                 GO TO XST-EXIT
             END-EVALUATE
           ELSE
             GO TO XST-EXIT
           END-IF
      *
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
      * U19-VERTRIEB-GES-ERMITTELN                                 *
      *------------------------------------------------------------*
      * AUFRUF EINES VB-UPROS ZUR ERMITTLUNG DER VERTRIEBSGESELL-  *
      * SCHAFT, ZU DER DER VB GEHÖRT                               *
      *------------------------------------------------------------*
       U19-VB-VERTRIEB-GES-ERM SECTION.
      *
           SET PI-ZWVB0001-DRVB0180           TO TRUE
      *
CCCPY1*    CALL K-ZWVB0001 USING BY REFERENCE PI-ZWVB0001-FUNKTION
CCC004*                                       PI-DRVB0180
CCC004*                                       PO-DRVB0180
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-OK
              EVALUATE TRUE
 
              WHEN PO-DRVB0180-RC-OK
              WHEN PO-DRVB0180-RC-NOT-FOUND
                   CONTINUE
              WHEN OTHER
                   MOVE 'ZWVB0001/DRVB0180: RETURNCODE UNGUELTIG '
                                                    TO ERR-VAR-ZEILE01
                   MOVE PO-DRVB0180-RC              TO ERR-VAR-ZEILE02
                   MOVE K-114                       TO ERR-ORT-LFD
                   PERFORM XR93-UPRO-PROG-FEHLER
                   GO TO XST-EXIT
              END-EVALUATE
           END-IF
      *
           .
       U19-EXIT.
           EXIT.
      *
      *----------------------------------------------------------
       U34-ZWVB0014-DRVB0584 SECTION.
      *----------------------------------------------------------
      * IN   : PI-DRVB0584
      * OUT  : PO-DRVB0584
      * VERAR: LESEN TBVB0014 ÜBER STELLENNUMMER
      *-----------------------------------------------------------
      *
           MOVE 'U34   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
 
           SET PI-ZWVB0014-DRVB0584  TO  TRUE
 
CCCPY1*    CALL K-ZWVB0014 USING BY REFERENCE PI-ZWVB0014
CCC004*                                       PI-DRVB0584
CCC004*                                       PO-DRVB0584
CCC004*                                     ERR-ERROR-BEREICH
CCC004*                                 STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
 
           IF ERR-RC-OK
             EVALUATE TRUE
 
             WHEN PO-DRVB0584-RC-OK
             WHEN PO-DRVB0584-RC-NOT-FOUND
                  CONTINUE
 
             WHEN OTHER
                 MOVE 'ZWVB0014/DRVB0584: RETURNCOCE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
                 MOVE PO-DRVB0584-RC    TO ERR-VAR-ZEILE02
                 MOVE D-ERR-VAR-ZEILE03
                   TO ERR-VAR-ZEILE03
                 MOVE D-ERR-VAR-ZEILE04
                   TO ERR-VAR-ZEILE04
                 MOVE K-1               TO ERR-ORT-LFD
                 PERFORM XR93-UPRO-PROG-FEHLER
                 GO TO XST-EXIT
             END-EVALUATE
           ELSE
             GO TO XST-EXIT
           END-IF
           .
       AAA-EXIT.
           EXIT.
      *
      *-------------------------------------------------------------
       U51-TDS-SCHREIBEN SECTION.
      *-------------------------------------------------------------
      * IN   : PI-UWAN0001-FUNKTION, WF-ANTRAG-ALT, WF-ANTRAG-NEU
      * OUT  :
      * VERAR: SCHREIBEN ANTRAG-TDS
      *-------------------------------------------------------------
      *
           MOVE 'U51   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           MOVE K-ZERO
             TO Z-TDS-FOLGE

      *    TDS-DATUM AUS TAGESDATUM
           MOVE Z-TAGESDATUM-EUR
             TO Z-DATUM-TTPMMPJHJJ
           PERFORM U54-TTPMMPJHJJ-NACH-JJMMTT
           MOVE Z-DATUM-JJMMTT-NUM
             TO Z-TDS-DATUM

           EVALUATE TRUE
           WHEN STAT-DIALOG

                MOVE 'AA'
                  TO Z-TDS-MODUS

                MOVE WF-ANTRAG-NEU
                  TO Z-WF-ANTRAG-DATEN

                PERFORM U52-WF-NACH-TDS-UMSETZEN
                PERFORM U53-ZAUU0301-DRAN0165

           WHEN OTHER

                CONTINUE

           END-EVALUATE

           ADD K-1
             TO Z-TDS-FOLGE

           EVALUATE TRUE

           WHEN PI-UWAN0001-DRAN0148
      *         NEUANLAGE
           WHEN PI-UWAN0001-DRAN0192
      *         NUR TDS SCHREIBEN

                MOVE 'I '
                  TO Z-TDS-MODUS
                MOVE WF-ANTRAG-NEU
                  TO Z-WF-ANTRAG-DATEN
                PERFORM U52-WF-NACH-TDS-UMSETZEN
                PERFORM U53-ZAUU0301-DRAN0165


           WHEN PI-UWAN0001-DRAN0149
      *         ÄNDERUNG

      *         FESTLEGEN MODUS FÜR ALTEN SATZ
                EVALUATE TRUE
                WHEN WF-ANTRAG-PE-STRUKTUR-NEU
                   = WF-ANTRAG-PE-STRUKTUR-ALT
                 AND ANTA-AVERM IN WF-ANTRAG-NEU
                   = ANTA-AVERM IN WF-ANTRAG-ALT
      *              PE-STRUKTUR OHNE ÄNDERUNG
                     MOVE 'UA'
                       TO Z-TDS-MODUS
                     MOVE WF-ANTRAG-ALT
                       TO Z-WF-ANTRAG-DATEN
      *
                     PERFORM U52-WF-NACH-TDS-UMSETZEN
                     PERFORM U53-ZAUU0301-DRAN0165
      *
                     ADD K-1
                      TO Z-TDS-FOLGE
      *
                     MOVE 'UN'
                       TO Z-TDS-MODUS
      *              MOVE WF-ANTRAG-ALT
                     MOVE WF-ANTRAG-NEU
                       TO Z-WF-ANTRAG-DATEN
      *
                     PERFORM U52-WF-NACH-TDS-UMSETZEN
                     PERFORM U53-ZAUU0301-DRAN0165
      *
                WHEN ANTA-AVERM IN WF-ANTRAG-NEU
                   = ANTA-AVERM IN WF-ANTRAG-ALT
      *              PE-STRUKTUR GEÄNDERT, AVERM GLEICH

                     MOVE 'U1'
                       TO Z-TDS-MODUS
                     MOVE WF-ANTRAG-ALT
                       TO Z-WF-ANTRAG-DATEN
      *
                     PERFORM U52-WF-NACH-TDS-UMSETZEN
                     PERFORM U53-ZAUU0301-DRAN0165
      *
                     ADD K-1
                      TO Z-TDS-FOLGE
      *
                     MOVE 'U2'
                       TO Z-TDS-MODUS
                     MOVE WF-ANTRAG-NEU
                       TO Z-WF-ANTRAG-DATEN
      *
                     PERFORM U52-WF-NACH-TDS-UMSETZEN
                     PERFORM U53-ZAUU0301-DRAN0165
      *
                WHEN OTHER
      *              PE-STRUKTUR GEÄNDERT, AVERM GEÄNDERT

                     MOVE 'D1'
                       TO Z-TDS-MODUS
                     MOVE WF-ANTRAG-ALT
                       TO Z-WF-ANTRAG-DATEN
      *
                     PERFORM U52-WF-NACH-TDS-UMSETZEN
                     PERFORM U53-ZAUU0301-DRAN0165
      *
                     ADD K-1
                      TO Z-TDS-FOLGE
      *
                     MOVE 'I2'
                       TO Z-TDS-MODUS
                     MOVE WF-ANTRAG-NEU
                       TO Z-WF-ANTRAG-DATEN
      *
                     PERFORM U52-WF-NACH-TDS-UMSETZEN
                     PERFORM U53-ZAUU0301-DRAN0165
      *
                END-EVALUATE
      *
           WHEN PI-UWAN0001-DRAN0162
      *         LÖSCHUNG

                MOVE 'DU'
                  TO Z-TDS-MODUS
                MOVE WF-ANTRAG-ALT
                  TO Z-WF-ANTRAG-DATEN
                PERFORM U52-WF-NACH-TDS-UMSETZEN
                PERFORM U53-ZAUU0301-DRAN0165
      *
                ADD K-1
                 TO Z-TDS-FOLGE
      *
           WHEN OTHER
      *
                CONTINUE
      *
           END-EVALUATE
      *
           EVALUATE TRUE
           WHEN STAT-DIALOG

                MOVE 'ZZ'
                  TO Z-TDS-MODUS
                MOVE WF-ANTRAG-NEU
                  TO Z-WF-ANTRAG-DATEN
      *
                MOVE K-9
                  TO Z-TDS-FOLGE
      *
                PERFORM U52-WF-NACH-TDS-UMSETZEN
                PERFORM U53-ZAUU0301-DRAN0165

           WHEN OTHER

                CONTINUE

           END-EVALUATE

      *
           .
       AAA-EXIT.
           EXIT.
      *
      *-------------------------------------------------------------
       U53-ZAUU0301-DRAN0165 SECTION.
      *-------------------------------------------------------------
      * IN   :
      * OUT  :
      * VERAR: AUFRUF DRAN0165
      *-------------------------------------------------------------
      *
           MOVE 'U53   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET  PI-ZAUU0301-DRAN0165 TO TRUE

CCCPY1*    CALL K-ZAUU0301 USING BY REFERENCE
CCC004*                           PI-ZAUU0301
CCC004*                           PI-DRAN0165
CCC004*                           PO-DRAN0165
CCC004*                           ERR-ERROR-BEREICH
CCC004*                           STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-OK
             EVALUATE TRUE
      *
             WHEN PO-DRAN0165-RC-OK
                  CONTINUE
      *
             WHEN OTHER
                 MOVE 'ZAUU0301/DRAN0165: RETURNCOCE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
                 MOVE PO-DRAN0165-RC    TO ERR-VAR-ZEILE02
                 MOVE D-ERR-VAR-ZEILE03
                   TO ERR-VAR-ZEILE03
                 MOVE D-ERR-VAR-ZEILE04
                   TO ERR-VAR-ZEILE04
                 MOVE K-74              TO ERR-ORT-LFD
                 PERFORM XR93-UPRO-PROG-FEHLER
                 GO TO XST-EXIT
             END-EVALUATE
           ELSE
             GO TO XST-EXIT
           END-IF
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *-------------------------------------------------------------
       U54-TTPMMPJHJJ-NACH-JJMMTT   SECTION.
      *-------------------------------------------------------------
      * IN   : Z-TTPMMPJHJJ
      * OUT  : Z-DATUM-JJMMTT
      * VERAR: UMWANDELN DATUM
      *-------------------------------------------------------------
      *
           MOVE 'U54   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
      *    UMFORMATIEREN TT.MM.JHJJ NACH JJMMTT
      *    TT
           MOVE Z-DATUM-TTPMMPJHJJ(01:02)
             TO Z-DATUM-JJMMTT    (05:02)
      *    MM
           MOVE Z-DATUM-TTPMMPJHJJ(04:02)
             TO Z-DATUM-JJMMTT    (03:02)
      *    JJ
           MOVE Z-DATUM-TTPMMPJHJJ(09:02)
             TO Z-DATUM-JJMMTT    (01:02)
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       U55-DAUER-IN-MONATEN-ERMITTELN SECTION.
       U55-ANF.
      *------------------------------------------------------------*
      * IN   : Z-DATUM-TTPMMPJHJJ-1, Z-DATUM-TTPMMPJHJJ-2
      * OUT  : Z-DAUER-IN-MONATEN
      * VERAR: ZEITRAUM ERMITTELN
      *-----------------------------------------------------------------
           MOVE 'U55  ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET PI-UPPA0016-DRPA0146
            TO  TRUE
      *
           SET PI-DRPA0146-FORMAT-MON-MIT
            TO TRUE

           MOVE Z-DATUM-TTPMMPJHJJ-1
             TO PI-DRPA0146-DATUM-ANF

           MOVE Z-DATUM-TTPMMPJHJJ-2
             TO PI-DRPA0146-DATUM-END

CCCPY1*    CALL K-UPPA0016 USING BY REFERENCE  PI-UPPA0016-FUNKTION
CCC004*                                     PI-DRPA0146
CCC004*                                     PO-DRPA0146
CCC004*                                     ERR-ERROR-BEREICH
CCC004*                                     STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF

           EVALUATE TRUE
           WHEN PO-DRPA0146-RC-OK

                MOVE PO-DRPA0146-DAUER
                  TO Z-DAUER-IN-MONATEN

           WHEN PO-DRPA0146-RC-NOK
      *
                EVALUATE TRUE
                WHEN STAT-DIALOG

                     CONTINUE

                WHEN STAT-BATCH
      *              IM KONTEXT BATCH
      *              SETZEN RC AUF OK
                     SET PO-DRPA0146-RC-OK TO TRUE
      *              SETZEN DAUER IN MONATEN AUF 12
                     MOVE K-12
                       TO Z-DAUER-IN-MONATEN
                END-EVALUATE
      *
           WHEN OTHER
      *
                EVALUATE TRUE
                WHEN STAT-DIALOG

                     MOVE 'UPPA0016/DRPA0146: RETURNCOCE UNGUELTIG'
                                            TO ERR-VAR-ZEILE01
                     MOVE PO-DRPA0146-RC    TO ERR-VAR-ZEILE02
                     MOVE D-ERR-VAR-ZEILE03
                       TO ERR-VAR-ZEILE03
                     MOVE D-ERR-VAR-ZEILE04
                       TO ERR-VAR-ZEILE04
                     MOVE K-75              TO ERR-ORT-LFD
                     PERFORM XR93-UPRO-PROG-FEHLER
                     GO TO XST-EXIT

                WHEN STAT-BATCH
      *              IM KONTEXT BATCH
      *              SETZEN RC AUF OK
                     SET PO-DRPA0146-RC-OK TO TRUE
      *              SETZEN DAUER IN MONATEN AUF 12
                     MOVE K-12
                       TO Z-DAUER-IN-MONATEN
                END-EVALUATE
      *
           END-EVALUATE
           .
       AAA-EXIT.
           EXIT.
      *
      *-------------------------------------------------------------
       U56-ZWVB0009-DRVB0329 SECTION.
      *----------------------------------------------------------
      * IN   : Z-ST-NR, Z-DATUM-JJMMTT, PI-DRVB0329-MS-BEZ-KURZ
      * OUT  : PO-DRVB0329-MS-NR, GGF. Z-TDS-EMP-STUFE
      * VERAR: ERMITTELN MS-NR ZU STUFENTEXT
      *-----------------------------------------------------------------
      *
           MOVE 'U56   ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE

           EVALUATE TRUE
           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-VM09
      *         OK
                SET PO-DRVB0329-RC-OK
                 TO TRUE

                MOVE K-9
                  TO PO-DRVB0329-MS-NR
      *
                GO TO AAA-EXIT

           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-SD
      *         OK
                SET PO-DRVB0329-RC-OK
                 TO TRUE

                MOVE 889
                  TO PO-DRVB0329-MS-NR
      *
                GO TO AAA-EXIT

           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-RF-T
           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-RFT
      *         OK
                SET PO-DRVB0329-RC-OK
                 TO TRUE

                MOVE 888
                  TO PO-DRVB0329-MS-NR
      *
                GO TO AAA-EXIT

           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-RF-R
      *         OK
                SET PO-DRVB0329-RC-OK
                 TO TRUE

                MOVE 889
                  TO PO-DRVB0329-MS-NR
      *
                GO TO AAA-EXIT

           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-CPD
           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-KL
      *         OK
                SET PO-DRVB0329-RC-OK
                 TO TRUE

                MOVE 999
                  TO PO-DRVB0329-MS-NR
      *
                GO TO AAA-EXIT

           WHEN PI-DRVB0329-MS-BEZ-KURZ
              = K-STUFE-ED
      *         OK
                SET PO-DRVB0329-RC-OK
                 TO TRUE

                MOVE 768
                  TO PO-DRVB0329-MS-NR
      *
                GO TO AAA-EXIT

           WHEN OTHER

                CONTINUE

           END-EVALUATE
      *
           SET PI-ZWVB0009-DRVB0329  TO  TRUE
      *
CCCPY1*    CALL K-ZWVB0009 USING BY REFERENCE PI-ZWVB0009
CCC004*                                       PI-DRVB0329
CCC004*                                       PO-DRVB0329
CCC004*                                     ERR-ERROR-BEREICH
CCC004*                                 STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-OK
             EVALUATE TRUE
      *
             WHEN PO-DRVB0329-RC-OK
             WHEN PO-DRVB0329-RC-LEER
             WHEN PO-DRVB0329-RC-EOF
             WHEN PO-DRVB0329-RC-NOT-FOUND
                  CONTINUE
      *
             WHEN OTHER
                 MOVE 'ZWVB0009/DRVB0329: RETURNCOCE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
                 MOVE PO-DRVB0329-RC    TO ERR-VAR-ZEILE02
                 MOVE D-ERR-VAR-ZEILE03
                   TO ERR-VAR-ZEILE03
                 MOVE D-ERR-VAR-ZEILE04
                   TO ERR-VAR-ZEILE04
                 MOVE K-76              TO ERR-ORT-LFD
                 PERFORM XR93-UPRO-PROG-FEHLER
                 GO TO XST-EXIT
             END-EVALUATE
           ELSE
             GO TO XST-EXIT
           END-IF
      *
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       U60-ZTAN0008-DRAN0187 SECTION.
       X0325-ANF.
      *------------------------------------------------------------*
      * INPUT : WF-ANTRAG-NEU                                      *
      * OUTPUT:                                                    *
      * VERARB: LESEN ANTRAG ÜBER PIN                              *
      *------------------------------------------------------------*
      *
           MOVE PGEB-GES
             IN WF-ANTRAG-KOPF-NEU
             TO PGEB-GES IN PI-DRAN0187

           MOVE ANTA-PIN
             IN WF-ANTRAG-NEU
             TO ANTA-PIN IN PI-DRAN0187
      *
           SET PI-ZTAN0008-DRAN0187-SEL-PIN TO TRUE
      *
CCCPY1*    CALL K-ZTAN0008 USING BY REFERENCE PI-ZTAN0008
CCC004*                                       PI-DRAN0187
CCC004*                                       PO-DRAN0187
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
      *    RETURNCODE ABFRAGEN
           EVALUATE TRUE
           WHEN PO-DRAN0187-RC-OK
           WHEN PO-DRAN0187-RC-NOT-FOUND

                CONTINUE
      *
           WHEN OTHER
                MOVE 'ZTAN0008/DRAN0187: RETURNCOCE UNGUELTIG'
                                              TO ERR-VAR-ZEILE01
                MOVE PO-DRAN0187-RC           TO ERR-VAR-ZEILE02
                MOVE D-ERR-VAR-ZEILE03
                  TO ERR-VAR-ZEILE03
                MOVE D-ERR-VAR-ZEILE04
                  TO ERR-VAR-ZEILE04
                MOVE K-81                     TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT
           END-EVALUATE
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       U61-UPAT0035 SECTION.
       AAA-1000.
      *------------------------------------------------------------*
      * INPUT : TXT-TEXT, TXT-LAENGE                               *
      * OUTPUT: TXT-TEXT                                           *
      * VERARB: UMWANDELN TEXT IN GROSS-KLEINSCHREIBUNG            *
      *------------------------------------------------------------*
      *
CCCPY1*    CALL K-UPAT0035 USING TXT
CCC004*    .
      *
CCC004*AAA-EXIT.
CCC004*    EXIT.
      *
      *------------------------------------------------------------*
       U62-GROSSBUCHSTABEN  SECTION.
       AAA-1000.
      *------------------------------------------------------------*
      * INPUT : TXT-TEXT, TXT-LAENGE                               *
      * OUTPUT: TXT-TEXT                                           *
      * VERARB: UMWANDELN TEXT IN GROSSBUCHSTABEN                  *
      *------------------------------------------------------------*
      *
140120*    INSPECT TXT-TEXT(1:TXT-LAENGE)
140120*      CONVERTING EBCDIC-LOWER TO EBCDIC-UPPER

           .
      *
       AAA-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------
       U062-ZTAN0011-DRAN0183  SECTION.
      *-----------------------------------------------------------
      * IN   : PI-DRAN0183
      * OUT  : PO-DRAN0183
      * VERAR: SELECT TBAN0011 PLAUSI
      *-----------------------------------------------------------
      *
           MOVE 'U062  ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
      *    ERMITTELN DATEN

           SET  PI-ZTAN0011-DRAN0183-SEL  TO TRUE

CCCPY1*    CALL K-ZTAN0011 USING BY REFERENCE PI-ZTAN0011
CCC004*         PI-DRAN0183
CCC004*         PO-DRAN0183
CCC004*         ERR-ERROR-BEREICH
CCC004*         STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAN0183-RC-OK
           WHEN PO-DRAN0183-RC-NOT-FOUND

                CONTINUE

           WHEN OTHER

                MOVE 'PO-DRAN0183-RC UNGÜLTIG:'
                                          TO ERR-VAR-ZEILE01(01:25)
                MOVE PO-DRAN0183-RC       TO ERR-VAR-ZEILE01(27:03)
                MOVE K-82                 TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT

           END-EVALUATE

           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       U072-ZWVB0020-DRVB0482 SECTION.
      *------------------------------------------------------------*
      * INPUT:  PI-DRVB0482                                        *
      * OUTPUT: PO-DRVB0482                                        *
      * VERARB: DRVB0482                                           *
      *------------------------------------------------------------*
      *
           MOVE 'U072  ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
      *
           SET  PI-ZWVB0020-DRVB0482  TO TRUE
      *
CCCPY1*    CALL K-ZWVB0020 USING BY REFERENCE PI-ZWVB0020
CCC004*                                       PI-DRVB0482
CCC004*                                       PO-DRVB0482
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRVB0482-RC-OK
           WHEN PO-DRVB0482-RC-NOK
           WHEN PO-DRVB0482-RC-NOT-FOUND
      *
                CONTINUE
      *
           WHEN OTHER
              MOVE 'FKT. DRVB0482 UNG. RC' TO ERR-VAR-ZEILE01
              MOVE PO-DRVB0482-RC          TO ERR-VAR-ZEILE02
              MOVE K-33                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
      *
           END-EVALUATE
      *
           .
       AAA-9000.
           EXIT.
      *
      * ---------------------------------------------------
      * DPAN0001-ENDE
      * ---------------------------------------------------
      *

