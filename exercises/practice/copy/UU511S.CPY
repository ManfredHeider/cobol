      * T13EX15   Stand: 19/08/98    08:49:23
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU511S.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/MF/CPY/UU511S.CPv  $
      *
      *   Rev 2.12   Feb 06 2002 15:13:32   t13ex15
      *FOP00379: Korrektur der Einheitenberechnung
      *
      *   Rev 2.11   Dec 20 2001 09:17:00   t13ex33
      *AAT00022 - EURO-Umstellung - Korrektur
      *
      *   Rev 2.10   Dec 18 2001 14:39:42   t13ex33
      *AAT00022 - Euro-Umstellung
      *
      *   Rev 2.9   13 Nov 2001 11:24:22   t13ex15
      *FOP00375: Korrektur der Einheitenberechnung bei Ges. 26
      *
      *   Rev 2.8   12 Oct 2001 12:10:20   t13ex15
      *FOP00371: Ges 26: keine Einheiten bei Folgeprvision
      *
      *   Rev 2.7   15 May 2001 14:55:46   t13ex15
      *FOP00362: Rücknahme 80%-Begrenzung bei WAL,
      *
      *
      *
      *   Rev 2.6   21 Nov 2000 13:18:38   T13EX31
      *AOP00213: EURO-Umstellung - Zwischenversion
      *
      *   Rev 2.5   21 Nov 2000 09:48:44   T13EX31
      *AOP00213 - EURO-Umstellung                 
      *
      *   Rev 2.4   12 Sep 2000 09:20:50   T13EX15
      *AOP00204: Neue Produkte (Ges. 63)
      *
      *   Rev 2.3   Jan 06 2000 12:20:56   T13EX15
      *AOP00190: Einf. Ges. 16, 26 Generali
      *
      *   Rev 2.2   Sep 09 1999 11:03:58   T13EX15
      *AOP00185: Einf. Ges. 68, Commerzbank
      *
      *   Rev 2.1   Jul 12 1999 15:17:28   T13EX15
      *AOP00183: Einf. Einheitenaufschlag (Ges.25)
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.5   04 Nov 1998 15:13:44   T13EX15
      *AOP00149 - Fachl. Anpassung f. Ges. 35
      *
      *   Rev 1.4   04 Sep 1998 12:22:32   T13EX15
      *AOP00149 - Einf. Ges. 35, 55 (AVBG)
      *
      *   Rev 1.3   May 22 1997 08:05:44   T13EX15
      *FEOP0193: Zurueckfallen auf Vorversion (1.1)
      *
      *   Rev 1.2   Apr 22 1997 15:27:24   T13EX15
      *AFOP0065: Einf./Anpassung Kundenbetr.-Provision
      *
      *   Rev 1.1   Mar 05 1996 11:01:40   T13AE16
      *AFOP0093 Ges. 95 neu
      *
      *   Rev 1.0   Nov 04 1995 10:00:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   24    27.01.95 11:26:03
      *****************************************************************
      *   COPY-MEMBER  UU511S    PROVISIONSBERECHNUNGEN FÜR           *
      *                          OPAL 2 - GESELLSCHAFTEN   UND        *
      *                          OPAL 3 - GESELLSCHAFTEN              *
      *---------------------------------------------------------------*
      *                                                               *
      * VERSION 02  VOM 12.02.88 ZU BFG / STRUKTUREN GEÄNDERT SCHMOLL *
      * VERSION 03  PROVISIONSBETRAG AUS OP-SATZ SUBTRAHIERT GES 40   *
      * VERSION 04  PROVISIONSBETRAG AUS OP-SATZ SUBTRAHIERT GES 70   *
      * VERSION 05  PROVISIONSBETRAG AUS OP-SATZ SUBTRAHIERT GES 41   *
      * VERSION 06  PROVISIONSBETRAG AUS OP-SATZ SUBTRAHIERT GES 46   *
      * VERSION 07  MIT XX,7 GEKENNZEICHNETE VB'S DURCH 8000020 ER-   *
      *             SETZEN.                                           *
      * VERSION 08  EINFUEHRUNG NEUE GES 47 BFG:IMMO UND              *
      *             63 DREBA BERLIN                                   *
      * VERSION 09  EINFUEHRUNG NEUE GES 32 BANKHAUS SCHOELLER        *
      * VERSION 10  EINFUEHRUNG NEUE GES 21 VOLKSFUERSORGE            *
      *             EINFUEHRUNG NEUE GES 82 DT. MIETERBUND RECHTS.    *
      * VERSION 11  EINFUEHRUNG NEUE GES 81 ADVOCARD                  *
      * VERSION 12  EINFUEHRUNG NEUE PRODARTEN 7 UND 8                *
      * VERSION 13  EINFUEHRUNG NEUE GES 91 `SONSTIGE PRODUKTE`       *
      * VERSION 14  EINFUEHRUNG NEUE GES 42 `DAM`                     *
      * VERSION 15  GES 32 MULTIPLIKATOR 0,87 ENTFERNT    / KRUMPHOLZ *
      * VERSION 16  EH-BERECHNUNG GES 31 GEAENDERT        / MUSALL    *
      * VERSION 17  EH-BERECHNUNG GES 31 KORRIGIERT       / MUSALL    *
      * VERSION 18                           11.03.94     /BOHLINGER  *
      *        AFOP0007    EINFÜHRUNG GES. 43 DREBA-SCHWEIZ           *
      *        AFOP0008    EINFÜHRUNG GES. 36 VILA VITA               *
      * VERSION 19                           06.06.1994  /BOHLINGER   *
      *             EINFÜHRUNG AVBG WIEN   S-BAUSPARKASSE GES 75      *
      *                                    WA-LEBEN       GES 25      *
      *             VARIABLE FÜR VTF UND VWF                          *
      * VERSION 20                           05.08.94    /BOHLINGER   *
      *             EINFÜHRUNG   WIENER ALLIANZ VERSICHERUNG GES. 15  *
      * VERSION 21                           09.09.94    /GSELLHOFER  *
      *             EINFÜHRUNG   GESELLSCHAFT 49  DGI                 *
      * VERSION 22  AFOP0029                 06.10.94    /BOHLINGER   *
      *             EINFÜHRUNG   BANK AUSTRIA GES. 45 UND 65          *
      * VERSION 22  AFOP0033                 06.10.94    /BOHLINGER   *
      *             EINFÜHRUNG   DIT ÖSTERREICH GES. 44               *
      * VERSION 23  AFOP0046                 23.12.94    /BOHLINGER   *
      *             BEGRENZUNG DER EINHEITEN AUF MAX. 1000 EH         *
      * VERSION 24  AFOP0050                 24.01.95    /BOHLINGER   *
      *             STRUKTURERWEITERUNG HERAUSGENOMMEN                *
      * VERSION 25  AFOP0093                 ......95    /BOHLINGER   *
      *             einführung sondergesellschaft 95   g95            *
      * VERSION 26  AOP00149                 ......98    /GSELLHOFER  *
      *             EINFüHRUNG GES. 35,55 (AVBG)   014.09.98          *
      * VERSION ..  AOP00149                 ......98    /GSELLHOFER  *
      *             Fachl. Aendrg.  Ges 35(AVBG)    XX.10.98          *
990712* VERSION ..  AOP00183                 ......98    /GSELLHOFER  *
990712*             Einh.-Aufschlag Ges 25(AVBG)    12.07.99          *
990909* VERSION ..  AOP00185                 ......98    /GSELLHOFER  *
990909*             Neue Ges. 68 COMMERZBANK        XX.08.99          *
000107* VERSION ..  AOP00190                             /GSELLHOFER  *
000107*             Neue Ges. 16, 26 GENERALI       01.01.2000        *
000912**  * AOP00204 Einf neue Produkte Ges. 63 (Sonderbeh. PS 06)
000912**  *                     / GSELLHOFER        13.09.2000   ..   *
      **  * AOP00213    EURO-Umstellung / Mellert   17.11.2000   ..   *
000107** FOP00362   Wegfall 80%-Begrenzung (Teil von AOP00215)        *
000107**                        /Gsellhofer          15.05.2001       *
011010** FOP00371   Wegfall Einheitenvergabe Folge-P. (Ges. 26)       *
011010**                        /Gsellhofer          10.10.2001       *
011113** FOP00375   Wegfall Einheitenvergabe Folge-P. (Ges. 26)       *
011113**                        /Gsellhofer          10.10.2001       *
011113** FOP00379   Fehler bei Einheitenberechnung    (Ges. 25)       *
011113**                        /Gsellhofer          06.02.2002       *
      * VORBELEGEN DER ZUORDNUNG ZU DEN ENTSPRECHENDEN COVER-KONTEN:  *
      *   ÖSTERREICHISCHE PRODUKTE : 5000000 UND 5000020
      *   DEUTSCHE PRODUKTE        : 8000000 UND 8000020
      *
      *---------------------------------------------------------------*
      *    STRUKTURVORBEREITUNG                                       *
      *        HERKZ = A   STRUKTUR AUS ANTRAGSDATEI                  *
      *                    MUSS UM VWF UND VT ERGÄNZT WERDEN          *
      *        HERKZ = B   STRUKTUR AUS BEWEGUNGSDATEI                *
      *                    KANN SO ÜBERNOMMEN WERDEN                  *
      *---------------------------------------------------------------*
g95
       A000 SECTION.
940606     IF   GES = '75' OR
940606          GES = '25'
950805       OR GES = '15'
000107       OR GES = '16'
000107       OR GES = '26'
941006       OR GES = '44'
941006       OR GES = '45'
941006       OR GES = '65'
980904       OR GES = '35'
980904       OR GES = '55'
g95          OR GES = '95'
940606          MOVE  5000000  TO Z-VWF
940606          MOVE  5000020  TO Z-VTF
940606     ELSE
940606          MOVE  8000000  TO Z-VWF
940606          MOVE  8000020  TO Z-VTF
940606     END-IF.

950124*    IF   HERKZ = 'A'
950124*         PERFORM A100.
      *
      *---------------------------------------------------------------*
      *    VORBEREITUNG DER PROVISIONSFELDER ZUR AUFNAHME DER         *
      *      NEU ERMITTELTEN PROVISIONSBETRÄGE                        *
      *---------------------------------------------------------------*
      *
           PERFORM A200.
      *
      *---------------------------------------------------------------*
      *    ZUORDNUNG DER GESELLSCHAFTEN ZU DEN PROVISIONS-            *
      *      BERECHNUNGSBEREICHEN                                     *
      *---------------------------------------------------------------*
      *
940606     IF   GES  =  '25'  PERFORM WA100.
           IF   GES  =  '30'  PERFORM BB100.
           IF   GES  =  '31'  PERFORM CC100.
940311     IF   GES  =  '36'  PERFORM VV100.
           IF   GES  =  '40'  PERFORM DD100.
           IF   GES  =  '41'  PERFORM EE100.
      *
      *---- WIRD PROVISION FÜR GES 42 WIE PROVISION GES 40 ERRECHNET ?
      *                                        10.01.92  KRUMPHOLZ
           IF   GES  =  '42'  PERFORM UU100.
940311     IF   GES  =  '43'  PERFORM WW100.
           IF   GES  =  '50'  PERFORM FF100.
           IF   GES  =  '60'  PERFORM GG100.
           IF   GES  =  '61'  PERFORM HH100.
           IF   GES  =  '62'  PERFORM II100.
           IF   GES  =  '70'  PERFORM JJ100.
           IF   GES  =  '71'  PERFORM KK100.
           IF   GES  =  '66'  PERFORM LL100.
990909     IF   GES  =  '68'  PERFORM COBA-100.
           IF   GES  =  '46'  PERFORM MM100.
           IF   GES  =  '47'  PERFORM NN100.
DGI        IF   GES  =  '49'  PERFORM NN100.
           IF   GES  =  '63'  PERFORM OO100.
           IF   GES  =  '32'  PERFORM PP100.
           IF   GES  =  '21'  PERFORM QQ100.
           IF   GES  =  '82'  PERFORM RR100.
           IF   GES  =  '81'  PERFORM SS100.
           IF   GES  =  '91'  PERFORM TT100.
940606     IF   GES  =  '75'  PERFORM OE100.
950805     IF   GES  =  '15'  PERFORM OE100.
941006     IF   GES  =  '44'  PERFORM OE100.
941006     IF   GES  =  '45'  PERFORM OE100.
941006     IF   GES  =  '65'  PERFORM OE100.
980904     IF   GES  =  '35'  PERFORM SB-OE100.
980904     IF   GES  =  '55'  PERFORM CK-OE100.
000107     IF   GES  =  '16'  PERFORM GLISV-OE100.
000107     IF   GES  =  '26'  PERFORM GLILV-OE100.
g95        IF   GES  =  '95'  PERFORM SO100.
      *
      *---------------------------------------------------------------*
      *    NACHBEREITUNG DER ERMITTLUNGSFELDER = KEINE AUFTEILUNG     *
      *      NACH AUSZAHLUNG UND RUECKSTELLUNG                        *
      *---------------------------------------------------------------*
      *
           PERFORM A300.
           GO TO A999.
      *
      *---------------------------------------------------------------*
      *           E N D E   DER  STEUERUNGSROUTINE                    *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    BEI HERKZ = A WIRD DIE STRUKTUR UM VWF UND VT ERGÄNTZT.    *
      *        INDEX = 1   KONTO VWF                                  *
      *        INDEX = 2   KONTO VTF                                  *
      *        INDEX = 3-N MITARBEITERKONTEN                          *
      *****************************************************************
      *
950124*A100 SECTION.
950124*    MOVE 0 TO WS-J
950124*              WS-WORK.
      *
      *---------------------------------------------------------------*
      *
950124*    PERFORM A110 THRU A129.
950124*     MOVE Z-VTF   TO VBNR(WS-WORK).
950124*     MOVE ZERO    TO STRUKTSTU(WS-WORK).
950124*     MOVE ZERO    TO DIFFPROV(WS-WORK).
950124*     MOVE ZERO    TO PROVBETR(WS-WORK).
      *
950124*    PERFORM A110 THRU A129.
950124*     MOVE Z-VWF   TO VBNR(WS-WORK).
950124*     MOVE ZERO    TO STRUKTSTU(WS-WORK).
950124*     MOVE ZERO    TO DIFFPROV(WS-WORK).
950124*     MOVE ZERO    TO PROVBETR(WS-WORK).
950124*    GO TO A199.
      *
      *---------------------------------------------------------------*
      *
950124*A110.
950124*    COMPUTE STRUKANZ = STRUKANZ + 1.
950124*    MOVE STRUKANZ TO WS-WORK.
950124*A120.
950124*    COMPUTE WS-J  =  WS-WORK - 1.
950124*    IF    WS-J    =  0
950124*          GO TO A129.
950124*    MOVE  STRUKTAB(WS-J) TO STRUKTAB(WS-WORK).
950124*    COMPUTE WS-WORK = WS-WORK - 1.
950124*    GO TO A120.
950124*A129.
950124*    EXIT.
      *---------------------------------------------------------------*
950124*A199.
950124*    EXIT.
950124*    EJECT
      *****************************************************************
      *    LÖSCHEN DER BETRAGSFELDER INNERHALB DER VORHANDENEN        *
      *    PROVISIONSSTRUKTUR                                         *
      *****************************************************************
      *
       A200 SECTION.
           MOVE 0 TO ZW-PROVISION
                     ZW-DIFFPROV.
      *
      *---------------------------------------------------------------*
      *
           PERFORM A210 THRU A299 VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > STRUKANZ.
           GO TO A299.
      *
       A210.
           MOVE ZERO TO PROVBETR (WS-J)
                        PROVRUECK(WS-J)
                        PROVEINH (WS-J).
       A299.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    RÜCKNAHME DER AUFTEILUNG IN AUSZAHLUNG UND RÜCKSTELLUNG    *
      *    IN AUSZAHLUNG. DIE AUFTEILUNG ERFOLGT FÜR SA815.           *
      *    DA BEI VERTEILUNG DER PROVISION EIN VM DURCH DEN VERTRIEBS-*
      *    FOND 8000020 ERSETZT WIRD, WIRD DIE PROVISIONSRUECKSTELLUNG*
      *    IN DEN ENTSPRECHENDEN PROVISIONSBETRAG ADDIERT.            *
      *    10.02.89  GEIGER                                           *
      *****************************************************************
      *
       A300 SECTION.
                PERFORM A310 THRU A319 VARYING WS-J FROM 1 BY 1
                        UNTIL WS-J > STRUKANZ.
           GO TO A399.
      *
      *---------------------------------------------------------------*
      *
       A310.
      *
      *
           IF    (VBNR(WS-J)       = Z-VWF
           OR     VBNR(WS-J)       = Z-VTF)
           AND    WS-J             < 3
                 MOVE PROVRUECK(WS-J) TO PROVBETR(WS-J)
           ELSE
                 ADD  PROVRUECK(WS-J) TO PROVBETR(WS-J).
           MOVE  ZERO TO PROVRUECK(WS-J).
       A319.
           EXIT.
      *---------------------------------------------------------------*
       A399.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 25                       *
      *****************************************************************
      *
940606 WA100 SECTION.
      *-------------GESELLSCHAFT 25  WA-LEBEN ------------------------*
      *---------------------------------------------------------------*
      *
940606 WA110.
940606     MOVE 0 TO WS-J
940606               WS-WORK
940606               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *    KEINE EINHEITEN FÜR PRODARTEN 5 UND 8          *
      *---------------------------------------------------*
940606     IF PRODART       = 5 OR 8
940606        MOVE ZERO TO ZW-EINHEITEN
940606     ELSE
940606        COMPUTE ZW-EINHEITEN ROUNDED =
940606                        ( PROVPFLSU * 80 / 100 ) *
 EURO-*                         TGRUND / 140000
 EURO+                          TGRUND / (K-EUR-EH * K-1000)
990712        IF INFOCODE = 'E'
990712           COMPUTE   ZW-EINHEITEN ROUNDED =
990712*                    ZW-EINHEITEN * 1,2
FOP37X                     ZW-EINHEITEN * 1,25
990712        END-IF
941223        IF ZW-EINHEITEN > K-EH-MAX
941223           MOVE K-EH-MAX TO ZW-EINHEITEN
941223        END-IF
940606     END-IF.
      *
      *---------------------------------------------------*
      *    BERECHNEN DER PRORATASU UND DES GESAMTPROVI-   *
      *    SIONSBETRAGS, DER AN DIE STRUKTUR VERTEILT WIRD*
      *    DIE DIFFERENZ DES PROVISIONSBETRAGES GEHT AN   *
      *    DEN VWF.                                       *
      *---------------------------------------------------*
      *
010515     IF PRODART = 1
010515     OR PRODART = 5
010515        COMPUTE Z-PRORATA-MA   = PRORATASU
010515     ELSE
940606        COMPUTE Z-PRORATA-MA   = PRORATASU *
940606                                 80 / 100
010410     END-IF.
      *
940606     COMPUTE Z-GESPROV-MA ROUNDED  = Z-PRORATA-MA *
940606                             (TVERT + TCOVER) / 1000.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
940606 WA120.
940606     IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
940606                                    ELSE GO TO WA140.
940606     IF   TCOVER          > 0
940606                                    NEXT SENTENCE
940606                      ELSE ADD 1 TO WS-J GO TO WA140.
940606 WA130.
940606     ADD 1 TO WS-J.
940606     MOVE WS-J   TO WS-SAVE-VWF.
940606     MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
940606     COMPUTE PROVRUECK (WS-J) ROUNDED
940606           = PRORATASU    * TCOVER / 1000.
940606     MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
940606 WA140.
940606     ADD 1 TO WS-J.
940606     IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
940606                                    ELSE GO TO WA160.
940606     IF   TVERT           > 0
940606                                    NEXT SENTENCE
940606                      ELSE ADD 1 TO WS-J GO TO WA160.
940606 WA150.
940606     MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
940606     MOVE WS-J   TO WS-SAVE.
      *
940606     ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
940606 WA160.
940606     PERFORM WA170 THRU WA189 VARYING WS-J  FROM WS-J BY 1
940606                              UNTIL   WS-J > STRUKANZ.
940606     GO TO WA190.
      *
940606 WA170.
940606     MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
940606     IF   WS-DIFFPROV4 = 2
940606          MOVE ZERO TO PROVBETR  (WS-J)
940606          MOVE ZERO TO PROVRUECK (WS-J)
940606          GO TO WA180.
940606     IF   WS-DIFFPROV4 = 4
940606          SUBTRACT 0,4 FROM  WS-DIFFPROV.
940606     IF   WS-DIFFPROV4 = 6
940606          SUBTRACT 0,6 FROM  WS-DIFFPROV.
940606     IF   WS-DIFFPROV4 = 7
940606          SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
940606     COMPUTE PROVRUECK (WS-J) ROUNDED =
940606     Z-PRORATA-MA * TGRUND * WS-DIFFPROV * TRUECK
940606     / 10000000.
940606     ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
940606     COMPUTE PROVBETR (WS-J) ROUNDED =
940606     Z-PRORATA-MA * TGRUND * WS-DIFFPROV
940606     / 100000 - PROVRUECK (WS-J).
940606     ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
940606     ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
940606     IF PRODART       = 5 OR 6 OR 7 OR 8
940606        NEXT SENTENCE
940606     ELSE
940606        GO TO WA180.

940606     MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
940606     IF WS-DIFFPROV4  = 4
940606              MOVE Z-VTF  TO VBNR (WS-J).
      *
940606 WA180.
940606     MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
940606     MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
940606     IF   WS-DIFFPROV4 = 4
940606          MOVE ZERO    TO PROVEINH (WS-J).
940606     IF   WS-DIFFPROV4 = 7
940606          MOVE Z-VTF   TO VBNR     (WS-J).
      *
940606 WA189.
940606     EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
940606 WA190.
940606     IF   WS-SAVE = 0
940606          GO TO WA192.
940606     COMPUTE PROVRUECK (WS-SAVE) = PRORATASU  *
940606                                   (TVERT + TCOVER) / 1000 -
                                         ZW-PROVISION.

940606     GO TO WA199.

      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VWF              *
      *    DER VERWALTUNGSFOND ERHÄLT DIE PROVISIONS-     *
      *    DIFFERENZ AUS DER MAXIMIERUNG, FALLT TVERT = 0 *
      *---------------------------------------------------*
940606 WA192.
940606     COMPUTE PROVRUECK (WS-SAVE-VWF) ROUNDED =
940606                           PROVRUECK (WS-SAVE-VWF) +
940606                         ( PRORATASU * (TVERT + TCOVER) / 1000 -
940606                                   ZW-PROVISION ).

       WA199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 30                       *
      *****************************************************************
      *
       BB100 SECTION.
      *-------------GESELLSCHAFT 30 ----------------------------------*
      *---------------------------------------------------------------*
      *
       BB110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.

      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       BB120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO BB140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO BB140.
       BB130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       BB140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO BB160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO BB160.
       BB150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       BB160.
           PERFORM BB170 THRU BB189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO BB190.
      *
       BB170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO BB180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
       BB180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR     (WS-J).
      *
       BB189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       BB190.
           IF   WS-SAVE = 0
                GO TO BB199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       BB199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 31                       *
      *****************************************************************
       CC100 SECTION.
      *-------------GESELLSCHAFT 31 ----------------------------------*
      *---------------------------------------------------------------*
      *
       CC110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE
                     WS-MSUMME.
      *
      *---------------------------------------------------*
      *    ERMITTELN ABSCHLUSSEINHEITEN                   *
      *---------------------------------------------------*
           IF   (PRODART  = '5' OR '6' OR '7' OR '8')
                MOVE ZERO TO ZW-EINHEITEN
           ELSE
              IF PRODSCHL = 10
                 IF VSNR11 < 2
930826              MOVE 5,0 TO ZW-EINHEITEN
930826           ELSE
930826              IF VSNR11 = 2
930826                 IF VSNR910 < 04
                          MOVE 5,0 TO ZW-EINHEITEN
                       ELSE
                          MOVE 4,0 TO ZW-EINHEITEN
                       END-IF
                    END-IF
930826              IF VSNR11 > 2
                       MOVE 4,0 TO ZW-EINHEITEN
930826              END-IF
                 END-IF
              ELSE
                 IF VSNR11 < 2
930826              MOVE 2,0 TO ZW-EINHEITEN
930826           ELSE
930826              IF VSNR11 = 2
930826                 IF VSNR910 < 04
                          MOVE 2,0 TO ZW-EINHEITEN
                       ELSE
                          MOVE 1,0 TO ZW-EINHEITEN
                       END-IF
                    END-IF
930826              IF VSNR11 > 2
                       MOVE 1,0 TO ZW-EINHEITEN
930826              END-IF
                 END-IF
              END-IF
           END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       CC120.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VWF
           AND  TCOVER          > 0
                MOVE ZW-EINHEITEN TO PROVEINH (WS-J)
                MOVE TCOVER       TO PROVRUECK (WS-J)
           ELSE SUBTRACT 1 FROM WS-J.
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       CC140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)        = Z-VTF
           AND  TVERT             > 0
                MOVE ZW-EINHEITEN TO PROVEINH (WS-J)
                MOVE WS-J         TO WS-SAVE
           ELSE SUBTRACT 1 FROM WS-J.
      *
      *
      *---------------------------------------------------*
      *    ERMITTELN SUMME MITARBEITERPROVISIONEN         *
      *  ANTEILIGE PROVISIONEN SIND Z.ZT. GESPERRT        *
      *---------------------------------------------------*
      *    PERFORM CC115 THRU CC119 VARYING WS-WORK FROM 1 BY 1
      *                             UNTIL   WS-WORK > STRUKANZ.
      *    COMPUTE ZW-FAKTOR ROUNDED = TGRUND / WS-MSUMME.
      *    MOVE 0 TO WS-WORK.
      *    GO TO CC160.
      *CC115.
      *    IF   VBNR(WS-WORK) = Z-VWF
      *    OR   VBNR(WS-WORK) = Z-VTF
      *         GO TO CC119.
      *    COMPUTE WS-MSUMME = DIFFPROV(WS-WORK) + WS-MSUMME.
      *    MOVE DIFFPROV (WS-WORK) TO WS-DIFFPROV.
      *    IF   WS-DIFFPROV4 = 2
      *         SUBTRACT 0,2 FROM  WS-MSUMME.
      *    IF   WS-DIFFPROV4 = 4
      *         SUBTRACT 0,4 FROM  WS-MSUMME.
      *    IF   WS-DIFFPROV4 = 6
      *         SUBTRACT 0,6 FROM  WS-MSUMME.
      *CC119.
      *    EXIT.
      *
      *---------------------------------------------------*
      *    ERMITTELN       MITARBEITERPROVISIONEN         *
      *---------------------------------------------------*
      *
       CC160.
      *
           ADD 1 TO WS-J.
           PERFORM CC170 THRU CC189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO CC190.
      *
       CC170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO CC180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
             TGRUND    * WS-DIFFPROV * TRUECK / 10000.
      *    COMPUTE PROVRUECK (WS-J) ROUNDED =
      *      PROVRUECK (WS-J) * 100 / 114.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
             TGRUND    * WS-DIFFPROV / 100 - PROVRUECK(WS-J).
      *    COMPUTE PROVBETR (WS-J) ROUNDED =
      *      PROVBETR(WS-J) * 100 / 114.
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
       CC180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF TO VBNR       (WS-J).
      *
       CC189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       CC190.
           IF   WS-SAVE = 0
                GO TO CC199.
           COMPUTE PROVRUECK (WS-SAVE) =
                                     TVERT  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       CC199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 40                       *
      *****************************************************************
      *
       DD100 SECTION.
      *-------------GESELLSCHAFT 40 ----------------------------------*
      *
       DD110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       DD120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO DD140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO DD140.
       DD130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       DD140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO DD160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO DD160.
       DD150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       DD160.
           PERFORM DD170 THRU DD189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO DD190.
      *
       DD170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO DD180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO DD180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF TO VBNR (WS-J).
      *
       DD180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       DD189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       DD190.
           IF   WS-SAVE = 0
                GO TO DD199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
      ***  COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
      ***                      / 1000  - ZW-PROVISION.
      **   IF WS-PROV < ZW-PROVISION
      **      EXHIBIT NAMED WS-PROV ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       DD199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 41                       *
      *****************************************************************
      *
       EE100 SECTION.
      *-------------GESELLSCHAFT 41 ----------------------------------*
      *
       EE110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       EE120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO EE140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO EE140.
       EE130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       EE140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO EE160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO EE160.
       EE150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       EE160.
           PERFORM EE170 THRU EE189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO EE190.
      *
       EE170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO EE180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO EE180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       EE180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       EE189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       EE190.
           IF   WS-SAVE = 0
                GO TO EE199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
      ***  COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
      ***                      / 1000  - ZW-PROVISION.
      **   IF WS-PROV < ZW-PROVISION
      **      EXHIBIT NAMED WS-PROV ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       EE199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 50                       *
      *****************************************************************
      *
       FF100 SECTION.
      *-------------GESELLSCHAFT 50 ----------------------------------*
      *---------------------------------------------------------------*
      *
       FF110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.

      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       FF120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO FF140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO FF140.
       FF130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       FF140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO FF160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO FF160.
       FF150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       FF160.
           PERFORM FF170 THRU FF189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO FF190.
      *
       FF170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO FF180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
       FF180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       FF189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       FF190.
           IF   WS-SAVE = 0
                GO TO FF199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       FF199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 60                       *
      *****************************************************************
      *
       GG100 SECTION.
      *-------------GESELLSCHAFT 60 ----------------------------------*
      *
       GG110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       GG120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO GG140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO GG140.
       GG130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       GG140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO GG160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO GG160.
       GG150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       GG160.
           PERFORM GG170 THRU GG189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO GG190.
      *
       GG170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO GG180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
       GG180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       GG189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       GG190.
           IF   WS-SAVE = 0
                GO TO GG199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       GG199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 61                       *
      *****************************************************************
      *
       HH100 SECTION.
      *-------------GESELLSCHAFT 61 ----------------------------------*
      *
       HH110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       HH120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO HH140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO HH140.
       HH130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       HH140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO HH160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO HH160.
       HH150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       HH160.
           PERFORM HH170 THRU HH189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO HH190.
      *
       HH170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO HH180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
       HH180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       HH189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       HH190.
           IF   WS-SAVE = 0
                GO TO HH199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       HH199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 62                       *
      *****************************************************************
      *
       II100 SECTION.
      *-------------GESELLSCHAFT 62 ----------------------------------*
      *
       II110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       II120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO II140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO II140.
       II130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       II140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO II160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO II160.
       II150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       II160.
           PERFORM II170 THRU II189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO II190.
      *
       II170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO II180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO II180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       II180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       II189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       II190.
           IF   WS-SAVE = 0
                GO TO II199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       II199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 70                       *
      *****************************************************************
      *
       JJ100 SECTION.
      *-------------GESELLSCHAFT 70 ----------------------------------*
      *
       JJ110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       JJ120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO JJ140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO JJ140.
       JJ130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       JJ140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO JJ160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO JJ160.
       JJ150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       JJ160.
           IF TGRUND = ZERO
              GO TO JJ190.
           PERFORM JJ170 THRU JJ189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO JJ190.
      *
       JJ170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO JJ180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
       JJ180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       JJ189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       JJ190.
           IF   WS-SAVE = 0
                GO TO JJ199.
      ***  COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
      ***                      / 1000  - ZW-PROVISION.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       JJ199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 71                       *
      *****************************************************************
      *
       KK100 SECTION.
      *-------------GESELLSCHAFT 71 ----------------------------------*
      *
       KK110.
           MOVE 0 TO WS-J
                     WS-WORK
                     WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       KK120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO KK140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO KK140.
       KK130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       KK140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO KK160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO KK160.
       KK150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       KK160.
           PERFORM KK170 THRU KK189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO KK190.
      *
       KK170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO KK180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
      *    COMPUTE PROVRUECK (WS-J) ROUNDED =
      *    PROVRUECK (WS-J) * 100 / 114.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
      *    COMPUTE PROVBETR (WS-J) ROUNDED =
      *    PROVBETR (WS-J) * 100 / 114.
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
       KK180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       KK189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       KK190.
           IF   WS-SAVE = 0
                GO TO KK199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000.
           COMPUTE PROVRUECK (WS-SAVE) = PROVRUECK (WS-SAVE)
                               - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       KK199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 66                       *
      *****************************************************************
      *
       LL100 SECTION.
      *-------------GESELLSCHAFT 66 ----------------------------------*
      *
       LL110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       LL120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO LL140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO LL140.
       LL130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       LL140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO LL160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO LL160.
       LL150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       LL160.
           PERFORM LL170 THRU LL189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO LL190.
      *
       LL170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO LL180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *----------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN         *
      *----------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO LL180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       LL180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       LL189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       LL190.
           IF   WS-SAVE = 0
                GO TO LL199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
      **   COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
      **                       / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       LL199.
           EXIT.
      *---------------------------------------------------------------*
990909*****************************************************************
990909*    PROVISIONSERMITTLUNG GESELLSCHAFT 66                       *
990909*****************************************************************
990909*
990909 COBA-100 SECTION.
990909*-------------GESELLSCHAFT 66 ----------------------------------*
990909*
990909 COBA-110.
990909     MOVE GESPROVBETR TO WS-PROV
990909     MOVE 0           TO WS-J
990909                         WS-WORK
990909                         WS-SAVE.
990909*---------------------------------------------------*
990909*    EINHEITENBERECHNUNG                            *
990909*---------------------------------------------------*
990909     IF PRODSCHL = 06
990909     OR PRODSCHL = 60
990909     OR PRODSCHL = 61
990909        COMPUTE ZW-EINHEITEN ROUNDED =
 EURO-*                          TGRUND / 20
 EURO+                           TGRUND / K-EUR-EH
990909     ELSE
990909        COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                          TGRUND / 20000
 EURO+                           TGRUND / (K-EUR-EH * K-1000)
990909
990909     END-IF.
990909
990909     IF ZW-EINHEITEN > K-EH-MAX
990909        MOVE K-EH-MAX TO ZW-EINHEITEN
990909     END-IF.
990909*
990909*---------------------------------------------------*
990909*    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
990909*---------------------------------------------------*
990909*
990909 COBA-120.
990909     IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
990909                                    ELSE GO TO COBA-140.
990909     IF   TCOVER          > 0
990909                                    NEXT SENTENCE
990909                      ELSE ADD 1 TO WS-J GO TO COBA-140.
990909 COBA-130.
990909     ADD 1 TO WS-J.
990909     MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
990909     IF PRODSCHL = 06
990909     OR PRODSCHL = 60
990909     OR PRODSCHL = 61
990909        COMPUTE PROVRUECK (WS-J) = TCOVER
990909     ELSE
990909        COMPUTE PROVRUECK (WS-J) ROUNDED
990909              = PRORATASU * TCOVER / 1000
990909     END-IF.
990909     MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
990909*
990909*---------------------------------------------------*
990909*    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
990909*---------------------------------------------------*
990909*
990909 COBA-140.
990909     ADD 1 TO WS-J.
990909     IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
990909                                    ELSE GO TO COBA-160.
990909     IF   TVERT           > 0
990909                                    NEXT SENTENCE
990909                      ELSE ADD 1 TO WS-J GO TO COBA-160.
990909 COBA-150.
990909     MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
990909     MOVE WS-J   TO WS-SAVE.
990909*
990909     ADD 1 TO WS-J.
990909*
990909*---------------------------------------------------*
990909*    PROVISIONSBERECHNUNG                           *
990909*    FUER DIE VB-STRUKTUR                           *
990909*---------------------------------------------------*
990909*
990909 COBA-160.
990909     PERFORM COBA-170 THRU COBA-189 VARYING WS-J  FROM WS-J BY 1
990909                              UNTIL   WS-J > STRUKANZ.
990909     GO TO COBA-190.
990909*
990909 COBA-170.
990909     MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
990909     IF   WS-DIFFPROV4 = 2
990909          MOVE ZERO TO PROVBETR  (WS-J)
990909          MOVE ZERO TO PROVRUECK (WS-J)
990909          GO TO COBA-180.
990909     IF   WS-DIFFPROV4 = 4
990909          SUBTRACT 0,4 FROM  WS-DIFFPROV.
990909     IF   WS-DIFFPROV4 = 6
990909          SUBTRACT 0,6 FROM  WS-DIFFPROV.
990909     IF   WS-DIFFPROV4 = 7
990909          SUBTRACT 0,7 FROM  WS-DIFFPROV.
990909*
990909     COMPUTE PROVRUECK (WS-J) ROUNDED =
990909     WS-PROV * WS-DIFFPROV * TRUECK / 10000.
990909     ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
990909*
990909     IF PRODSCHL = 06
990909     OR PRODSCHL = 60
990909     OR PRODSCHL = 61
990909        COMPUTE PROVBETR (WS-J) ROUNDED =
990909        TGRUND * WS-DIFFPROV / 100 - PROVRUECK (WS-J)
990909     ELSE
990909        COMPUTE PROVBETR (WS-J) ROUNDED =
990909        PRORATASU * TGRUND * WS-DIFFPROV
990909        / 100000 - PROVRUECK (WS-J)
990909     END-IF
990909     ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
990909*
990909     ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
990909*
990909*----------------------------------------------*
990909*    UMSETZEN VM FUER FOLGEPROVISIONEN         *
990909*----------------------------------------------*
990909*
990909     IF PRODART       = 5 OR 6 OR 7 OR 8
990909        NEXT SENTENCE
990909     ELSE
990909        GO TO COBA-180.
990909*
990909     MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
990909     IF WS-DIFFPROV4  = 4
990909              MOVE Z-VTF   TO VBNR (WS-J).
990909*
990909 COBA-180.
990909     MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
990909     MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
990909     IF   WS-DIFFPROV4 = 4
990909          MOVE ZERO    TO PROVEINH (WS-J).
990909     IF   WS-DIFFPROV4 = 7
990909          MOVE Z-VTF   TO VBNR (WS-J).
990909*
990909 COBA-189.
990909     EXIT.
990909*---------------------------------------------------*
990909*    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
990909*---------------------------------------------------*
990909 COBA-190.
990909     IF   WS-SAVE = 0
990909          GO TO COBA-199.
990909     COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
990909**   COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
990909**                       / 1000  - ZW-PROVISION.
990909     MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
990909*
990909 COBA-199.
990909     EXIT.
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 46                       *
      *****************************************************************
      *
       MM100 SECTION.
      *-------------GESELLSCHAFT 46 ----------------------------------*
      *
       MM110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       MM120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO MM140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO MM140.
       MM130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       MM140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO MM160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO MM160.
       MM150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       MM160.
           PERFORM MM170 THRU MM189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO MM190.
      *
       MM170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO MM180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *----------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN         *
      *----------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO MM180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       MM180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       MM189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       MM190.
           IF   WS-SAVE = 0
                GO TO MM199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
      ***  COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
      ***                      / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       MM199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 47                       *
      *****************************************************************
      *
       NN100 SECTION.
      *-------------GESELLSCHAFT 47 ----------------------------------*
      *
       NN110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       NN120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO NN140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO NN140.
       NN130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       NN140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO NN160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO NN160.
       NN150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       NN160.
           PERFORM NN170 THRU NN189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO NN190.
      *
       NN170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO NN180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *----------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN         *
      *----------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO NN180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       NN180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       NN189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       NN190.
           IF   WS-SAVE = 0
                GO TO NN199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       NN199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 63                       *
      *****************************************************************
      *
       OO100 SECTION.
      *-------------GESELLSCHAFT 63 ----------------------------------*
      *
       OO110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
000912*    COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
000912*                       TGRUND / 20000.
000912     IF PRODSCHL = 06
000912        COMPUTE ZW-EINHEITEN ROUNDED =
 EURO-*                          TGRUND / 20
 EURO+                           TGRUND / K-EUR-EH
000912     ELSE
000912        COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                          TGRUND / 20000
 EURO+                           TGRUND / (K-EUR-EH * K-1000)
000912     END-IF

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       OO120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO OO140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO OO140.
       OO130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
000912     IF PRODSCHL = 06
000912        COMPUTE PROVRUECK (WS-J) = TCOVER
000912     ELSE
000912        COMPUTE PROVRUECK (WS-J) ROUNDED
000912             = PRORATASU * TCOVER / 1000
000912     END-IF
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       OO140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO OO160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO OO160.
       OO150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       OO160.
           PERFORM OO170 THRU OO189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO OO190.
      *
       OO170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO OO180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
000912     WS-PROV * WS-DIFFPROV * TRUECK / 10000
000912*    PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
000912*    / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
000912     IF PRODSCHL = 06
000912        COMPUTE PROVBETR (WS-J) ROUNDED =
000912        TGRUND * WS-DIFFPROV / 100 - PROVRUECK (WS-J)
000912     ELSE
000912        COMPUTE PROVBETR (WS-J) ROUNDED =
000912        PRORATASU * TGRUND * WS-DIFFPROV
000912        / 100000 - PROVRUECK (WS-J)
000912     END-IF.
000912*    COMPUTE PROVBETR (WS-J) ROUNDED =
000912*    PRORATASU * TGRUND * WS-DIFFPROV
000912*    / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO OO180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       OO180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       OO189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       OO190.
           IF   WS-SAVE = 0
                GO TO OO199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       OO199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 32                       *
      *****************************************************************
      *
       PP100 SECTION.
      *-------------GESELLSCHAFT 32 ----------------------------------*
      *
       PP110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).
      *    COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU * TGRUND *
      *                       0,87 / 20000.

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       PP120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO PP140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO PP140.
       PP130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       PP140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO PP160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO PP160.
       PP150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       PP160.
           PERFORM PP170 THRU PP189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO PP190.
      *
       PP170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO PP180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
      *    PRORATASU * TGRUND * 0,87 * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
      *    PRORATASU * TGRUND * 0,87 * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO PP180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       PP180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       PP189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       PP190.
           IF   WS-SAVE = 0
                GO TO PP199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       PP199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 21                       *
      *****************************************************************
      *
       QQ100 SECTION.
      *-------------GESELLSCHAFT 21 ----------------------------------*
      *
       QQ110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU * TGRUND
 EURO-*                            / 20000.
 EURO+                             / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       QQ120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO QQ140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO QQ140.
       QQ130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       QQ140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO QQ160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO QQ160.
       QQ150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       QQ160.
           PERFORM QQ170 THRU QQ189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO QQ190.
      *
       QQ170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO QQ180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO QQ180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       QQ180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       QQ189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       QQ190.
           IF   WS-SAVE = 0
                GO TO QQ199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       QQ199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *
      *
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 82                       *
      *****************************************************************
      *
       RR100 SECTION.
      *-------------GESELLSCHAFT 82 ----------------------------------*
      *
       RR110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU * TGRUND
 EURO-*                            / 20000.
 EURO+                             / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       RR120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO RR140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO RR140.
       RR130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       RR140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO RR160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO RR160.
       RR150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       RR160.
           PERFORM RR170 THRU RR189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO RR190.
      *
       RR170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO RR180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO RR180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       RR180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       RR189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       RR190.
           IF   WS-SAVE = 0
                GO TO RR199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       RR199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *
      *
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 81                       *
      *****************************************************************
      *
       SS100 SECTION.
      *-------------GESELLSCHAFT 81 ----------------------------------*
      *
       SS110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN = PROVPFLSU * TGRUND
 EURO-*                            / 20000.
 EURO+                             / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       SS120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO SS140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO SS140.
       SS130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J)
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       SS140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO SS160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO SS160.
       SS150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       SS160.
           PERFORM SS170 THRU SS189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO SS190.
      *
       SS170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO SS180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO SS180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF   TO VBNR (WS-J).
      *
       SS180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       SS189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       SS190.
           IF   WS-SAVE = 0
                GO TO SS199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       SS199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *
      *
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 91                       *
      *****************************************************************
      *
       TT100 SECTION.
      *-------------GESELLSCHAFT 91 ----------------------------------*
      *
       TT110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE ZERO        TO WS-J
                               WS-WORK
                               WS-SAVE
                               ZW-PROVISION.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *    GRUNDPROV IST DER GRUNDPROVISIONSSATZ, DER     *
      *    IM UU45 EINGEGEBEN WURDE.                      *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN = PROVPFLSU * GRUNDPROV
 EURO-*                            / 2000.
 EURO+                             / (K-EUR-EH * K-100).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       TT120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO TT140.
       TT130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       TT140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO TT160.
       TT150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J         TO WS-SAVE.
           ADD 1             TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       TT160.
           PERFORM TT170 THRU TT189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO TT190.
      *
       TT170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO TT180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * GRUNDPROV * WS-DIFFPROV
           / 10000.
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO TT180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
              MOVE Z-VTF   TO VBNR (WS-J).
      *
       TT180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       TT189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *---------------------------------------------------*
       TT190.
           IF   WS-SAVE = 0
                GO TO TT199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       TT199.
           EXIT.
      *
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 42                       *
      *****************************************************************
      *
       UU100 SECTION.
      *-------------GESELLSCHAFT 42 ----------------------------------*
      *
       UU110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       UU120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO UU140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO UU140.
       UU130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       UU140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO UU160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO UU160.
       UU150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       UU160.
           PERFORM UU170 THRU UU189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO UU190.
      *
       UU170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO UU180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO UU180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       UU180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       UU189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       UU190.
           IF   WS-SAVE = 0
                GO TO UU199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       UU199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 36                       *
      *****************************************************************
      *
       VV100 SECTION.
      *-------------GESELLSCHAFT 36 ----------------------------------*
      *
       VV110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       VV120.
           IF   VBNR(1)         = 8000000 NEXT SENTENCE
                                          ELSE GO TO VV140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO VV140.
       VV130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       VV140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = 8000020 NEXT SENTENCE
                                          ELSE GO TO VV160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO VV160.
       VV150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       VV160.
           PERFORM VV170 THRU VV189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO VV190.
      *
       VV170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO VV180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO VV180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE 8000020 TO VBNR (WS-J).
      *
       VV180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE 8000020 TO VBNR (WS-J).
      *
       VV189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       VV190.
           IF   WS-SAVE = 0
                GO TO VV199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       VV199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 43                       *
      *****************************************************************
      *
       WW100 SECTION.
      *-------------GESELLSCHAFT 43 ----------------------------------*
      *
       WW110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 20000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       WW120.
           IF   VBNR(1)         = 8000000 NEXT SENTENCE
                                          ELSE GO TO WW140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO WW140.
       WW130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       WW140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = 8000020 NEXT SENTENCE
                                          ELSE GO TO WW160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO WW160.
       WW150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       WW160.
           PERFORM WW170 THRU WW189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO WW190.
      *
       WW170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO WW180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO WW180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE 8000020 TO VBNR (WS-J).
      *
       WW180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE 8000020 TO VBNR (WS-J).
      *
       WW189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       WW190.
           IF   WS-SAVE = 0
                GO TO WW199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
       WW199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT

      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 75 S-BAUSPARKASSE  UND   *
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 15 WA-VERSICHERUNG       *
      *           A V B G                                             *
      *****************************************************************
      *
940606 OE100 SECTION.
      *-------------GESELLSCHAFT 75 --S-BAUSP-------------------------*
      *-------------GESELLSCHAFT 15 --WA-VERS-------------------------*
      *-------------GESELLSCHAFT 44 --DIT ÖST-------------------------*
      *
       OE110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 140000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

941223     IF ZW-EINHEITEN > K-EH-MAX
941223        MOVE K-EH-MAX TO ZW-EINHEITEN
941223     END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       OE120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO OE140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO OE140.
       OE130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       OE140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO OE160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO OE160.
       OE150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       OE160.
           PERFORM OE170 THRU OE189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO OE190.
      *
       OE170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO OE180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO OE180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       OE180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       OE189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       OE190.
           IF   WS-SAVE = 0
                GO TO OE199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
940606 OE199.
           EXIT.

      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 55 CKV                   *
      *           A V B G                                             *
      *****************************************************************
      *
980904 CK-OE100 SECTION.
      *-------------GESELLSCHAFT 55 --CKV-OE--------------------------*
      *
       CK-OE110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 140000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

           IF ZW-EINHEITEN > K-EH-MAX
              MOVE K-EH-MAX TO ZW-EINHEITEN
           END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       CK-OE120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO CK-OE140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO CK-OE140.
       CK-OE130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000.
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       CK-OE140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO CK-OE160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO CK-OE160.
       CK-OE150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       CK-OE160.
           PERFORM CK-OE170 THRU CK-OE189 VARYING WS-J FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO CK-OE190.
      *
       CK-OE170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO CK-OE180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV * TRUECK
           / 10000000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           PRORATASU * TGRUND * WS-DIFFPROV
           / 100000 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO CK-OE180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       CK-OE180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       CK-OE189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       CK-OE190.
           IF   WS-SAVE = 0
                GO TO CK-OE199.
           COMPUTE PROVRUECK (WS-SAVE) = PRORATASU * (TVERT + TCOVER)
                               / 1000  - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
980904 CK-OE199.
           EXIT.
      *---------------------------------------------------------------*
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 35 SCHOELLERBANK         *
      *           A V B G                                             *
      *****************************************************************
      *
980904 SB-OE100 SECTION.
      *-------------GESELLSCHAFT 55 --CKV-OE--------------------------*
      *
       SB-OE110.
           MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 140000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

           IF ZW-EINHEITEN > K-EH-MAX
              MOVE K-EH-MAX TO ZW-EINHEITEN
           END-IF.
      *
           IF PRODSCHL = 10
              COMPUTE WS-PROV = TGRUND
           ELSE
              COMPUTE WS-PROV = PRORATASU * TGRUND / 1000
           END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       SB-OE120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO SB-OE140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO SB-OE140.
       SB-OE130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           IF PRODSCHL = 10
              COMPUTE PROVRUECK (WS-J) = TCOVER
           ELSE
              COMPUTE PROVRUECK (WS-J) ROUNDED
                    = PRORATASU * TCOVER / 1000
           END-IF
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       SB-OE140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO SB-OE160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO SB-OE160.
       SB-OE150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       SB-OE160.
           PERFORM SB-OE170 THRU SB-OE189 VARYING WS-J FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO SB-OE190.
      *
       SB-OE170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO SB-OE180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
           WS-PROV * WS-DIFFPROV * TRUECK
           / 10000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
           WS-PROV * WS-DIFFPROV
           / 100 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO SB-OE180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       SB-OE180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       SB-OE189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       SB-OE190.
           IF   WS-SAVE = 0
                GO TO SB-OE199.
           IF PRODSCHL = 10
              COMPUTE PROVRUECK (WS-SAVE) = TVERT + TCOVER
                                - ZW-PROVISION
           ELSE
              COMPUTE PROVRUECK (WS-SAVE) =
                      PRORATASU * (TVERT + TCOVER) / 1000
                                -  ZW-PROVISION
           END-IF
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
980904 SB-OE199.
           EXIT.
      *---------------------------------------------------------------*
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 16 GENERALI-SACH         *
      *           A V B G                                             *
      *****************************************************************
      *
000107 GLISV-OE100 SECTION.
      *-------------GESELLSCHAFT 55 --CKV-OE--------------------------*
      *
       GLISV-OE110.
000107     MOVE GESPROVBETR TO WS-PROV
000107     MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
000107     COMPUTE WS-GRUNDPROV = PRORATASU * TGRUND / 1000
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                       TGRUND / 140000.
 EURO+                        TGRUND / (K-EUR-EH * K-1000).

           IF ZW-EINHEITEN > K-EH-MAX
              MOVE K-EH-MAX TO ZW-EINHEITEN
           END-IF.
      *
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       GLISV-OE120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO GLISV-OE140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO GLISV-OE140.
       GLISV-OE130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       GLISV-OE140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO GLISV-OE160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO GLISV-OE160.
       GLISV-OE150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       GLISV-OE160.
           PERFORM GLISV-OE170 THRU
                   GLISV-OE189 VARYING WS-J FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO GLISV-OE190.
      *
       GLISV-OE170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO GLISV-OE180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
000107     WS-GRUNDPROV * WS-DIFFPROV * TRUECK
           / 10000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
000107     WS-GRUNDPROV * WS-DIFFPROV
           / 100 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO GLISV-OE180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       GLISV-OE180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       GLISV-OE189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       GLISV-OE190.
           IF   WS-SAVE = 0
                GO TO GLISV-OE199.
000107     COMPUTE PROVRUECK (WS-SAVE) =
000107                WS-PROV  -  ZW-PROVISION
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
980904 GLISV-OE199.
           EXIT.
      *****************************************************************
      *    PROVISIONSERMITTLUNG GESELLSCHAFT 26 GENERALI-LEBEN        *
      *           A V B G                                             *
      *****************************************************************
      *
000107 GLILV-OE100 SECTION.
      *-------------GESELLSCHAFT 55 --CKV-OE--------------------------*
      *
       GLILV-OE110.
000107     MOVE GESPROVBETR TO WS-PROV
000107     MOVE 0           TO WS-J
                               WS-WORK
                               WS-SAVE.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *---------------------------------------------------*
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *    KEINE EINHEITEN FÜR PRODARTEN 5 UND 8          *
      *---------------------------------------------------*
FOP371     IF PRODART       = 5 OR 6 OR 7 OR 8
FOP371        MOVE ZERO TO ZW-EINHEITEN
FOP371     ELSE
FOP371        COMPUTE ZW-EINHEITEN ROUNDED =
FOP371*                       ( PROVPFLSU * 80 / 100 ) *
FOP375                        ( PROVPFLSU *
 EURO-*                         TGRUND / 140000 )
 EURO+                          TGRUND / (K-EUR-EH * K-1000))
FOP371     END-IF

           IF ZW-EINHEITEN > K-EH-MAX
              MOVE K-EH-MAX TO ZW-EINHEITEN
           END-IF.
      *
000107     COMPUTE WS-GRUNDPROV = PRORATASU * TGRUND / 1000.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       GLILV-OE120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO GLILV-OE140.
           IF   TCOVER          > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO GLILV-OE140.
       GLILV-OE130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           COMPUTE PROVRUECK (WS-J) ROUNDED
                 = PRORATASU * TCOVER / 1000
           MOVE PROVRUECK (WS-J) TO ZW-PROVISION.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       GLILV-OE140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO GLILV-OE160.
           IF   TVERT           > 0
                                          NEXT SENTENCE
                            ELSE ADD 1 TO WS-J GO TO GLILV-OE160.
       GLILV-OE150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       GLILV-OE160.
           PERFORM GLILV-OE170 THRU
                   GLILV-OE189 VARYING WS-J FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO GLILV-OE190.
      *
       GLILV-OE170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO GLILV-OE180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVRUECK (WS-J) ROUNDED =
000107     WS-GRUNDPROV * WS-DIFFPROV * TRUECK
           / 10000.
           ADD  PROVRUECK (WS-J) TO ZW-PROVISION.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
000107     WS-GRUNDPROV * WS-DIFFPROV
           / 100 - PROVRUECK (WS-J).
           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
      *
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO GLILV-OE180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       GLILV-OE180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       GLILV-OE189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       GLILV-OE190.
           IF   WS-SAVE = 0
                GO TO GLILV-OE199.
000107     COMPUTE PROVRUECK (WS-SAVE) =
000107                  WS-PROV  -  ZW-PROVISION
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
980904 GLILV-OE199.
           EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    PROVISIONSERMITTLUNG SONDERGESELLSCHAFT 95  AVBG           *
      *           A V B G                                             *
      *****************************************************************
      *
g95    SO100 SECTION.
      *-------------GESELLSCHAFT 95 --sondergesellschaft avbg---------*
      *
       SO110.
           MOVE GESPROVBETR TO WS-PROV.
           MOVE zero        TO WS-J
                               WS-WORK
                               WS-SAVE
                               ZW-PROVISION.
      *---------------------------------------------------*
      *    EINHEITENBERECHNUNG                            *
      *    GRUNDPROV IST DER GRUNDPROVISIONSSATZ, DER     *
      *    IN UU45 EINGEGEBEN WURDE                       *
      *---------------------------------------------------*
           COMPUTE ZW-EINHEITEN ROUNDED = PROVPFLSU *
 EURO-*                         GRUNDPROV  / 14000.
 EURO+                          GRUNDPROV  / (K-EUR-EH * K-100).

           IF ZW-EINHEITEN > K-EH-MAX
              MOVE K-EH-MAX TO ZW-EINHEITEN
           END-IF.
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERWALTUNGSFONDS      *
      *---------------------------------------------------*
      *
       SO120.
           IF   VBNR(1)         = Z-VWF   NEXT SENTENCE
                                          ELSE GO TO SO140.
       SO130.
           ADD 1 TO WS-J.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
      *
      *---------------------------------------------------*
      *    TABELLENEINTRAG FUER DEN VERTRIEBSFONDS        *
      *---------------------------------------------------*
      *
       SO140.
           ADD 1 TO WS-J.
           IF   VBNR(WS-J)      = Z-VTF   NEXT SENTENCE
                                          ELSE GO TO SO160.
       SO150.
           MOVE ZW-EINHEITEN TO PROVEINH(WS-J).
           MOVE WS-J   TO WS-SAVE.
      *
           ADD 1 TO WS-J.
      *
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG                           *
      *    FUER DIE VB-STRUKTUR                           *
      *---------------------------------------------------*
      *
       SO160.
           PERFORM SO170 THRU SO189 VARYING WS-J  FROM WS-J BY 1
                                    UNTIL   WS-J > STRUKANZ.
           GO TO SO190.
      *
       SO170.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 2
                MOVE ZERO TO PROVBETR  (WS-J)
                MOVE ZERO TO PROVRUECK (WS-J)
                GO TO SO180.
           IF   WS-DIFFPROV4 = 4
                SUBTRACT 0,4 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 6
                SUBTRACT 0,6 FROM  WS-DIFFPROV.
           IF   WS-DIFFPROV4 = 7
                SUBTRACT 0,7 FROM  WS-DIFFPROV.
      *
           COMPUTE PROVBETR (WS-J) ROUNDED =
                   PRORATASU * GRUNDPROV * WS-DIFFPROV
                   / 10000.

           ADD  PROVBETR (WS-J)  TO ZW-PROVISION.
           ADD  WS-DIFFPROV      TO ZW-DIFFPROV.
      *
      *---------------------------------------------------*
      *    UMSETZEN VM FUER FOLGEPROVISIONEN
      *---------------------------------------------------*
      *
           IF PRODART       = 5 OR 6 OR 7 OR 8
              NEXT SENTENCE
           ELSE
              GO TO SO180.
      *
           MOVE DIFFPROV (WS-J)  TO WS-DIFFPROV.
           IF WS-DIFFPROV4  = 4
                    MOVE Z-VTF  TO VBNR (WS-J).
      *
       SO180.
           MOVE DIFFPROV (WS-J) TO WS-DIFFPROV.
           MOVE ZW-EINHEITEN TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 4
                MOVE ZERO    TO PROVEINH (WS-J).
           IF   WS-DIFFPROV4 = 7
                MOVE Z-VTF   TO VBNR (WS-J).
      *
       SO189.
           EXIT.
      *---------------------------------------------------*
      *    PROVISIONSBERECHNUNG FUER DEN VERTRIEBSFOND    *
      *    DER AUSZUSCHUETTENDE PROVBETR WIRD BEREITS     *
      *    VORHER ERMITTELT.                              *
      *---------------------------------------------------*
       SO190.
           IF   WS-SAVE = 0
                GO TO SO199.
           COMPUTE PROVRUECK (WS-SAVE) = WS-PROV - ZW-PROVISION.
           MOVE ZW-DIFFPROV  TO DIFFPROV (WS-SAVE).
      *
940606 SO199.
           EXIT.
      *---------------------------------------------------------------*
CCC009*    EJECT
      *
      *
      *****************************************************************
      *    ENDE COPY-MEMBER UU511S   PROVISIONSBERECHNUNGEN OPAL 2    *
      *                              PROVISIONSBERECHNUNGEN OPAL 3    *
      *****************************************************************
      *
       A999 SECTION.

