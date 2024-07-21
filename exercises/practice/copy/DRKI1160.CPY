      *                  ###############                               *
      *         #######  D R K I 1 1 6 0  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND DEPOTERöFF- *
      *    NUNGEN UND KUNDENDATENäNDERUNGEN AGI                        *
      *                                                                *
      *    LAENGE =  400  BYTES                                        *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 400 (vorher 202)             *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *  Ersetzt DRKI0184 in PMKI0093.                                 *
      *                                                                *
      *  DATEINAME = P130006.KI012800                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *20.11.01!NEUERSTELLUNG für AGI wegen neue        !v.d.Goltz! 01 *
      *AKI00162!Felder.                                 !         !    *
      *--------!----------------------------------------!---------!----*
      *14.11.07!NEUE FELDER FÜR AGI GEBBETRAG,         !SCHLEICHER!2.2 *
      *        !GEBSCHLÜSSEL UND GEBTEXT               !          !    *
      *--------!----------------------------------------!---------!----*
      *16.01.09!NEUE AUFBAU PRIMARYKEY                  !WHO      !2.3 *
      *        !GEBSCHLÜSSEL UND GEBTEXT                !         !    *
      *--------!----------------------------------------!---------!----*
      *26.05.14!NEUE FELDER FUER OIF                    !SWZ      !2.4 *
      *--------!----------------------------------------!---------!----*
160322*16.03.22!Risikoklasse aufgenommen.               !U131661  !    *
160322*DBO-9823!                                        !(B.Huhn) !    *
250522*--------!----------------------------------------!---------!----*
250522*25.05.22!Logzeilen entfernt                      ! U131661 !    *
250522*        !                                        !(B.Huhn) !    *
      *--------!----------------------------------------!---------!----*
      ******************************************************************
      *                                                                 
       01  DRKI1160.                                                    
               05  BNK-SA                      PIC  X(001).             
      *            SATZART                                              
               05  BNK-GES                     PIC  9(002).             
      *            GESELLSCHAFT                                         
               05  BNK-VNR                     PIC  9(015).
160109         05  FILLER REDEFINES BNK-VNR.
160109             10 BNK-VNR-1-13             PIC  9(013).
160109*            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
160109         05  BNK-UNTER-NR                PIC  9(002).
      *            UNTERDEPOTNUMMER )
               05  BNK-VERARB-KZ               PIC  X(001).             
      *            Verarbeitungskennzeichen                             
      *                I = Insert                                       
      *                U = Update                                       
               05  BNK-WP-KENN-NR              PIC  X(006).             
      *            WERTPAPIERKENN-NUMMER                                
               05  BNK-AEND-WP-KENN-NR-KZ      PIC  X(001).             
      *            J = WERTPAPIERKENNNR ÄNDERUNG                        
180214         05  BNK-KTO-ART                 PIC  X(002).
      *            Kontoart (DE, VL)
180214         05  BNK-AEND-KTO-ART-KZ         PIC  X(001).
      *
               05  BNK-ANTEILE                 PIC  9(006)V999.
      *            ANTEILE (BESTAND STÜCK)                              
               05  BNK-AEND-ANTEILE-KZ         PIC  X(001).             
      *            J = ANTEILE ÄNDERUNG                                 
               05  BNK-BUCH-DAT                PIC  9(008).             
      *            BUCHUNGSDATUM                                        
               05  BNK-AEND-BUCH-DAT-KZ        PIC  X(001).             
      *            J = BUCHUNGSDATUM ÄNDERUNG                           
               05  BNK-BUCH-BETRAG             PIC  9(007)V99.          
      *            BETRAG LETZTE BUCHUNG                                
               05  BNK-AEND-BUCH-BETRAG-KZ     PIC  X(001).             
      *            J = BUCHUNGSBETRAG ÄNDERUNG                          
               05  BNK-BUCH-ST-BETRAG          PIC  9(007)V99.          
      *            STEUERBETRAG LETZTE BUCHUNG                          
               05  BNK-AEND-BUCH-ST-BETRAG-KZ  PIC  X(001).             
      *            J = STEUERBETRAG   ÄNDERUNG                          
               05  BNK-BUCH-SCHL               PIC  X(002).             
      *            BUCHUNGSSCHLÜSSEL                                    
               05  BNK-AEND-BUCH-SCHL-KZ       PIC  X(001).             
      *            J = BUCHUNGSSCHLüSSEL äNDERUNG                       
               05  BNK-EINZ-KUM                PIC  9(013).             
      *            KUMULIERTE EINZAHLUNGEN                              
               05  BNK-AEND-EINZ-KUM-KZ        PIC  X(001).             
      *            J = KUMULIERTE EINZAHLUNGEN äNDERUNG                 
               05  BNK-AUSZ-KUM                PIC  9(013).             
      *            KUMULIERTE AUSZAHLUNGEN                              
               05  BNK-AEND-AUSZ-KUM-KZ        PIC  X(001).
      *            J = KUMULIERTE AUSZAHLUNGEN äNDERUNG                 
180214*        05  BNK-ZINS-AB-ST              PIC  9(006)V99.
      *            ZINSABSCHLAGSTEUER                                   
180214*        05  BNK-AEND-ZINS-AB-ST-KZ      PIC  X(001).
      *            J = ZINSABSCHLAGSTEUER äNDERUNG                      
180214*        05  BNK-KTO-ART                 PIC  X(002).
      *            Kontoart (DE, VL)
180214*        05  BNK-AEND-KTO-ART-KZ         PIC  X(001).
      *                                                                 
180214*        05  BNK-FREI-VERBR              PIC  9(005)V99.
      *            Verbr. FREISTELLUNGSBETRAG IN EURO                   
180214*        05  BNK-AEND-FREI-VERBR-KZ      PIC  X(001).
                                                                        
180214*        05  BNK-ZW-GEWINN               PIC S9(005)V99.
      *            Zwischengewinn Guthaben                              
180214*        05  BNK-AEND-ZW-GEWINN-KZ       PIC  X(001).

               05  BNK-BUCH-ANTEILE            PIC  9(006)V999.         
      *            Buchungsbestand Stückzahl                            
               05  BNK-AEND-BUCH-ANTEILE-KZ    PIC  X(001).             
                                                                        
               05  BNK-UMSATZ-NETTO            PIC  9(007)V99.          
      *            Nettobetrag = Rücknahmepreis x Buchungsanteile       
               05  BNK-AEND-UMSATZ-NETTO-KZ    PIC  X(001).             
                                                                        
141107         05  BNK-GEB-BETRAG              PIC  9(007)V99.          
..!   *            GEBÜHRENBETRAG                                       
..!            05  BNK-AEND-GEB-BETRAG-KZ      PIC  X(001).             
..!   *                                                                 
..!            05  BNK-GEB-SCHLUESSEL          PIC  9(002).             
..!   *            GEBÜHRENSCHLÜSSEL                                    
..!            05  BNK-AEND-GEB-SCHLUESSEL-KZ  PIC  X(001).             
..!            05  BNK-GEB-TEXT                PIC  X(020).             
..!   *            GEBÜHRENTEXT                                         
..!            05  BNK-AEND-GEB-TEXT-KZ        PIC  X(001).             
..!
180214         05  BNK-ISIN                    PIC  X(012).
      *            ISIN
      *            (absichtlich ohne AEND-KZ)
      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
140314         05  BNK-ANZ-ANT-UN-RCK-GEK  PIC  9(07)V9999.
      *            Anzahl wegen unwiderruflicher Rückgabe gekündigte
      *            Anteile bei offenen Immobilienfonds
..!            05  BNK-AEND-ANZ-ANT-UN-RCK-GEK     PIC  X(001).
..!
140314         05  BNK-ANZ-ANT-ABL-MHF-ERR PIC  9(07)V9999.
      *            Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *            erreicht (12 Monate oder länger im Bestand des
      *            Kunden)
..!            05  BNK-AEND-ANZ-ANT-ABL-MHF-ERR     PIC  X(001).
..!
140314         05  BNK-ANZ-ANT-ABL-MHF-N-E PIC  9(07)V9999.
      *            Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *            noch nicht erreicht (kürzer als 12 Monate im
      *            Bestand des Kunden)
..!            05  BNK-AEND-ANZ-ANT-ABL-MHF-N-E     PIC  X(001).
..!
140314         05  BNK-ANZ-ANT-ERW-VOR-JU13 PIC  9(07)V9999.
      *            Anzahl Anteile OIF mit Erwerb vor dem 22.07.2013
      *            (Anzahl der Anteile, die im Rahmen des halbjährl.
      *            Kontingentes für vor dem 22.07.2013 erworbene
      *            Anteile ohne Kündigung zurückgegeben werden können)
..!            05  BNK-AEND-ANZ-ANT-ERW-VOR-JU13     PIC  X(001).
..!
140314         05  BNK-EUR-BER-VERK-JU13 PIC  9(06)V99.
      *            Bereits ausgenutztes Halbjahres-Kontingent dieser
      *            WKN, zur Rückgabe von OIF ( Volumen in Euro, das im
      *            Rahmen des halbjährlichen Kontingentes dieser WKN
      *            für die Rückgabe von vor dem 22.07.2013 erworbene
      *            Anteile im aktuellen Kalenderhalbjahr bereits in
      *            Anspruch genommen wurdech genommen wurde)
..!            05  BNK-AEND-EUR-BER-VERK-JU13     PIC  X(001).
..!
140314         05  BNK-ANLEGER-ID        PIC  X(14).
      *            Anleger-ID zu bereits ausgenutztem
      *            Halbjahres-Kontingent dieser WKN, zur Rückgabe
      *            von OIF
..!            05  BNK-AEND-ANLEGER-ID        PIC  X(001).
..!
160322         05  BNK-RISIKOKLASSE           PIC  9(001).
160322*            Risikoklasse 0 - 9                  
      
      *        *****************************************************
      *        *   Reserve
      *        *****************************************************
140314*        05  FILLER                  PIC  X(161).
160322         05  FILLER                  PIC  X(160).        
      *            RESERVE                                              
                                                                        
                                                                        
