      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS                         
      *            Datum   :  Januar 1999                               
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1999 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1159.CPY  $                                 00010000










      *    $Revision:   2.11  $                                         
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1159.CPv  $                            
      *
      *   Rev 2.11   Jun 10 2014 14:45:22   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.10   Jun 10 2014 12:08:10   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.9   Jun 05 2014 13:57:50   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.8   Jun 05 2014 13:50:46   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.7   May 28 2014 10:34:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.6   May 28 2014 10:19:34   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.5   Feb 26 2014 16:53:48   t13ex15
      *FKI01447: AGI Bestands-DTA Ergänzung ISIN
      *
      *   Rev 2.4   Jun 29 2009 10:21:12   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.3   Jun 15 2009 16:33:06   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.2   Dec 19 2007 13:55:44   t13ex50
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen
      *                                                                 
      *   Rev 2.1   Mar 07 2002 17:45:14   t13ex12                      
      *AKI00162 Neue Felder von AGI
      *                                                                 
      *   Rev 2.0   07 Dec 2000 09:36:08   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG BNK (DTA)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    Ersetzt DRKI0191 in PMKI0085.                               *
      *    Länge = 400 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!---------------------------------------!----------!----*
      *15.11.01!NEUERSTELLUNG FÜR AGI WEGEN NEUE       !V.D.GOLTZ ! 01 *
      *AKI00162!FELDER.                                !          !    *
      *--------!---------------------------------------!----------!----*
      *14.11.07!NEUE FELDER FÜR AGI GEBBETRAG,         !SCHLEICHER!2.2 *
      *        !GEBSCHLÜSSEL UND GEBTEXT               !          !    *
      *--------!---------------------------------------!----------!----*
      *16.01.09!NEUER AUFBAU PRIMARY-KEY               !WHO       !    *
      *--------!---------------------------------------!----------!----*
      *18.02.14!Umbau wegen ISIN                       !SWZ       !    *
      *--------!---------------------------------------!----------!----*
      *26.05.14!Umbau wegen OIF                        !SWZ       !    *
      *        !                                       !          !    *
      *        !                                       !          !    *
      *--------!---------------------------------------!----------!----*
      *                                                                 
       01  DRKI1159.                                                    
      *                                                                 
               05  KOMPL-SA             PIC  X(001).
      *            SATZART        = 'C'                                 
               05  KOMPL-GES            PIC  9(002).
      *            GESELLSCHAFT                                         
      *                                                                 
160109         05  KOMPL-KEY.
>>                 25 KOMPL-VNR              PIC  9(015).
      *               VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER) 
                   25 FILLER REDEFINES KOMPL-VNR.
                      30 KOMPL-VNR-1-13      PIC  9(013).
                      30 KOMPL-UNTER-NR      PIC  9(002).
      *               UNTERDEPOTNUMMER)
                   25 KOMPL-WP-KENN-NR       PIC  X(006).
<<    *               WERTPAPIERKENN-NUMMER
160109             25 KOMPL-UNTER-KONTOART   PIC  X(002).
      *               UnterKontoart
      *

               05  KOMPL-ANTEILE        PIC  9(006)V999.
      *            ANTEILE (BESTAND STÜCK)                              
               05  KOMPL-BUCH-DAT       PIC  9(008).
      *            BUCHUNGSDATUM                                        
               05  KOMPL-BUCH-BETRAG    PIC  9(007)V99.
      *            BETRAG LETZTE BUCHUNG                                
               05  KOMPL-BUCH-ST-BETRAG PIC  9(007)V99.
      *            STEUERBETRAG LETZTE BUCHUNG                          
               05  KOMPL-BUCH-SCHL      PIC  X(002).
      *            BUCHUNGSSCHLÜSSEL                                    
               05  KOMPL-EINZ-KUM       PIC  9(013).
      *            KUMULIERTE EINZAHLUNGEN                              
               05  KOMPL-AUSZ-KUM       PIC  9(013).
      *            KUMULIERTE AUSZAHLUNGEN                              
140314         05  FILLER               PIC  X(22).
               05  KOMPL-BUCH-ANTEILE   PIC  9(006)V999.
      *            ANZAHL DER ANTEILE FÜR BUCHUNG                       
               05  KOMPL-UMSATZ-NETTO   PIC  9(007)V99.
      *            NETTOBETRAG FÜR BUCHUNG = RÜCKNAHMEPREIS X ANTEILE   
141107         05  KOMPL-GEB-BETRAG     PIC  9(007)V99.
..!   *            GEBÜHRENBETRAG                                       
..!            05  KOMPL-GEB-SCHLUESSEL PIC  9(002).
..!   *            GEBÜHRENSCHLÜSSEL                                    
..!            05  KOMPL-GEB-TEXT       PIC  X(020).
..!   *            GEBÜHRENTEXT

..!            05  KOMPL-ISIN           PIC  X(012).
..!   *            GEBÜHRENTEXT
      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
140314         05  KOMPL-ANZ-ANT-UN-RCK-GEK  PIC  9(07)V9999.
      *            Anzahl wegen unwiderruflicher Rückgabe gekündigte
      *            Anteile bei offenen Immobilienfonds
140314         05  KOMPL-ANZ-ANT-ABL-MHF-ERR PIC  9(07)V9999.
      *            Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *            erreicht (12 Monate oder länger im Bestand des
      *            Kunden)
140314         05  KOMPL-ANZ-ANT-ABL-MHF-N-E PIC  9(07)V9999.
      *            Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *            noch nicht erreicht (kürzer als 12 Monate im
      *            Bestand des Kunden)
140314         05  KOMPL-ANZ-ANT-ERW-VOR-JU13 PIC  9(07)V9999.
      *            Anzahl Anteile OIF mit Erwerb vor dem 22.07.2013
      *            (Anzahl der Anteile, die im Rahmen des halbjährl.
      *            Kontingentes für vor dem 22.07.2013 erworbene
      *            Anteile ohne Kündigung zurückgegeben werden können)
140314         05  KOMPL-EUR-BER-VERK-JU13 PIC  9(06)V99.
      *            Bereits ausgenutztes Halbjahres-Kontingent dieser
      *            WKN, zur Rückgabe von OIF ( Volumen in Euro, das im
      *            Rahmen des halbjährlichen Kontingentes dieser WKN
      *            für die Rückgabe von vor dem 22.07.2013 erworbene
      *            Anteile im aktuellen Kalenderhalbjahr bereits in
      *            Anspruch genommen wurdech genommen wurde)
140314         05  KOMPL-ANLEGER-ID        PIC  X(14).
      *            Anleger-ID zu bereits ausgenutztem
      *            Halbjahres-Kontingent dieser WKN, zur Rückgabe
      *            von OIF
      *        *****************************************************
      *        *   Reserve
      *        *****************************************************
140314         05  FILLER                  PIC  X(162).
      *            RESERVE                                              
                                                                        
                                                                        
