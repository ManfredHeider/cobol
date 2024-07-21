      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0084.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0084.CPv  $
      *
      *   Rev 2.1   Jul 03 2007 16:56:44   t13ex54
      *AKI00666: Einführung in MFE
      *
      *   Rev 2.0   Feb 27 1999 12:52:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 May 1998 09:33:56   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:45:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *       DGFF8925                                                  
      *       S A V A R I  - AUSGABESATZ FÜR DVAG-DTA                   
      *                                               insges. 12000 B   
      *                                                                 
      *       SATZAUFBAU FÜR AMV-SACH VERTRAG                     ...   
      *                                                                 
      *       letzte nderung: 23.10.97 Thiele                           
      *                       -Allgemeinen Teil ausgegliedert           
      *                       - Haftpflicht Teil geändert               
      *                                                                 
      *       letzte Änderung: 19.08.97 Thiele                          
      *                        Änderungswünsche DVAG                    
      *                                                                 
      *              Änderung: 29.07.97 SCHÖNENBERG                     
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
      *                                                                 
         05 AMV-S-BESTAND.                                              
      *                                                                 
      *  Allgemeine Daten                                               
      *                                                                 
           10  AMV-S-ALLG-DATEN.                                        
               15  AMV-S-A-AEND-KZ           PIC 9(02).                 
               15  AMV-S-A-VSNR              PIC 9(15).                 
               15  AMV-S-A-DVAG-VSNR         PIC 9(07).                 
               15  AMV-S-A-DVAG-PRUEF        PIC 9(02).                 
               15  AMV-S-A-BEGINN            PIC 9(08).                 
               15  FILLER REDEFINES AMV-S-A-BEGINN.                     
                   20  AMV-S-A-BEG-JJJJ      PIC 9(04).                 
                   20  AMV-S-A-BEG-MM        PIC 9(02).                 
                   20  AMV-S-A-BEG-TT        PIC 9(02).                 
               15  FILLER REDEFINES AMV-S-A-BEGINN.                     
                   20  AMV-S-A-BEG-JH        PIC 9(02).                 
                   20  AMV-S-A-BEG-JJ        PIC 9(02).                 
                   20  FILLER                PIC 9(04).                 
               15  AMV-S-A-ABLAUF            PIC 9(08).                 
               15  FILLER REDEFINES AMV-S-A-ABLAUF.                     
                   20  AMV-S-A-ABL-JJJJ      PIC 9(04).                 
                   20  AMV-S-A-ABL-MM        PIC 9(02).                 
                   20  AMV-S-A-ABL-TT        PIC 9(02).                 
               15  FILLER REDEFINES AMV-S-A-ABLAUF.                     
                   20  AMV-S-A-ABL-JH        PIC 9(02).                 
                   20  AMV-S-A-ABL-JJ        PIC 9(02).                 
                   20  FILLER                PIC 9(04).                 
               15  AMV-S-A-VERZUSTAND        PIC X(01).                 
               15  AMV-S-A-BLZ               PIC 9(11).                 
               15  AMV-S-A-KTONR             PIC 9(18).                 
               15  AMV-S-A-ZW                PIC X(01).                 
               15  AMV-S-A-BEITRAG           PIC 9(08)V99.              
               15  AMV-S-A-DVAG-VBNR         PIC 9(07).                 
               15  AMV-S-A-AENDBEG           PIC 9(08).                 
               15  FILLER REDEFINES AMV-S-A-AENDBEG.                    
                   20  AMV-S-A-AEND-JJJJ     PIC 9(04).                 
                   20  AMV-S-A-AEND-MM       PIC 9(02).                 
                   20  AMV-S-A-AEND-TT       PIC 9(02).                 
               15  FILLER REDEFINES AMV-S-A-AENDBEG.                    
                   20  AMV-S-A-AEND-JH       PIC 9(02).                 
                   20  AMV-S-A-AEND-JJ       PIC 9(02).                 
                   20  FILLER                PIC 9(04).                 
               15  AMV-S-A-LBDAT             PIC 9(08).                 
               15  FILLER REDEFINES AMV-S-A-LBDAT.                      
                   20  AMV-S-A-LBDAT-JJJJ    PIC 9(04).                 
                   20  AMV-S-A-LBDAT-MM      PIC 9(02).                 
                   20  AMV-S-A-LBDAT-TT      PIC 9(02).                 
               15  FILLER REDEFINES AMV-S-A-LBDAT.                      
                   20  AMV-S-A-LBDAT-JH      PIC 9(02).                 
                   20  AMV-S-A-LBDAT-JJ      PIC 9(02).                 
                   20  FILLER                PIC 9(04).                 
               15  AMV-S-A-LBUHRZEIT         PIC 9(07).                 
               15  AMV-S-A-INKASSO           PIC X(01).                 
               15  AMV-S-A-STORNO            PIC 9(08).                 
               15  FILLER REDEFINES AMV-S-A-STORNO.                     
                   20  AMV-S-A-STORNO-JJJJ   PIC 9(04).                 
                   20  AMV-S-A-STORNO-MM     PIC 9(02).                 
                   20  AMV-S-A-STORNO-TT     PIC 9(02).                 
               15  FILLER REDEFINES AMV-S-A-STORNO.                     
                   20  AMV-S-A-STORNO-JH     PIC 9(02).                 
                   20  AMV-S-A-STORNO-JJ     PIC 9(02).                 
                   20  FILLER                PIC 9(04).                 
               15  AMV-S-A-NACHL-TABELLE.                               
                   20 AMV-S-A-NACHL-TABELLE-SAV  OCCURS 10.             
                      25 AMV-S-A-NACHL-TEXT     PIC X(26).              
                      25 AMV-S-A-NACHL-EINHEIT  PIC 9(01).              
                      25 AMV-S-A-NACHL-WERT     PIC 9(08)V99.           
               15  AMV-S-A-GEB-VERTRAG       PIC X(01).                 
               15  AMV-S-A-VORT-LKZ          PIC X(03).                 
               15  AMV-S-A-VORT-PLZ          PIC X(05).                 
               15  AMV-S-A-VORT-STR          PIC X(21).                 
               15  AMV-S-A-VORT-ORT          PIC X(21).                 
               15  AMV-S-A-FILLER            PIC X(324).                
      *                                                                 
      *  Daten aus SAVARI                                               
      *                                                                 
            10 AMV-S-SACH-DATEN.                                        
               15 AMV-S-SACH-WOHNGEB.                                   
                  20  AMV-S-SW-ZWG               PIC 9(02).             
                  20  AMV-S-SW-ZWG-TEXT          PIC X(03).             
                  20  AMV-S-SW-GEFAHR-ZONE-SB               OCCURS 5.   
                     25  AMV-S-SW-GEFAHR         PIC X(03).             
                     25  AMV-S-SW-ZONE           PIC 9(01).             
                  20  AMV-S-SW-ST-SB.                                   
                     25  AMV-S-SW-ST-VSUMME      PIC 9(02).             
                     25  AMV-S-SW-ST-SCHADEN     PIC 9(02).             
                     25  AMV-S-SW-ST-MIN         PIC 9(05).             
                     25  AMV-S-SW-ST-MAX         PIC 9(05).             
                  20  AMV-S-SW-LW-SB.                                   
                     25  AMV-S-SW-LW-VSUMME      PIC 9(02).             
                     25  AMV-S-SW-LW-SCHADEN     PIC 9(02).             
                     25  AMV-S-SW-LW-MIN         PIC 9(05).             
                     25  AMV-S-SW-LW-MAX         PIC 9(05).             
                  20  AMV-S-SW-EL-ERD-SB.                               
                     25  AMV-S-SW-EL-ERD-VSUMME  PIC 9(02).             
                     25  AMV-S-SW-EL-ERD-SCHADEN PIC 9(02).             
                     25  AMV-S-SW-EL-ERD-MIN     PIC 9(05).             
                     25  AMV-S-SW-EL-ERD-MAX     PIC 9(05).             
                  20  AMV-S-SW-EL-SON-SB.                               
                     25  AMV-S-SW-EL-SON-VSUMME  PIC 9(02).             
                     25  AMV-S-SW-EL-SON-SCHADEN PIC 9(02).             
                     25  AMV-S-SW-EL-SON-MIN     PIC 9(05).             
                     25  AMV-S-SW-EL-SON-MAX     PIC 9(05).             
                  20  AMV-S-SW-VSUMME            PIC 9(09)V99.          
                  20  AMV-S-SW-SB-KZ             PIC X(02).             
                  20  AMV-S-SW-LEISTUNG          PIC 9(02).             
                  20  AMV-S-SW-BAUART-KLASSE     PIC X(02).             
                  20  AMV-S-SW-BAUJAHR           PIC 9(04).             
                  20  AMV-S-SW-ART-GEBAEUDE      PIC 9(02).             
                  20  AMV-S-SW-GN-FAKTOR         PIC 9(02)V9.           
                  20  AMV-S-SW-VGB88-GARAGE      PIC X(01).             
                  20  AMV-S-SW-VGB88-NEBENGEB    PIC X(01).             
                  20  AMV-S-SW-VGB88-ANBAU       PIC X(01).             
                  20  AMV-S-SW-VGB88-CARPORT     PIC X(01).             
                  20  AMV-S-SW-UMFANG            PIC 9(03)  OCCURS 20.  
                  20  AMV-S-SW-BEDING            PIC 9(02)  OCCURS 15.  
                  20  AMV-S-SW-KLAUSEL           PIC 9(04)  OCCURS 15.  
                  20  AMV-S-SW-NETTO-PRAEM       PIC 9(08)V99.          
                  20  AMV-S-SW-FILLER            PIC X(100).            
      *                                                                 
               15 AMV-S-SACH-HAUSRAT.                                   
                  20  AMV-S-SH-ZWG              PIC 9(02).              
                  20  AMV-S-SH-ZWG-TEXT         PIC X(03).              
                  20  AMV-S-SH-GEFAHR           PIC X(03)   OCCURS 6.   
                  20  AMV-S-SH-VSUMME           PIC 9(09)V99.           
                  20  AMV-S-SH-SB-KZ            PIC X(02).              
                  20  AMV-S-SH-LEISTUNG         PIC 9(02).              
                  20  AMV-S-SH-ZONE             PIC 9(01).              
                  20  AMV-S-SH-ART-GEBAEUDE     PIC 9(01).              
                  20  AMV-S-SH-BEWOHNT          PIC X(01).              
                  20  AMV-S-SH-QM-WOHNFL        PIC 9(05).              
                  20  AMV-S-SH-UMFANG-PROZ                  OCCURS 5.   
                     25  AMV-S-SH-UMFANG        PIC 9(03).              
                     25  AMV-S-SH-PROZ          PIC 9(02)V9.            
                  20  AMV-S-SH-BEDING           PIC 9(02)   OCCURS 15.  
                  20  AMV-S-SH-KLAUSEL          PIC 9(04)   OCCURS 15.  
                  20  AMV-S-SH-PRAEMIE          PIC 9(05)V99.           
                  20  AMV-S-SH-FILLER           PIC X(100).             
      *                                                                 
               15 AMV-S-SACH-GLAS.                                      
                  20  AMV-S-SG-ZWG              PIC 9(02).              
                  20  AMV-S-SG-ZWG-TEXT         PIC X(03).              
                  20  AMV-S-SG-WOHNFLAECHE      PIC 9(05).              
                  20  AMV-S-SG-GEFAHR-INFO                  OCCURS 10.  
                     25  AMV-S-SG-GEFAHR        PIC 9(03).              
                     25  AMV-S-SG-BERECH-NACH   PIC 9(02).              
                     25  AMV-S-SG-BERECH-WERT   PIC 9(09).              
                  20  AMV-S-SG-ART-GEBAEUDE     PIC 9(01).              
                  20  AMV-S-SG-BEDING           PIC 9(02)   OCCURS 15.  
                  20  AMV-S-SG-KLAUSEL          PIC 9(04)   OCCURS 15.  
                  20  AMV-S-SG-PRAEMIE          PIC 9(05)V99.           
                  20  AMV-S-SG-FILLER           PIC X(100).             
      *                                                                 
               15 AMV-S-HAFTPFLICHT.                                    
                  20  AMV-S-H-ZWG               PIC 9(02).              
                  20  AMV-S-H-ZWG-TEXT          PIC X(03).              
                  20  AMV-S-H-BEDING            PIC 9(02)   OCCURS 15.  
                  20  AMV-S-H-KLAUSEL           PIC 9(04)   OCCURS 15.  
                  20  AMV-S-H-PRODUKT           PIC 9(02).              
                  20  AMV-S-H-PRODUKT-GRUND-ZUS  occurs 2.              
                     25  AMV-S-H-GRUND-ZUS-KZ   PIC X(01).              
                     25  AMV-S-H-WANR-MENGE         occurs 20.          
                        30  AMV-S-H-WANR        PIC 9(04).              
                        30  AMV-S-H-MENGE       PIC 9(07).              
                        30  AMV-S-H-MENGE-TEXT  PIC X(06).              
                     25  AMV-S-H-VSUM-PERS         PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-SACH         PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-VERM         PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-EK           PIC 9(05).           
                     25  AMV-S-H-VSUM-MIET         PIC 9(08)V99.        
                     25  AMV-S-H-PRAEMIE           PIC 9(05)V99.        
                     25  AMV-S-H-VSUM-PERS-SB      PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-SACH-SB      PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-VERM-SB      PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-EK-SB        PIC 9(08)V99.        
                     25  AMV-S-H-VSUM-MIET-SB      PIC 9(08)V99.        
                     25  AMV-S-H-LEBENSPARTNER     PIC X(21).           
                  20  AMV-S-H-FILLER            PIC X(100).             
      *                                                                 
               15 AMV-S-UNFALL.                                         
                  20  AMV-S-U-ZWG               PIC 9(02).              
                  20  AMV-S-U-ZWG-TEXT          PIC X(03).              
                  20  AMV-S-U-BEDING            PIC 9(02)   OCCURS 15.  
                  20  AMV-S-U-KLAUSEL           PIC 9(04)   OCCURS 15.  
                  20  AMV-S-U-DTUTAB OCCURS 33.                         
                     25  AMV-S-U-PROGRESSION    PIC 9(04).              
                     25  AMV-S-U-DYNAMIKPROZ    PIC 9(02)V99.           
                     25  AMV-S-U-NAME           PIC X(21).              
                     25  AMV-S-U-GEBDAT         PIC X(08).              
                     25  FILLER REDEFINES AMV-S-U-GEBDAT.               
                         30  FILLER             PIC 9(02).              
                         30  AMV-S-U-GEBDAT-JMT PIC 9(06).              
                     25  FILLER REDEFINES AMV-S-U-GEBDAT.               
                         30  AMV-S-U-GEBDAT-JH  PIC 9(02).              
                         30  AMV-S-U-GEBDAT-JJ  PIC 9(02).              
                         30  FILLER             PIC 9(04).              
                     25  AMV-S-U-BERUF          PIC 9(03).              
                     25  AMV-S-U-BERUF-GEN      PIC 9(01).              
                     25  AMV-S-U-DECKUNG        PIC 9(01).              
                     25  AMV-S-U-VSUM-TOD       PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-INV       PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-KTG       PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-TG        PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-GG        PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-HEIL      PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-HHILF     PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-SCHULGE   PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-GRUNDSUM  PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-UEBERG    PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-KOSMETIK  PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-BERGUNG   PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-KUR       PIC 9(08)V99.           
                     25  AMV-S-U-VSUM-RENTE     PIC 9(08)V99.           
                     25  AMV-S-U-PRAEMIE        PIC 9(05)V99.           
                  20  AMV-S-U-FILLER            PIC X(100).             
      *                                                                 
               15 AMV-S-SONSTIGE-SACH.                                  
                  20  AMV-S-SONSTIGE-SACH-DATEN             OCCURS 9.   
                     25  AMV-S-SS-LFDNR         PIC 9(02).              
                     25  AMV-S-SS-ZWG           PIC 9(02).              
                     25  AMV-S-SS-ZWG-TEXT      PIC X(03).              
                     25  AMV-S-SS-VSUMME        PIC 9(09)V99.           
                     25  AMV-S-SS-bedocc                    OCCURS 15.  
                       30 amv-s-ss-beding       pic 9(02).              
                     25  AMV-S-SS-KLAocc                    OCCURS 15.  
                       30 AMV-S-SS-KLAUSEL       PIC 9(04).             
                     25  AMV-S-SS-PRAEMIE       PIC 9(05)V99.           
                     25  AMV-S-SS-FILLER        PIC X(100).             
      *                                                                 
               15 FILLER                        pic x(360).             
      *                                                                 
      * Daten aus K und Moped                                           
      *                                                                 
      *     10 AMV-S-K-DATEN.                                           
      *        15 AMV-S-KRAFTFAHRT.                                     
      *                                                                 
      *           20  AMV-S-ZWEIGSCHLUESSEL     PIC 9(02).              
      *           20  AMV-S-ZWEIGTEXT           PIC X(03).              
      *           20  AMV-S-AKZ                 PIC X(12).              
      *           20  AMV-S-ERSTZUL-DAT         PIC 9(08).              
      *           20  AMV-S-WKZ                 PIC 9(03).              
      *           20  AMV-S-DA-KH               PIC 9(02).              
      *           20  AMV-S-BEITRAGSSATZ-KH     PIC X(03).              
      *           20  AMV-S-DA-KF               PIC 9(02).              
num   *           20  AMV-S-BEITRAGSSATZ-KF     PIC 9(03).              
      *           20  AMV-S-DA-KU               PIC 9(02).              
      *           20  AMV-S-HERSTELLER-NR       PIC X(04).              
      *           20  AMV-S-TSN                 PIC X(03).              
      *           20  AMV-S-FAHRZEUG-ID         PIC X(20).              
      *           20  AMV-S-KFZ-LEIST-KZ        PIC X(01).              
      *           20  AMV-S-KFZ-LEISTUNG        PIC 9(05)V99.           
      *           20  AMV-S-RKL-KH              PIC X(04).              
      *           20  AMV-S-BEITRAGSKL-KH       PIC X(06).              
      *           20  AMV-S-BEITRAG-ZW-KH       PIC 9(08)V99.           
      *           20  AMV-S-TYPKL-KH            PIC 9(02).              
      *           20  AMV-S-SB-VK               PIC 9(08)V99.           
      *           20  AMV-S-SB-TK               PIC 9(08)V99.           
      *           20  AMV-S-TYPKL-KF            PIC 9(02).              
      *           20  AMV-S-RKL-KF              PIC X(04).              
      *           20  AMV-S-BEITRAGSKL-KF       PIC X(06).              
      *           20  AMV-S-BEITRAG-ZW-KF       PIC 9(08)V99.           
      *           20  AMV-S-INV-SUMME-KU        PIC 9(08)V99.           
      *           20  AMV-S-TOD-SUMME-KU        PIC 9(08)V99.           
      *           20  AMV-S-INV-PROG-KU         PIC 9(03).              
      *           20  AMV-S-BEITRAG-ZW-KU       PIC 9(08)V99.           
      *           20  AMV-S-TAG-SUMME-KU        PIC 9(08)V99.           
      *           20  AMV-S-KFZ-NEUWERT         PIC 9(07).              
      *           20  AMV-S-SCHUTZBRIEF         PIC X(10).              
      *           20  AMV-S-SAISON-KZ           PIC X(01).              
      *           20  AMV-S-KFZ-TYP-KZ          PIC X(50).              
      *           20  AMV-S-KFZ-WKZ-TEXT        PIC X(25).              
      *           20  AMV-S-SAISON-BEG-MM       PIC 9(02).              
      *           20  AMV-S-SAISON-END-MM       PIC 9(02).              
      *           20  AMV-S-VERSBED-TAB         OCCURS 3.               
      *            25 AMV-S-VERS-DECK-SL        PIC X(02).              
      *            25 AMV-S-VERS-AKB-TEXT       PIC X(19).              
      *           20  AMV-S-TARIF-TAB           OCCURS 3.               
      *            25 AMV-S-TARIF-DECK-SL       PIC X(02).              
      *            25 AMV-S-TARIF-BEZEICHNUNG   PIC X(05).              
      *           20  AMV-S-KRAFT-FILLER        PIC X(400).             
 neu  *   K-Teil entspricht insgesamt xxx Bytes.                        
