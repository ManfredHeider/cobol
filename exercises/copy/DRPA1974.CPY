      *****************************************************************
      *    COPY-MEMBER: DRPA1974                                      *
      *                                                               *
      *   <KURZE FACHLICHE BESCHREIBUNG>                              *
      *   DAS COPY DIENT ALS AUSGABESCHNITTSTELLE FÜR DAS PMPA0148    *
      *   FÜR DIE ERSTELLEN DES DETAILLIERTEN EX-POST KOSTENAUSWEISES *
      *   FÜR GES. 40.                                                *
      *   DIE ZUGEHÖRIGE DATEI PA055900 WIRD ZUR PDF ERSTELLUNG AN    *
      *   ITOPEA (NEOFYTOS CHAITAS) GELIEFERT.                        *
      *   (BIS AUF DEPOTNUMMER-FORMAT ANALOG DRPA1966 / DEUBA)        *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY04832   !28.07.22! NEU                          ! BUCH     *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           12  DTL-KOSTENAUSWEIS.                                     
               13 DEPOTEBENE.                                         
                  15 PGEB-GES              PIC  X(002).               
                  15 FILLER                PIC  X          VALUE ';'.
      *** ZWEITER FILLER FÜR ITOPEA FÜR GLEICHE SPALTEN ANALOG DEUBA    
                  15 FILLER                PIC  X          VALUE ';'.
                  15 DEPOTNR               PIC  X(010).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 BERICHTSJAHR          PIC  X(004).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 KU-ZEILE-1            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 KU-ZEILE-2            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 KU-ZEILE-3            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 KU-ZEILE-4            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 KU-ZEILE-5            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 KU-ZEILE-6            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 VB-NUMMER             PIC  X(007).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 VB-ZEILE-1            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 VB-ZEILE-2            PIC  X(060).               
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-3            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-4            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-5            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-6            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-7            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-8            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-9            PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-10           PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 VB-ZEILE-11           PIC  X(060).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                                                                         
               13 UNTERDEPOTEBENE.                                       
                  15 UDP-NR                PIC  X(002).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 BERICHTSZEITRAUM      PIC  X(019).                  
                  15 FILLER                PIC  X          VALUE ';'.    
                  15 UDP-PREIS             PIC  X(011).                  
                  15 FILLER                PIC  X          VALUE ';'.   
                  15 UDP-PREIS-PRZ         PIC  X(007).                 
                  15 FILLER                PIC  X          VALUE ';'.   
      *              DIENSTLEISTUNGSKOSTEN                              
                  15 UDP-DNSTLST           PIC  X(011).                 
                  15 FILLER                PIC  X          VALUE ';'.   
      *              ANTEIL DIENSTLEISTUNGSKOSTEN AN VOLUMEN            
                  15 UDP-DNSTLST-PRZ       PIC  X(007).                 
                  15 FILLER                PIC  X          VALUE ';'.   
      *              ANTEIL PROVISION AN DIENSTLEISTUNGKOSTEN           
                  15 UDP-DNSTLST-PROV      PIC X(011).                  
                  15 FILLER                PIC  X          VALUE ';'.   
      *              ANTEIL PROVISION VOLUMEN                           
                  15 UDP-DNSTLST-PROV-PRZ  PIC X(007).                  
                  15 FILLER                PIC  X          VALUE ';'.   
      *              PRODUKTKOSTEN                                      
                  15 UDP-PRDKT             PIC X(011).                  
                  15 FILLER                PIC  X          VALUE ';'.   
      *              ANTEIL PRODUKTKOSTEN AN VOLUMEN                    
                  15 UDP-PRDKT-PRZ         PIC X(007).                  
                  15 FILLER                PIC  X          VALUE ';'.   
      *              GESAMTKOSTEN                                       
                  15 UDP-GSMT-KOSTEN       PIC X(011).                  
                  15 FILLER                PIC  X          VALUE ';'.   
      *              ANTEIL GESAMTKOSTEN AN VOLUMEN                     
                  15 UDP-GSMT-KOSTEN-PRZ   PIC X(007).                  
                  15 FILLER                PIC  X          VALUE ';'.    
      *              ANTEIL PROVISION DER GESAMTKOSTEN                   
                  15 UDP-GSMT-PROV         PIC X(011).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              ANTEIL PROVISION AN VOLUMEN                         
                  15 UDP-GSMT-PROV-PRZ     PIC X(007).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              SUMME EINMALIGE KOSTEN                              
                  15 UDP-EINMALIG          PIC X(011).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              ANTEIL EINMALIGE KOSTEN AN VOLUMEN                  
                  15 UDP-EINMALIG-PRZ      PIC X(007).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              SUMME LAUFENDE KOSTEN                               
                  15 UDP-LAUFEND           PIC X(011).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              ANTEIL LAUFENDE KOSTEN AN VOLUMEN                   
                  15 UDP-LAUFEND-PRZ       PIC X(007).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              SUMME TRANSAKTIONSKOSTEN                            
                  15 UDP-TRANSAKTIONEN     PIC X(011).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              ANTEIL TRANSAKTIONEN AN VOLUMEN                     
                  15 UDP-TRANSAKTIONEN-PRZ PIC X(007).                   
                  15 FILLER                PIC  X          VALUE ';'.    
      *              SUMME EREIGNISKOSTEN                                
                  15 UDP-EREIGNIS          PIC X(011).                
                  15 FILLER                PIC  X          VALUE ';'. 
      *              ANTEIL EREIGNISKOSTEN AN VOLUMEN                 
                  15 UDP-EREIGNIS-PRZ      PIC X(007).                
                  15 FILLER                PIC  X          VALUE ';'. 
      *              DURCHSCHNITTLICHES UNTERDEPOTVOLUMEN             
                  15 UDP-VOLUMEN           PIC  X(015).               
                  15 FILLER                PIC  X          VALUE ';'. 
               13 FINANZINSTRUMENTEBENE.                              
                  15 WKN                   PIC  X(006).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 ISIN                  PIC  X(012).               
                  15 FILLER                PIC  X          VALUE ';'. 
                  15 FI-BEZ                PIC  X(100).               
                  15 FILLER                PIC  X          VALUE ';'. 
      *              GESAMTKOSTEN FINANZINSTRUMENT                    
                  15 FI-GSMT               PIC X(011).                
                  15 FILLER                PIC  X          VALUE ';'. 
      *              ANTEIL FI-GSMT AN VOLUMEN                        
                  15 FI-GSMT-PRZ           PIC X(007).                
                  15 FILLER                PIC  X          VALUE ';'. 
      *              ANTEIL PROVISIONEN DER GESAMTKOSTEN              
                  15 FI-GSMT-PROV          PIC X(011).                
                  15 FILLER                PIC  X          VALUE ';'. 
      *              ANTEIL PROVISIONEN AN VOLUMEN                    
                  15 FI-GSMT-PROV-PRZ      PIC X(007).                
                  15 FILLER                PIC  X          VALUE ';'.  
      *              SUMME DIENSTLEISTUNGEN JE FI                      
                  15 FI-DNSTLST            PIC X(011).                 
                  15 FILLER                PIC  X          VALUE ';'.  
      *              ANTEIL DIENSTLEISTUNG AN VOLUMEN                  
                  15 FI-DNSTLST-PRZ        PIC X(007).                 
                  15 FILLER                PIC  X          VALUE ';'.
      *              SUMME PRODUKTKOSTEN JE FI                         
                  15 FI-PRDKT              PIC X(011).                 
                  15 FILLER                PIC  X          VALUE ';'.  
      *              ANTEIL PRODUKTKOSTEN AN VOLUMEN                   
                  15 FI-PRDKT-PRZ          PIC X(007).                 
                  15 FILLER                PIC  X          VALUE ';'.  
               13 TRANSAKTIONSEBENE.                                   
      *              FÄLLIGKEIT                                        
                  15 DATUM                 PIC  X(010).                
                  15 FILLER                PIC  X          VALUE ';'.  
                  15 ZEITRAUM-VON          PIC  X(010).                
                  15 FILLER                PIC  X          VALUE ';'.  
                  15 ZEITRAUM-BIS          PIC  X(010).                
                  15 FILLER                PIC  X          VALUE ';'.
                  15 KOSTENPOSITION        PIC  X(179).
                  15 FILLER                PIC  X          VALUE ';'.   
                  15 BETRAG                PIC X(011).                  
                  15 FILLER                PIC  X          VALUE ';'.   
                  15 BETRAG-PRZ            PIC X(007).                  
                  15 FILLER                PIC  X          VALUE ';'.   
                  15 KOSTENKATEGORIE       PIC  X(081).                 
      *****************************************************************
      *---------------------- ENDE XXXX0000 --------------------------* 
      *****************************************************************
