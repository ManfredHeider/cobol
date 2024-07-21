      *****************************************************************         
      *    COPY-MEMBER: DRKI0685                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0685                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         29.12.2021 / 15:20:21 / T13EX39            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI0685                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0685.                                                         
           05  PI-DRKI0685-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0685-BASGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0685-BASVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0685.                                                         
           05  PO-DRKI0685-RC                     PIC  X(003).                  
               88 PO-DRKI0685-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0685-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI0685-NI.                                                  
               10 NI-DRKI0685-TAB.                                              
                  15 DRKI0685-ELE                                               
                     PIC S9(04) COMP OCCURS 90 TIMES.                           
               10 NI-DRKI0685 REDEFINES NI-DRKI0685-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI0685-NI-ABB-VERSION   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASABTRETUNGBLZ   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASANFTILGUNG   PIC S9(4) COMP.                  
             15 PO-DRKI0685-NI-BASANNURATE   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASBEITRAG   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSBRWRTNGSZHL   PIC S9(4) COMP.                   
             15 PO-DRKI0685-NI-BASBLANKODARLKZ   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASBONUSAKTUELL   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASBONUSART   PIC S9(4) COMP.                    
             15 PO-DRKI0685-N-BSBNSBGDTM   PIC S9(4) COMP.                      
             15 PO-DRKI0685-NI-BASBONUSFAKTOR   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASBONUSVORJAHR   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASBWZVORJAHR   PIC S9(4) COMP.                  
             15 PO-DRKI0685-N-BSDRLHNKTKZ   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASDARLHOEHE   PIC S9(4) COMP.                   
             15 PO-DRKI0685-NI-BASDARLSALDO   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSDRLZSGMTRF   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASDATAFBEGINN   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASDATAGEING   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSDTGHBZNSBGR   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSDTKMLWHNBPR   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSDTLTZTRKZ   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSDTNVBSCHN   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASDATSPERRFR   PIC S9(4) COMP.                  
             15 PO-DRKI0685-N-BSDTWHNBPRM   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSDTZNSFSTB   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSDTZNSFSTBS   PIC S9(4) COMP.                    
             15 PO-DRKI0685-N-BSDTZTLSS   PIC S9(4) COMP.                       
             15 PO-DRKI0685-N-BSDRZLGKZ   PIC S9(4) COMP.                       
             15 PO-DRKI0685-N-BSDSGPRZNT   PIC S9(4) COMP.                      
             15 PO-DRKI0685-N-BSNZBTRGJHR   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSNZBTRGMN   PIC S9(4) COMP.                      
             15 PO-DRKI0685-N-BSNZGDTMJHR   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSNZGDTMMN   PIC S9(4) COMP.                      
             15 PO-DRKI0685-NI-BASEINZUGDYNAM   PIC S9(4) COMP.                 
             15 PO-DRKI0685-N-BSNDBNDFRST   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSNDSTRLFZ   PIC S9(4) COMP.                      
             15 PO-DRKI0685-N-BSRHHZBPRZ   PIC S9(4) COMP.                      
             15 PO-DRKI0685-N-BSFHLBTRMNNSP   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSFNNZRNGKZ   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASFINPRODSCHL   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASFRONTTEC-KNR   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASFRONTTEC-KTY   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASGUTHABEN   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASIMMOFITKZ   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSKNTSTNDKZD   PIC S9(4) COMP.                    
             15 PO-DRKI0685-N-BSKMLWHNBPR   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASLAUFZEITFIN   PIC S9(4) COMP.                 
             15 PO-DRKI0685-N-BSLTZTDTKT   PIC S9(4) COMP.                      
             15 PO-DRKI0685-NI-BASLETZTEZUTEIL   PIC S9(4) COMP.                
             15 PO-DRKI0685-N-BSMXZNSKPTL   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSMHRZTLNG   PIC S9(4) COMP.                      
             15 PO-DRKI0685-N-BSMTKTNKZ   PIC S9(4) COMP.                       
             15 PO-DRKI0685-N-BSMNBWRTZHL   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSMNDSTNSPRZ   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASNRLETZTERKAZ   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASONLKIART-KNR   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASONLKIART-KTY   PIC S9(4) COMP.                
             15 PO-DRKI0685-N-BSNLKPRD-KNR   PIC S9(4) COMP.                    
             15 PO-DRKI0685-N-BSNLKPRD-KTY   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASPASSWVEREINB   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASPEAUSZAHLART   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASPRODSCHL   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASPRODUKTID   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSSCHLSSDTM   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSSNDRTLGNG   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSSNDRZHLNG   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASSONDTILGJAHR   PIC S9(4) COMP.                
             15 PO-DRKI0685-N-BSSPRZLFRB   PIC S9(4) COMP.                      
             15 PO-DRKI0685-NI-BASSTEUERLFZDAT   PIC S9(4) COMP.                
             15 PO-DRKI0685-N-BSTRFMSTBTRG   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASTUBAKTION   PIC S9(4) COMP.                   
             15 PO-DRKI0685-N-BSVRNWCHSKZ   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASVERMITTELTKZ   PIC S9(4) COMP.                
             15 PO-DRKI0685-N-BSVRTRGSSMM   PIC S9(4) COMP.                     
             15 PO-DRKI0685-N-BSVLPRSNLTKZ   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASVWLKZ   PIC S9(4) COMP.                       
             15 PO-DRKI0685-NI-BASWERBVERBOTKZ   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASWOHNSPARKEY   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASWOPANSPRUCH   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASWOPKZ   PIC S9(4) COMP.                       
             15 PO-DRKI0685-NI-BASZIELTILGUNG   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASZINSEFFEKTIV   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASZINSLFDJAHR   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASZINSNOMINAL   PIC S9(4) COMP.                 
             15 PO-DRKI0685-N-BSZNSPLSGNR   PIC S9(4) COMP.                     
             15 PO-DRKI0685-NI-BASZINSSATZ   PIC S9(4) COMP.                    
             15 PO-DRKI0685-NI-BASZINSVORTRAG   PIC S9(4) COMP.                 
             15 PO-DRKI0685-NI-BASZUTEILANWART   PIC S9(4) COMP.                
             15 PO-DRKI0685-NI-BASZUTEILANWDAT   PIC S9(4) COMP.                
END   *                                                                         
                                                                                
           05  PO-DRKI0685-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0685-ABB-VERSION   PIC  X(0007).                         
             10 PO-DRKI0685-BASABTRETUNGBLZ   PIC S9(9) COMP.                   
             10 PO-DRKI0685-BASANFTILGUNG   PIC S9(3)V9(2) COMP-3.              
             10 PO-DRKI0685-BASANNURATE   PIC S9(9)V9(2) COMP-3.                
             10 PO-DRKI0685-BASBEITRAG   PIC S9(9)V9(2) COMP-3.                 
             10 PO-DRKI0685-BASBERWERTUNGSZAHL   PIC S9(3)V9(5) COMP-3.         
             10 PO-DRKI0685-BASBLANKODARLKZ   PIC  X(0001).                     
             10 PO-DRKI0685-BASBONUSAKTUELL   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI0685-BASBONUSART   PIC  X(0002).                         
             10 PO-DRKI0685-BASBONUSBEGDATUM   PIC  X(010).                     
             10 PO-DRKI0685-BASBONUSFAKTOR   PIC S9(2)V9(2) COMP-3.             
             10 PO-DRKI0685-BASBONUSVORJAHR   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI0685-BASBWZVORJAHR   PIC S9(3)V9(5) COMP-3.              
             10 PO-DRKI0685-BASDARLEHNAKTEKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASDARLHOEHE   PIC S9(9)V9(2) COMP-3.               
             10 PO-DRKI0685-BASDARLSALDO   PIC S9(9)V9(2) COMP-3.               
             10 PO-DRKI0685-BASDARLZSGEMTARIF   PIC S9(3)V9(2) COMP-3.          
             10 PO-DRKI0685-BASDATAFBEGINN   PIC  X(010).                       
             10 PO-DRKI0685-BASDATAGEING   PIC  X(010).                         
             10 PO-DRKI0685-BASDATGHBZINSBEGR   PIC  X(010).                    
             10 PO-DRKI0685-BASDATKUMULWOHNBPR   PIC  X(010).                   
             10 PO-DRKI0685-BASDATLETZTERKAZ   PIC  X(010).                     
             10 PO-DRKI0685-BASDATNVBESCHEIN   PIC  X(010).                     
             10 PO-DRKI0685-BASDATSPERRFR   PIC  X(010).                        
             10 PO-DRKI0685-BASDATWOHNBAUPRAEM   PIC  X(010).                   
             10 PO-DRKI0685-BASDATZINSFESTAB   PIC  X(010).                     
             10 PO-DRKI0685-BASDATZINSFESTBIS   PIC  X(010).                    
             10 PO-DRKI0685-BASDATZUTEILAUSSI   PIC  X(010).                    
             10 PO-DRKI0685-BASDAUERZULAGEKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASDISAGIOPROZENT   PIC S9(3)V9(2) COMP-3.          
             10 PO-DRKI0685-BASEINZBETRAGJHR   PIC S9(7)V9(2) COMP-3.           
             10 PO-DRKI0685-BASEINZBETRAGMON   PIC S9(7)V9(2) COMP-3.           
             10 PO-DRKI0685-BASEINZGDATUMJHR   PIC  X(010).                     
             10 PO-DRKI0685-BASEINZGDATUMMON   PIC  X(010).                     
             10 PO-DRKI0685-BASEINZUGDYNAM   PIC S9(3)V9(2) COMP-3.             
             10 PO-DRKI0685-BASENDEBINDFRIST   PIC  X(010).                     
             10 PO-DRKI0685-BASENDESTEUERLFZ   PIC  X(010).                     
             10 PO-DRKI0685-BASERHOEHEZBPROZ   PIC S9(3)V9(2) COMP-3.           
             10 PO-DRKI0685-BASFEHLBETRMAWKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASFEHLBETRMINANSP   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI0685-BASFINANZIERNGKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASFINPRODSCHL   PIC S9(4) COMP.                    
             10 PO-DRKI0685-BASFRONTTEC-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI0685-BASFRONTTEC-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI0685-BASGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PO-DRKI0685-BASIMMOFITKZ   PIC  X(0001).                        
             10 PO-DRKI0685-BASKONTOSTANDKZID   PIC S9(4) COMP.                 
             10 PO-DRKI0685-BASKUMULWOHNBAUPR   PIC S9(9)V9(2) COMP-3.          
             10 PO-DRKI0685-BASKUMWOHNBPRWKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASLAUFZEITFIN   PIC S9(4) COMP.                    
             10 PO-DRKI0685-BASLETZTEDATAKTU   PIC  X(010).                     
             10 PO-DRKI0685-BASLETZTEZUTEIL   PIC  X(010).                      
             10 PO-DRKI0685-BASMAXZINSKAPITAL   PIC S9(9)V9(2) COMP-3.          
             10 PO-DRKI0685-BASMEHRZUTEILUNG   PIC S9(7)V9(2) COMP-3.           
             10 PO-DRKI0685-BASMIETKAUTIONKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASMINBEWERTZAHL   PIC S9(3)V9(3) COMP-3.           
             10 PO-DRKI0685-BASMINDESTANSPROZ   PIC S9(3)V9(2) COMP-3.          
             10 PO-DRKI0685-BASNRLETZTERKAZ   PIC  X(0007).                     
             10 PO-DRKI0685-BASONLKIART-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI0685-BASONLKIART-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI0685-BASONLKIPROD-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI0685-BASONLKIPROD-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI0685-BASPASSWVEREINB   PIC  X(0001).                     
             10 PO-DRKI0685-BASPEAUSZAHLART   PIC  X(0001).                     
             10 PO-DRKI0685-BASPRODSCHL   PIC  X(0002).                         
             10 PO-DRKI0685-BASPRODUKTID   PIC S9(4) COMP.                      
             10 PO-DRKI0685-BASSCHLIESSDATUM   PIC  X(010).                     
             10 PO-DRKI0685-BASSONDERTILGUNG   PIC S9(3)V9(2) COMP-3.           
             10 PO-DRKI0685-BASSONDERZAHLUNG   PIC S9(3)V9(2) COMP-3.           
             10 PO-DRKI0685-BASSONDTILGJAHR   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI0685-BASSPARZULFREIAB   PIC  X(010).                     
             10 PO-DRKI0685-BASSTEUERBESCHEIKZ   PIC  X(0001).                  
             10 PO-DRKI0685-BASSTEUERLFZDAT   PIC  X(010).                      
             10 PO-DRKI0685-BASTARIFUMSTBTRG   PIC S9(5)V9(2) COMP-3.           
             10 PO-DRKI0685-BASTARIFVARINTIII   PIC  X(0001).                   
             10 PO-DRKI0685-BASTUBAKTION   PIC  X(0001).                        
             10 PO-DRKI0685-BASVARIANWECHSKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASVERMITTELTKZ   PIC  X(0001).                     
             10 PO-DRKI0685-BASVERTRAGSSUMME   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI0685-BASVLPERSONALTKZ   PIC  X(0001).                    
             10 PO-DRKI0685-BASVTSUMMEWKZ   PIC  X(0001).                       
             10 PO-DRKI0685-BASVWLKZ   PIC  X(0001).                            
             10 PO-DRKI0685-BASWERBVERBOTKZ   PIC  X(0001).                     
             10 PO-DRKI0685-BASWOHNSPARKEY   PIC S9(4) COMP.                    
             10 PO-DRKI0685-BASWOPANSPRUCH   PIC S9(7)V9(2) COMP-3.             
             10 PO-DRKI0685-BASWOPKZ   PIC  X(0001).                            
             10 PO-DRKI0685-BASZIELTILGUNG   PIC S9(3)V9(2) COMP-3.             
             10 PO-DRKI0685-BASZINSEFFEKTIV   PIC S9(3)V9(3) COMP-3.            
             10 PO-DRKI0685-BASZINSLFDJAHR   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI0685-BASZINSNOMINAL   PIC S9(3)V9(3) COMP-3.             
             10 PO-DRKI0685-BASZINSPLUSGENER   PIC S9(2)V COMP-3.               
             10 PO-DRKI0685-BASZINSSATZ   PIC S9(3)V9(2) COMP-3.                
             10 PO-DRKI0685-BASZINSVORTRAG   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI0685-BASZUTEILANWART   PIC  X(010).                      
             10 PO-DRKI0685-BASZUTEILANWDAT   PIC  X(010).                      
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
