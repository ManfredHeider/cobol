      *****************************************************************         
      *    COPY-MEMBER: DRKI0687                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0687                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         29.12.2021 / 15:20:31 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI0687                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0687.                                                         
           05  PI-DRKI0687-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0687-BASGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0687-BASVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0687-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0687-NI-ABB-VERSION      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASABTRETUNGBLZ  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASANFTILGUNG    PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASANNURATE      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBEITRAG       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSBRWRTNGSZHL     PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBLANKODARLKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBONUSAKTUELL  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBONUSART      PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSBNSBGDTM        PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBONUSFAKTOR   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBONUSVORJAHR  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASBWZVORJAHR    PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDRLHNKTKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASDARLHOEHE     PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASDARLSALDO     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDRLZSGMTRF      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASDATAFBEGINN   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASDATAGEING     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTGHBZNSBGR     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTKMLWHNBPR     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTLTZTRKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTNVBSCHN       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASDATSPERRFR    PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTWHNBPRM       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTZNSFSTB       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTZNSFSTBS      PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDTZTLSS         PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDRZLGKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSDSGPRZNT        PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNZBTRGJHR       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNZBTRGMN        PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNZGDTMJHR       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNZGDTMMN        PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASEINZUGDYNAM   PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNDBNDFRST       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNDSTRLFZ        PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSRHHZBPRZ        PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSFHLBTRMWKZ      PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSFHLBTRMNNSP     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSFNNZRNGKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASFINPRODSCHL   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASFRONTTEC-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASFRONTTEC-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASIMMOFITKZ     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSKNTSTNDKZD      PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSKMLWHNBPR       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSKMWHNBPRWKZ     PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASLAUFZEITFIN   PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSLTZTDTKT        PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASLETZTEZUTEIL  PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSMXZNSKPTL       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSMHRZTLNG        PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSMTKTNKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSMNBWRTZHL       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSMNDSTNSPRZ      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASNRLETZTERKAZ  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASONLKIART-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASONLKIART-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNLKPRD-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSNLKPRD-KTY      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASPASSWVEREINB  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASPEAUSZAHLART  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASPRODSCHL      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASPRODUKTID     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSSCHLSSDTM       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSSNDRTLGNG       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSSNDRZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASSONDTILGJAHR  PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSSPRZLFRB        PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSSTRBSCHKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASSTEUERLFZDAT  PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSTRFMSTBTRG      PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSTRFVRNT         PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASTUBAKTION     PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSVRNWCHSKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASVERMITTELTKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSVRTRGSSMM       PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSVLPRSNLTKZ      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASVTSUMMEWKZ    PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASVWLKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASWERBVERBOTKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASWOHNSPARKEY   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASWOPANSPRUCH   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASWOPKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZIELTILGUNG   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZINSEFFEKTIV  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZINSLFDJAHR   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZINSNOMINAL   PIC S9(4) COMP.                 
             15 PI-DRKI0687-N-BSZNSPLSGNR       PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZINSSATZ      PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZINSVORTRAG   PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZUTEILANWART  PIC S9(4) COMP.                 
             15 PI-DRKI0687-NI-BASZUTEILANWDAT  PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0687-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0687-ABB-VERSION   PIC  X(0007).                         
             10 PI-DRKI0687-BASABTRETUNGBLZ   PIC S9(9) COMP.                   
             10 PI-DRKI0687-BASANFTILGUNG   PIC S9(3)V9(2) COMP-3.              
             10 PI-DRKI0687-BASANNURATE   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI0687-BASBEITRAG   PIC S9(9)V9(2) COMP-3.                 
             10 PI-DRKI0687-BASBERWERTUNGSZAHL   PIC S9(3)V9(5) COMP-3.         
             10 PI-DRKI0687-BASBLANKODARLKZ   PIC  X(0001).                     
             10 PI-DRKI0687-BASBONUSAKTUELL   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0687-BASBONUSART   PIC  X(0002).                         
             10 PI-DRKI0687-BASBONUSBEGDATUM   PIC  X(010).                     
             10 PI-DRKI0687-BASBONUSFAKTOR   PIC S9(2)V9(2) COMP-3.             
             10 PI-DRKI0687-BASBONUSVORJAHR   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0687-BASBWZVORJAHR   PIC S9(3)V9(5) COMP-3.              
             10 PI-DRKI0687-BASDARLEHNAKTEKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASDARLHOEHE   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI0687-BASDARLSALDO   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI0687-BASDARLZSGEMTARIF   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI0687-BASDATAFBEGINN   PIC  X(010).                       
             10 PI-DRKI0687-BASDATAGEING   PIC  X(010).                         
             10 PI-DRKI0687-BASDATGHBZINSBEGR   PIC  X(010).                    
             10 PI-DRKI0687-BASDATKUMULWOHNBPR   PIC  X(010).                   
             10 PI-DRKI0687-BASDATLETZTERKAZ   PIC  X(010).                     
             10 PI-DRKI0687-BASDATNVBESCHEIN   PIC  X(010).                     
             10 PI-DRKI0687-BASDATSPERRFR   PIC  X(010).                        
             10 PI-DRKI0687-BASDATWOHNBAUPRAEM   PIC  X(010).                   
             10 PI-DRKI0687-BASDATZINSFESTAB   PIC  X(010).                     
             10 PI-DRKI0687-BASDATZINSFESTBIS   PIC  X(010).                    
             10 PI-DRKI0687-BASDATZUTEILAUSSI   PIC  X(010).                    
             10 PI-DRKI0687-BASDAUERZULAGEKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASDISAGIOPROZENT   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI0687-BASEINZBETRAGJHR   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI0687-BASEINZBETRAGMON   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI0687-BASEINZGDATUMJHR   PIC  X(010).                     
             10 PI-DRKI0687-BASEINZGDATUMMON   PIC  X(010).                     
             10 PI-DRKI0687-BASEINZUGDYNAM   PIC S9(3)V9(2) COMP-3.             
             10 PI-DRKI0687-BASENDEBINDFRIST   PIC  X(010).                     
             10 PI-DRKI0687-BASENDESTEUERLFZ   PIC  X(010).                     
             10 PI-DRKI0687-BASERHOEHEZBPROZ   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0687-BASFEHLBETRMAWKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASFEHLBETRMINANSP   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI0687-BASFINANZIERNGKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASFINPRODSCHL   PIC S9(4) COMP.                    
             10 PI-DRKI0687-BASFRONTTEC-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0687-BASFRONTTEC-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0687-BASGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI0687-BASIMMOFITKZ   PIC  X(0001).                        
             10 PI-DRKI0687-BASKONTOSTANDKZID   PIC S9(4) COMP.                 
             10 PI-DRKI0687-BASKUMULWOHNBAUPR   PIC S9(9)V9(2) COMP-3.          
             10 PI-DRKI0687-BASKUMWOHNBPRWKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASLAUFZEITFIN   PIC S9(4) COMP.                    
             10 PI-DRKI0687-BASLETZTEDATAKTU   PIC  X(010).                     
             10 PI-DRKI0687-BASLETZTEZUTEIL   PIC  X(010).                      
             10 PI-DRKI0687-BASMAXZINSKAPITAL   PIC S9(9)V9(2) COMP-3.          
             10 PI-DRKI0687-BASMEHRZUTEILUNG   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI0687-BASMIETKAUTIONKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASMINBEWERTZAHL   PIC S9(3)V9(3) COMP-3.           
             10 PI-DRKI0687-BASMINDESTANSPROZ   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI0687-BASNRLETZTERKAZ   PIC  X(0007).                     
             10 PI-DRKI0687-BASONLKIART-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0687-BASONLKIART-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0687-BASONLKIPROD-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0687-BASONLKIPROD-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0687-BASPASSWVEREINB   PIC  X(0001).                     
             10 PI-DRKI0687-BASPEAUSZAHLART   PIC  X(0001).                     
             10 PI-DRKI0687-BASPRODSCHL   PIC  X(0002).                         
             10 PI-DRKI0687-BASPRODUKTID   PIC S9(4) COMP.                      
             10 PI-DRKI0687-BASSCHLIESSDATUM   PIC  X(010).                     
             10 PI-DRKI0687-BASSONDERTILGUNG   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0687-BASSONDERZAHLUNG   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0687-BASSONDTILGJAHR   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0687-BASSPARZULFREIAB   PIC  X(010).                     
             10 PI-DRKI0687-BASSTEUERBESCHEIKZ   PIC  X(0001).                  
             10 PI-DRKI0687-BASSTEUERLFZDAT   PIC  X(010).                      
             10 PI-DRKI0687-BASTARIFUMSTBTRG   PIC S9(5)V9(2) COMP-3.           
             10 PI-DRKI0687-BASTARIFVARINTIII   PIC  X(0001).                   
             10 PI-DRKI0687-BASTUBAKTION   PIC  X(0001).                        
             10 PI-DRKI0687-BASVARIANWECHSKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASVERMITTELTKZ   PIC  X(0001).                     
             10 PI-DRKI0687-BASVERTRAGSSUMME   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI0687-BASVLPERSONALTKZ   PIC  X(0001).                    
             10 PI-DRKI0687-BASVTSUMMEWKZ   PIC  X(0001).                       
             10 PI-DRKI0687-BASVWLKZ   PIC  X(0001).                            
             10 PI-DRKI0687-BASWERBVERBOTKZ   PIC  X(0001).                     
             10 PI-DRKI0687-BASWOHNSPARKEY   PIC S9(4) COMP.                    
             10 PI-DRKI0687-BASWOPANSPRUCH   PIC S9(7)V9(2) COMP-3.             
             10 PI-DRKI0687-BASWOPKZ   PIC  X(0001).                            
             10 PI-DRKI0687-BASZIELTILGUNG   PIC S9(3)V9(2) COMP-3.             
             10 PI-DRKI0687-BASZINSEFFEKTIV   PIC S9(3)V9(3) COMP-3.            
             10 PI-DRKI0687-BASZINSLFDJAHR   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI0687-BASZINSNOMINAL   PIC S9(3)V9(3) COMP-3.             
             10 PI-DRKI0687-BASZINSPLUSGENER   PIC S9(2)V COMP-3.               
             10 PI-DRKI0687-BASZINSSATZ   PIC S9(3)V9(2) COMP-3.                
             10 PI-DRKI0687-BASZINSVORTRAG   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI0687-BASZUTEILANWART   PIC  X(010).                      
             10 PI-DRKI0687-BASZUTEILANWDAT   PIC  X(010).                      
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0687-NI1.                                                     
           05 PI-DRKI0687-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 95.                                    
       01  PI-DRKI0687-NI7.                                                     
           05 PI-DRKI0687-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 95.                                    
      *                                                                         
       01  PO-DRKI0687.                                                         
           05  PO-DRKI0687-RC                     PIC  X(003).                  
               88 PO-DRKI0687-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0687-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
