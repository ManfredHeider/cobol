      *****************************************************************         
      *    COPY-MEMBER: DRKI0686                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0686                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         29.12.2021 / 15:20:26 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI0686                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0686.                                                         
           05  PI-DRKI0686-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0686-BASGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0686-BASVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0686-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0686-NI-ABB-VERSION      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASABTRETUNGBLZ  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASANFTILGUNG    PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASANNURATE      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBEITRAG       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSBRWRTNGSZHL     PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBLANKODARLKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBONUSAKTUELL  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBONUSART      PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSBNSBGDTM        PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBONUSFAKTOR   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBONUSVORJAHR  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASBWZVORJAHR    PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDRLHNKTKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASDARLHOEHE     PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASDARLSALDO     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDRLZSGMTRF      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASDATAFBEGINN   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASDATAGEING     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTGHBZNSBGR     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTKMLWHNBPR     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTLTZTRKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTNVBSCHN       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASDATSPERRFR    PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTWHNBPRM       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTZNSFSTB       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTZNSFSTBS      PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDTZTLSS         PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDRZLGKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSDSGPRZNT        PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNZBTRGJHR       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNZBTRGMN        PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNZGDTMJHR       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNZGDTMMN        PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASEINZUGDYNAM   PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNDBNDFRST       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNDSTRLFZ        PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSRHHZBPRZ        PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSFHLBTRMNNSP     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSFNNZRNGKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASFINPRODSCHL   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASFRONTTEC-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASFRONTTEC-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASIMMOFITKZ     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSKNTSTNDKZD      PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSKMLWHNBPR       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASLAUFZEITFIN   PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSLTZTDTKT        PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASLETZTEZUTEIL  PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSMXZNSKPTL       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSMHRZTLNG        PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSMTKTNKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSMNBWRTZHL       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSMNDSTNSPRZ      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASNRLETZTERKAZ  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASONLKIART-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASONLKIART-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNLKPRD-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSNLKPRD-KTY      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASPASSWVEREINB  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASPEAUSZAHLART  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASPRODSCHL      PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSSCHLSSDTM       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSSNDRTLGNG       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSSNDRZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASSONDTILGJAHR  PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSSPRZLFRB        PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASSTEUERLFZDAT  PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSTRFMSTBTRG      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASTUBAKTION     PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSVRNWCHSKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASVERMITTELTKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSVRTRGSSMM       PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSVLPRSNLTKZ      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASVWLKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASWERBVERBOTKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASWOHNSPARKEY   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASWOPANSPRUCH   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASWOPKZ         PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZIELTILGUNG   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZINSEFFEKTIV  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZINSLFDJAHR   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZINSNOMINAL   PIC S9(4) COMP.                 
             15 PI-DRKI0686-N-BSZNSPLSGNR       PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZINSSATZ      PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZINSVORTRAG   PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZUTEILANWART  PIC S9(4) COMP.                 
             15 PI-DRKI0686-NI-BASZUTEILANWDAT  PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0686-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0686-ABB-VERSION   PIC  X(0007).                         
             10 PI-DRKI0686-BASABTRETUNGBLZ   PIC S9(9) COMP.                   
             10 PI-DRKI0686-BASANFTILGUNG   PIC S9(3)V9(2) COMP-3.              
             10 PI-DRKI0686-BASANNURATE   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI0686-BASBEITRAG   PIC S9(9)V9(2) COMP-3.                 
             10 PI-DRKI0686-BASBERWERTUNGSZAHL   PIC S9(3)V9(5) COMP-3.         
             10 PI-DRKI0686-BASBLANKODARLKZ   PIC  X(0001).                     
             10 PI-DRKI0686-BASBONUSAKTUELL   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0686-BASBONUSART   PIC  X(0002).                         
             10 PI-DRKI0686-BASBONUSBEGDATUM   PIC  X(010).                     
             10 PI-DRKI0686-BASBONUSFAKTOR   PIC S9(2)V9(2) COMP-3.             
             10 PI-DRKI0686-BASBONUSVORJAHR   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0686-BASBWZVORJAHR   PIC S9(3)V9(5) COMP-3.              
             10 PI-DRKI0686-BASDARLEHNAKTEKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASDARLHOEHE   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI0686-BASDARLSALDO   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI0686-BASDARLZSGEMTARIF   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI0686-BASDATAFBEGINN   PIC  X(010).                       
             10 PI-DRKI0686-BASDATAGEING   PIC  X(010).                         
             10 PI-DRKI0686-BASDATGHBZINSBEGR   PIC  X(010).                    
             10 PI-DRKI0686-BASDATKUMULWOHNBPR   PIC  X(010).                   
             10 PI-DRKI0686-BASDATLETZTERKAZ   PIC  X(010).                     
             10 PI-DRKI0686-BASDATNVBESCHEIN   PIC  X(010).                     
             10 PI-DRKI0686-BASDATSPERRFR   PIC  X(010).                        
             10 PI-DRKI0686-BASDATWOHNBAUPRAEM   PIC  X(010).                   
             10 PI-DRKI0686-BASDATZINSFESTAB   PIC  X(010).                     
             10 PI-DRKI0686-BASDATZINSFESTBIS   PIC  X(010).                    
             10 PI-DRKI0686-BASDATZUTEILAUSSI   PIC  X(010).                    
             10 PI-DRKI0686-BASDAUERZULAGEKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASDISAGIOPROZENT   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI0686-BASEINZBETRAGJHR   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI0686-BASEINZBETRAGMON   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI0686-BASEINZGDATUMJHR   PIC  X(010).                     
             10 PI-DRKI0686-BASEINZGDATUMMON   PIC  X(010).                     
             10 PI-DRKI0686-BASEINZUGDYNAM   PIC S9(3)V9(2) COMP-3.             
             10 PI-DRKI0686-BASENDEBINDFRIST   PIC  X(010).                     
             10 PI-DRKI0686-BASENDESTEUERLFZ   PIC  X(010).                     
             10 PI-DRKI0686-BASERHOEHEZBPROZ   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0686-BASFEHLBETRMAWKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASFEHLBETRMINANSP   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI0686-BASFINANZIERNGKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASFINPRODSCHL   PIC S9(4) COMP.                    
             10 PI-DRKI0686-BASFRONTTEC-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0686-BASFRONTTEC-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0686-BASGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI0686-BASIMMOFITKZ   PIC  X(0001).                        
             10 PI-DRKI0686-BASKONTOSTANDKZID   PIC S9(4) COMP.                 
             10 PI-DRKI0686-BASKUMULWOHNBAUPR   PIC S9(9)V9(2) COMP-3.          
             10 PI-DRKI0686-BASKUMWOHNBPRWKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASLAUFZEITFIN   PIC S9(4) COMP.                    
             10 PI-DRKI0686-BASLETZTEDATAKTU   PIC  X(010).                     
             10 PI-DRKI0686-BASLETZTEZUTEIL   PIC  X(010).                      
             10 PI-DRKI0686-BASMAXZINSKAPITAL   PIC S9(9)V9(2) COMP-3.          
             10 PI-DRKI0686-BASMEHRZUTEILUNG   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI0686-BASMIETKAUTIONKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASMINBEWERTZAHL   PIC S9(3)V9(3) COMP-3.           
             10 PI-DRKI0686-BASMINDESTANSPROZ   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI0686-BASNRLETZTERKAZ   PIC  X(0007).                     
             10 PI-DRKI0686-BASONLKIART-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0686-BASONLKIART-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0686-BASONLKIPROD-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0686-BASONLKIPROD-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0686-BASPASSWVEREINB   PIC  X(0001).                     
             10 PI-DRKI0686-BASPEAUSZAHLART   PIC  X(0001).                     
             10 PI-DRKI0686-BASPRODSCHL   PIC  X(0002).                         
             10 PI-DRKI0686-BASPRODUKTID   PIC S9(4) COMP.                      
             10 PI-DRKI0686-BASSCHLIESSDATUM   PIC  X(010).                     
             10 PI-DRKI0686-BASSONDERTILGUNG   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0686-BASSONDERZAHLUNG   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0686-BASSONDTILGJAHR   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0686-BASSPARZULFREIAB   PIC  X(010).                     
             10 PI-DRKI0686-BASSTEUERBESCHEIKZ   PIC  X(0001).                  
             10 PI-DRKI0686-BASSTEUERLFZDAT   PIC  X(010).                      
             10 PI-DRKI0686-BASTARIFUMSTBTRG   PIC S9(5)V9(2) COMP-3.           
             10 PI-DRKI0686-BASTARIFVARINTIII   PIC  X(0001).                   
             10 PI-DRKI0686-BASTUBAKTION   PIC  X(0001).                        
             10 PI-DRKI0686-BASVARIANWECHSKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASVERMITTELTKZ   PIC  X(0001).                     
             10 PI-DRKI0686-BASVERTRAGSSUMME   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI0686-BASVLPERSONALTKZ   PIC  X(0001).                    
             10 PI-DRKI0686-BASVTSUMMEWKZ   PIC  X(0001).                       
             10 PI-DRKI0686-BASVWLKZ   PIC  X(0001).                            
             10 PI-DRKI0686-BASWERBVERBOTKZ   PIC  X(0001).                     
             10 PI-DRKI0686-BASWOHNSPARKEY   PIC S9(4) COMP.                    
             10 PI-DRKI0686-BASWOPANSPRUCH   PIC S9(7)V9(2) COMP-3.             
             10 PI-DRKI0686-BASWOPKZ   PIC  X(0001).                            
             10 PI-DRKI0686-BASZIELTILGUNG   PIC S9(3)V9(2) COMP-3.             
             10 PI-DRKI0686-BASZINSEFFEKTIV   PIC S9(3)V9(3) COMP-3.            
             10 PI-DRKI0686-BASZINSLFDJAHR   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI0686-BASZINSNOMINAL   PIC S9(3)V9(3) COMP-3.             
             10 PI-DRKI0686-BASZINSPLUSGENER   PIC S9(2)V COMP-3.               
             10 PI-DRKI0686-BASZINSSATZ   PIC S9(3)V9(2) COMP-3.                
             10 PI-DRKI0686-BASZINSVORTRAG   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI0686-BASZUTEILANWART   PIC  X(010).                      
             10 PI-DRKI0686-BASZUTEILANWDAT   PIC  X(010).                      
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0686-NI1.                                                     
           05 PI-DRKI0686-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 89.                                
       01  PI-DRKI0686-NI7.                                                     
           05 PI-DRKI0686-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 89.                                
                                                                                
      *                                                                         
       01  PO-DRKI0686.                                                         
           05  PO-DRKI0686-RC                     PIC  X(003).                  
               88 PO-DRKI0686-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI0686-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
