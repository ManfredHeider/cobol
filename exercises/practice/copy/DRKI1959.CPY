      *****************************************************************         
      *    COPY-MEMBER: DRKI1959                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1959                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         11.01.2023 / 14:24:02 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI1959                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1959.                                                         
           05  PI-DRKI1959-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1959-VERVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI1959-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI1959-NI-VERABSCHLUSSGRU  PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRBSCHLSSVBNR     PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRNDRNGSDTM       PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRDTPRDLSCH       PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERDATVERTRAEND  PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VEREHELEUTEKZ    PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VEREIGENVERMKZ   PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERFIRMENVTGKZ   PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERFRALFDNR      PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRFRPRSNND        PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRFRSTFTRG        PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRFRMDVRTRGKZ     PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERGEBUENDELTKZ  PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRGSMTBTRG        PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERGESBEITRBKZ   PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERGESBEITRWKZ   PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRNTRNVRTRNR      PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERMANDANTNR     PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRMNTDRHPTF       PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERNACHLASSKZ    PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERNOTIZID       PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRPRTGSLLSCHD     PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERPRUEFZIFFVNR  PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERREMDVERTRGES  PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRSTRNGRND-KNR    PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRSTRNGRND-KTY    PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERSTORNODATUM   PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRVRTRGSBLF       PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRVRTRGSBGNN      PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRVRTRGSNMMR      PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRVRTRZSTNDDT     PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRVRTRZSTNDD      PIC S9(4) COMP.                 
             15 PI-DRKI1959-NI-VERVSNRZUSATZ    PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRZHLNGSRTD       PIC S9(4) COMP.                 
             15 PI-DRKI1959-N-VRZHLNGSWSD       PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI1959-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI1959-VERABSCHLUSSGRUND.                                  
                49 PI-DRKI1959-VRBSCHLSSGRND-LEN PIC S9(4) COMP.                
                49 PI-DRKI1959-VRBSCHLSSGRND-TEXT PIC X(30).                    
             10 PI-DRKI1959-VERABSCHLUSSVBNR   PIC S9(9) COMP.                  
             10 PI-DRKI1959-VERAENDERUNGSDATUM   PIC  X(0026).                  
             10 PI-DRKI1959-VERDATPRODLOESCH   PIC  X(010).                     
             10 PI-DRKI1959-VERDATVERTRAEND   PIC  X(010).                      
             10 PI-DRKI1959-VEREHELEUTEKZ   PIC  X(0001).                       
             10 PI-DRKI1959-VEREIGENVERMKZ   PIC  X(0001).                      
             10 PI-DRKI1959-VERFIRMENVTGKZ   PIC  X(0001).                      
             10 PI-DRKI1959-VERFRALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI1959-VERFRAPERSONENID   PIC  X(0013).                    
             10 PI-DRKI1959-VERFREISTAUFTRAG   PIC  X(0001).                    
             10 PI-DRKI1959-VERFREMDVERTRAGKZ   PIC  X(0001).                   
             10 PI-DRKI1959-VERGEBUENDELTKZ   PIC  X(0001).                     
             10 PI-DRKI1959-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI1959-VERGESBEITRBKZ   PIC  X(0001).                      
             10 PI-DRKI1959-VERGESBEITRWKZ   PIC  X(0001).                      
             10 PI-DRKI1959-VERINTERNEVERTRNR   PIC S9(9) COMP.                 
             10 PI-DRKI1959-VERMANDANTNR   PIC S9(9) COMP.                      
             10 PI-DRKI1959-VERMONATDERHAUPTF   PIC S9(4) COMP.                 
             10 PI-DRKI1959-VERNACHLASSKZ   PIC  X(0001).                       
             10 PI-DRKI1959-VERNOTIZID   PIC  X(0013).                          
             10 PI-DRKI1959-VERPARTGESELLSCHID   PIC S9(4) COMP.                
             10 PI-DRKI1959-VERPRUEFZIFFVNR   PIC S9(4) COMP.                   
             10 PI-DRKI1959-VERREMDVERTRGESNAM.                                 
                49 PI-DRKI1959-VRRMDVRTRGSNM-LEN PIC S9(4) COMP.                
                49 PI-DRKI1959-VRRMDVRTRGSNM-TEXT PIC X(25).                    
             10 PI-DRKI1959-VERSTORNGRND-KNR   PIC S9(4) COMP.                  
             10 PI-DRKI1959-VERSTORNGRND-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI1959-VERSTORNODATUM   PIC  X(010).                       
             10 PI-DRKI1959-VERVERTRAGSABLAUF   PIC  X(010).                    
             10 PI-DRKI1959-VERVERTRAGSBEGINN   PIC  X(010).                    
             10 PI-DRKI1959-VERVERTRAGSNUMMER   PIC  X(0015).                   
             10 PI-DRKI1959-VERVERTRZUSTANDDAT   PIC  X(010).                   
             10 PI-DRKI1959-VERVERTRZUSTANDID   PIC S9(4) COMP.                 
             10 PI-DRKI1959-VERVSNRZUSATZ   PIC  X(0010).                       
             10 PI-DRKI1959-VERZAHLUNGSARTID   PIC S9(4) COMP.                  
             10 PI-DRKI1959-VERZAHLUNGSWEISEID   PIC S9(4) COMP.                
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI1959-NI1.                                                     
           05 PI-DRKI1959-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 35.                                    
       01  PI-DRKI1959-NI7.                                                     
           05 PI-DRKI1959-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 35.                                    
      *                                                                         
       01  PO-DRKI1959.                                                         
           05  PO-DRKI1959-RC                     PIC  X(003).                  
               88 PO-DRKI1959-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1959-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
