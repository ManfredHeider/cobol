      *****************************************************************         
      *    COPY-MEMBER: DRKI3202                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3202                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         27.09.2022 / 11:34:44 / U131661            
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
      *    FÜR DIE FUNKTION DRKI3202                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3202.                                                         
           05  PI-DRKI3202-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3202-ZMAFONDSID   PIC S9(4) COMP.                        
END   *                                                                         
                                                                                
           05  PI-DRKI3202-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3202-NI-ZMAAIGKZ         PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAANLINFO       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAANLINFOEDH    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFBRANCHE    PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSFGRNT-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFKAT        PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFLAND       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFLAUFZ-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFSONDER     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFTYP        PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAASFWAEHRG     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAAUSGABEMAX    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAAUSGABEMIN    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAAVWICKLG      PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDVBRCHBRF       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDVBRCHGLD       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDVMTTLKRS       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDSNLHRZ-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDSNLZL-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDSKNNTNS-KNR    PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDSKNKT-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMADISSTRAT-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDSVRLTF-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMADVAGFONDS     PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMDVGWNFNDS       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMNLHRZ-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMNLZL-KNR       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMKNNTNS-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMKNKT-KNR       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMRSKPRF         PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMSPZNF-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMVRLTF-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMMVRSTRT-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGANTOEKO    PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSGNTKDT         PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGINV        PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGINVDAT     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGLETZTINV   PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSGLTZTNVDT      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGLZTATOEKO  PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSGLZTTKDT       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSGPRFRNZ        PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSGSCHWRPNKT     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGUZ01       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGUZ02       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGUZ03       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGUZ04       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGUZ05       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAESGUZ06       PIC S9(4) COMP.                 
             15 PI-DRKI3202-N-ZMSGZSNFSFDR      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAETFETC        PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAFONDSART      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAFONDSKAT      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAFONDSKOSTDAT  PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAFONDSKOSTEN   PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAGENFONDS      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAGESAUSAUF     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAGESAUSAUFKN   PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAHEBELKZ       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAISIN          PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAISTINVEST     PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAMINDESTANL    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAPERFFEE       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAPERFFEEDAT    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMARISIKOKL      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMARUECKMAX      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMARUECKMIN      PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMARUEKOST       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMARUEKOSTDAT    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMASCHAETZFOKO   PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMASCHAETZPEFE   PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMASCHAETZRUEAB  PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMASCHAETZRUEKO  PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMASCHAETZTRKO   PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMATICKGR        PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMATRANSKOSTDAT  PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMATRANSKOSTEN   PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAVERTRZULAT    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAVERTRZULDE    PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAVLFONDS       PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAWKN           PIC S9(4) COMP.                 
             15 PI-DRKI3202-NI-ZMAWPGRUPPE      PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3202-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3202-ZMAAIGKZ   PIC  X(0001).                            
             10 PI-DRKI3202-ZMAANLINFO   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAANLINFOEDH   PIC  X(0001).                       
             10 PI-DRKI3202-ZMAASFBRANCHE   PIC  X(0002).                       
             10 PI-DRKI3202-ZMAASFGARANT-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3202-ZMAASFKAT   PIC  X(0001).                           
             10 PI-DRKI3202-ZMAASFLAND   PIC  X(0002).                          
             10 PI-DRKI3202-ZMAASFLAUFZ-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3202-ZMAASFSONDER   PIC  X(0001).                        
             10 PI-DRKI3202-ZMAASFTYP   PIC  X(0001).                           
             10 PI-DRKI3202-ZMAASFWAEHRG   PIC  X(0002).                        
             10 PI-DRKI3202-ZMAAUSGABEMAX   PIC S9(6)V9(8) COMP-3.              
             10 PI-DRKI3202-ZMAAUSGABEMIN   PIC S9(6)V9(8) COMP-3.              
             10 PI-DRKI3202-ZMAAVWICKLG   PIC  X(0001).                         
             10 PI-DRKI3202-ZMADEVABRECHBRIEF   PIC S9(8)V9(4) COMP-3.          
             10 PI-DRKI3202-ZMADEVABRECHGELD   PIC S9(8)V9(4) COMP-3.           
             10 PI-DRKI3202-ZMADEVMITTELKURS   PIC S9(8)V9(5) COMP-3.           
             10 PI-DRKI3202-ZMADISANLHORIZ-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3202-ZMADISANLZIEL-KNR   PIC S9(15)V COMP-3.             
             10 PI-DRKI3202-ZMADISKENNTNIS-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3202-ZMADISKUNKAT-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3202-ZMADISSTRAT-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3202-ZMADISVERLTF-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3202-ZMADVAGFONDS   PIC  X(0001).                        
             10 PI-DRKI3202-ZMADVAGWIENFONDS   PIC  X(0001).                    
             10 PI-DRKI3202-ZMAEMIANLHORIZ-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3202-ZMAEMIANLZIEL-KNR   PIC S9(15)V COMP-3.             
             10 PI-DRKI3202-ZMAEMIKENNTNIS-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3202-ZMAEMIKUNKAT-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3202-ZMAEMIRISIKOPROF   PIC  X(0001).                    
             10 PI-DRKI3202-ZMAEMISPEZANF-KNR   PIC S9(15)V COMP-3.             
             10 PI-DRKI3202-ZMAEMIVERLTF-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3202-ZMAEMIVERSTRAT-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3202-ZMAESGANTOEKO   PIC S9(5)V9(4) COMP-3.              
             10 PI-DRKI3202-ZMAESGANTOEKODAT   PIC  X(010).                     
             10 PI-DRKI3202-ZMAESGINV   PIC S9(5)V9(4) COMP-3.                  
             10 PI-DRKI3202-ZMAESGINVDAT   PIC  X(010).                         
             10 PI-DRKI3202-ZMAESGLETZTINV   PIC S9(5)V9(4) COMP-3.             
             10 PI-DRKI3202-ZMAESGLETZTINVDAT   PIC  X(010).                    
             10 PI-DRKI3202-ZMAESGLZTATOEKO   PIC S9(5)V9(4) COMP-3.            
             10 PI-DRKI3202-ZMAESGLZTATOEKODAT   PIC  X(010).                   
             10 PI-DRKI3202-ZMAESGPRAEFERENZ   PIC  X(0001).                    
             10 PI-DRKI3202-ZMAESGSCHWERPUNKT   PIC  X(0001).                   
             10 PI-DRKI3202-ZMAESGUZ01   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAESGUZ02   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAESGUZ03   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAESGUZ04   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAESGUZ05   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAESGUZ06   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAESGZUSINFOSFDR   PIC  X(0002).                   
             10 PI-DRKI3202-ZMAETFETC   PIC  X(0004).                           
             10 PI-DRKI3202-ZMAFONDSART   PIC  X(0020).                         
             10 PI-DRKI3202-ZMAFONDSKAT   PIC  X(0001).                         
             10 PI-DRKI3202-ZMAFONDSKOSTDAT   PIC  X(010).                      
             10 PI-DRKI3202-ZMAFONDSKOSTEN   PIC S9(4)V9(2) COMP-3.             
             10 PI-DRKI3202-ZMAGENFONDS   PIC  X(0001).                         
             10 PI-DRKI3202-ZMAGESAUSAUF   PIC S9(3)V9(6) COMP-3.               
             10 PI-DRKI3202-ZMAGESAUSAUFKN   PIC  X(0001).                      
             10 PI-DRKI3202-ZMAHEBELKZ   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAISIN   PIC  X(0012).                             
             10 PI-DRKI3202-ZMAISTINVEST   PIC  X(0001).                        
             10 PI-DRKI3202-ZMAMINDESTANL   PIC S9(15)V9(3) COMP-3.             
             10 PI-DRKI3202-ZMAPERFFEE   PIC S9(4)V9(2) COMP-3.                 
             10 PI-DRKI3202-ZMAPERFFEEDAT   PIC  X(010).                        
             10 PI-DRKI3202-ZMARISIKOKL   PIC  X(0001).                         
             10 PI-DRKI3202-ZMARUECKMAX   PIC S9(6)V9(8) COMP-3.                
             10 PI-DRKI3202-ZMARUECKMIN   PIC S9(6)V9(8) COMP-3.                
             10 PI-DRKI3202-ZMARUEKOST   PIC S9(4)V9(2) COMP-3.                 
             10 PI-DRKI3202-ZMARUEKOSTDAT   PIC  X(010).                        
             10 PI-DRKI3202-ZMASCHAETZFOKO   PIC  X(0001).                      
             10 PI-DRKI3202-ZMASCHAETZPEFE   PIC  X(0001).                      
             10 PI-DRKI3202-ZMASCHAETZRUEAB   PIC  X(0001).                     
             10 PI-DRKI3202-ZMASCHAETZRUEKO   PIC  X(0001).                     
             10 PI-DRKI3202-ZMASCHAETZTRKO   PIC  X(0001).                      
             10 PI-DRKI3202-ZMATICKGR   PIC  X(0001).                           
             10 PI-DRKI3202-ZMATRANSKOSTDAT   PIC  X(010).                      
             10 PI-DRKI3202-ZMATRANSKOSTEN   PIC S9(4)V9(2) COMP-3.             
             10 PI-DRKI3202-ZMAVERTRZULAT   PIC  X(0002).                       
             10 PI-DRKI3202-ZMAVERTRZULDE   PIC  X(0002).                       
             10 PI-DRKI3202-ZMAVLFONDS   PIC  X(0001).                          
             10 PI-DRKI3202-ZMAWKN   PIC  X(0006).                              
             10 PI-DRKI3202-ZMAWPGRUPPE   PIC  X(0002).                         
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3202-NI1.                                                     
           05 PI-DRKI3202-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 82.                                    
       01  PI-DRKI3202-NI7.                                                     
           05 PI-DRKI3202-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 82.                                    
      *                                                                         
       01  PO-DRKI3202.                                                         
           05  PO-DRKI3202-RC                     PIC  X(003).                  
               88 PO-DRKI3202-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3202-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
