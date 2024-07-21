      *****************************************************************         
      *    COPY-MEMBER: DRKI3200                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3200                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         27.09.2022 / 11:34:38 / U131661            
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
      *    FÜR DIE FUNKTION DRKI3200                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3200.                                                         
           05  PI-DRKI3200-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3200-ZMAFONDSID   PIC S9(4) COMP.                        
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3200.                                                         
           05  PO-DRKI3200-RC                     PIC  X(003).                  
               88 PO-DRKI3200-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3200-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3200-NI.                                                  
               10 NI-DRKI3200-TAB.                                              
                  15 DRKI3200-ELE                                               
                     PIC S9(04) COMP OCCURS 58 TIMES.                           
               10 NI-DRKI3200 REDEFINES NI-DRKI3200-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI3200-NI-ZMAAIGKZ   PIC S9(4) COMP.                       
             15 PO-DRKI3200-NI-ZMAANLINFO   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAANLINFOEDH   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAASFBRANCHE   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAASFKAT   PIC S9(4) COMP.                      
             15 PO-DRKI3200-NI-ZMAASFLAND   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAASFSONDER   PIC S9(4) COMP.                   
             15 PO-DRKI3200-NI-ZMAASFTYP   PIC S9(4) COMP.                      
             15 PO-DRKI3200-NI-ZMAASFWAEHRG   PIC S9(4) COMP.                   
             15 PO-DRKI3200-NI-ZMAAUSGABEMAX   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAAUSGABEMIN   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAAVWICKLG   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMADVAGFONDS   PIC S9(4) COMP.                   
             15 PO-DRKI3200-N-ZMDVGWNFNDS   PIC S9(4) COMP.                     
             15 PO-DRKI3200-N-ZMMRSKPRF   PIC S9(4) COMP.                       
             15 PO-DRKI3200-NI-ZMAESGANTOEKO   PIC S9(4) COMP.                  
             15 PO-DRKI3200-N-ZMSGNTKDT   PIC S9(4) COMP.                       
             15 PO-DRKI3200-NI-ZMAESGINV   PIC S9(4) COMP.                      
             15 PO-DRKI3200-NI-ZMAESGINVDAT   PIC S9(4) COMP.                   
             15 PO-DRKI3200-NI-ZMAESGLETZTINV   PIC S9(4) COMP.                 
             15 PO-DRKI3200-N-ZMSGLTZTNVDT   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMAESGLZTATOEKO   PIC S9(4) COMP.                
             15 PO-DRKI3200-N-ZMSGLZTTKDT   PIC S9(4) COMP.                     
             15 PO-DRKI3200-N-ZMSGPRFRNZ   PIC S9(4) COMP.                      
             15 PO-DRKI3200-N-ZMSGSCHWRPNKT   PIC S9(4) COMP.                   
             15 PO-DRKI3200-NI-ZMAESGUZ01   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAESGUZ02   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAESGUZ03   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAESGUZ04   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAESGUZ05   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAESGUZ06   PIC S9(4) COMP.                     
             15 PO-DRKI3200-N-ZMSGZSNFSFDR   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMAETFETC   PIC S9(4) COMP.                      
             15 PO-DRKI3200-NI-ZMAFONDSART   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMAFONDSKAT   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMAFONDSKOSTDAT   PIC S9(4) COMP.                
             15 PO-DRKI3200-NI-ZMAFONDSKOSTEN   PIC S9(4) COMP.                 
             15 PO-DRKI3200-NI-ZMAGENFONDS   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMAHEBELKZ   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAMINDESTANL   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAPERFFEE   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMAPERFFEEDAT   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMARISIKOKL   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMARUECKMAX   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMARUECKMIN   PIC S9(4) COMP.                    
             15 PO-DRKI3200-NI-ZMARUEKOST   PIC S9(4) COMP.                     
             15 PO-DRKI3200-NI-ZMARUEKOSTDAT   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMASCHAETZFOKO   PIC S9(4) COMP.                 
             15 PO-DRKI3200-NI-ZMASCHAETZPEFE   PIC S9(4) COMP.                 
             15 PO-DRKI3200-NI-ZMASCHAETZRUEAB   PIC S9(4) COMP.                
             15 PO-DRKI3200-NI-ZMASCHAETZRUEKO   PIC S9(4) COMP.                
             15 PO-DRKI3200-NI-ZMASCHAETZTRKO   PIC S9(4) COMP.                 
             15 PO-DRKI3200-NI-ZMATICKGR   PIC S9(4) COMP.                      
             15 PO-DRKI3200-NI-ZMATRANSKOSTDAT   PIC S9(4) COMP.                
             15 PO-DRKI3200-NI-ZMATRANSKOSTEN   PIC S9(4) COMP.                 
             15 PO-DRKI3200-NI-ZMAVERTRZULAT   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAVERTRZULDE   PIC S9(4) COMP.                  
             15 PO-DRKI3200-NI-ZMAVLFONDS   PIC S9(4) COMP.                     
END   *                                                                         
                                                                                
           05  PO-DRKI3200-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3200-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3200-ZMAAIGKZ   PIC  X(0001).                            
             10 PO-DRKI3200-ZMAANLINFO   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAANLINFOEDH   PIC  X(0001).                       
             10 PO-DRKI3200-ZMAASFBRANCHE   PIC  X(0002).                       
             10 PO-DRKI3200-ZMAASFGARANT-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3200-ZMAASFGARANT-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3200-ZMAASFKAT   PIC  X(0001).                           
             10 PO-DRKI3200-ZMAASFLAND   PIC  X(0002).                          
             10 PO-DRKI3200-ZMAASFLAUFZ-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI3200-ZMAASFLAUFZ-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI3200-ZMAASFSONDER   PIC  X(0001).                        
             10 PO-DRKI3200-ZMAASFTYP   PIC  X(0001).                           
             10 PO-DRKI3200-ZMAASFWAEHRG   PIC  X(0002).                        
             10 PO-DRKI3200-ZMAAUSGABEMAX   PIC S9(6)V9(8) COMP-3.              
             10 PO-DRKI3200-ZMAAUSGABEMIN   PIC S9(6)V9(8) COMP-3.              
             10 PO-DRKI3200-ZMAAVWICKLG   PIC  X(0001).                         
             10 PO-DRKI3200-ZMADEVABRECHBRIEF   PIC S9(8)V9(4) COMP-3.          
             10 PO-DRKI3200-ZMADEVABRECHGELD   PIC S9(8)V9(4) COMP-3.           
             10 PO-DRKI3200-ZMADEVMITTELKURS   PIC S9(8)V9(5) COMP-3.           
             10 PO-DRKI3200-ZMADISANLHORIZ-KNR   PIC S9(15)V COMP-3.            
             10 PO-DRKI3200-ZMADISANLHORIZ-KTY   PIC S9(4) COMP.                
             10 PO-DRKI3200-ZMADISANLZIEL-KNR   PIC S9(15)V COMP-3.             
             10 PO-DRKI3200-ZMADISANLZIEL-KTY   PIC S9(4) COMP.                 
             10 PO-DRKI3200-ZMADISKENNTNIS-KNR   PIC S9(15)V COMP-3.            
             10 PO-DRKI3200-ZMADISKENNTNIS-KTY   PIC S9(4) COMP.                
             10 PO-DRKI3200-ZMADISKUNKAT-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3200-ZMADISKUNKAT-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3200-ZMADISSTRAT-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI3200-ZMADISSTRAT-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI3200-ZMADISVERLTF-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3200-ZMADISVERLTF-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3200-ZMADVAGFONDS   PIC  X(0001).                        
             10 PO-DRKI3200-ZMADVAGWIENFONDS   PIC  X(0001).                    
             10 PO-DRKI3200-ZMAEMIANLHORIZ-KNR   PIC S9(15)V COMP-3.            
             10 PO-DRKI3200-ZMAEMIANLHORIZ-KTY   PIC S9(4) COMP.                
             10 PO-DRKI3200-ZMAEMIANLZIEL-KNR   PIC S9(15)V COMP-3.             
             10 PO-DRKI3200-ZMAEMIANLZIEL-KTY   PIC S9(4) COMP.                 
             10 PO-DRKI3200-ZMAEMIKENNTNIS-KNR   PIC S9(15)V COMP-3.            
             10 PO-DRKI3200-ZMAEMIKENNTNIS-KTY   PIC S9(4) COMP.                
             10 PO-DRKI3200-ZMAEMIKUNKAT-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3200-ZMAEMIKUNKAT-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3200-ZMAEMIRISIKOPROF   PIC  X(0001).                    
             10 PO-DRKI3200-ZMAEMISPEZANF-KNR   PIC S9(15)V COMP-3.             
             10 PO-DRKI3200-ZMAEMISPEZANF-KTY   PIC S9(4) COMP.                 
             10 PO-DRKI3200-ZMAEMIVERLTF-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3200-ZMAEMIVERLTF-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3200-ZMAEMIVERSTRAT-KNR   PIC S9(15)V COMP-3.            
             10 PO-DRKI3200-ZMAEMIVERSTRAT-KTY   PIC S9(4) COMP.                
             10 PO-DRKI3200-ZMAESGANTOEKO   PIC S9(5)V9(4) COMP-3.              
             10 PO-DRKI3200-ZMAESGANTOEKODAT   PIC  X(010).                     
             10 PO-DRKI3200-ZMAESGINV   PIC S9(5)V9(4) COMP-3.                  
             10 PO-DRKI3200-ZMAESGINVDAT   PIC  X(010).                         
             10 PO-DRKI3200-ZMAESGLETZTINV   PIC S9(5)V9(4) COMP-3.             
             10 PO-DRKI3200-ZMAESGLETZTINVDAT   PIC  X(010).                    
             10 PO-DRKI3200-ZMAESGLZTATOEKO   PIC S9(5)V9(4) COMP-3.            
             10 PO-DRKI3200-ZMAESGLZTATOEKODAT   PIC  X(010).                   
             10 PO-DRKI3200-ZMAESGPRAEFERENZ   PIC  X(0001).                    
             10 PO-DRKI3200-ZMAESGSCHWERPUNKT   PIC  X(0001).                   
             10 PO-DRKI3200-ZMAESGUZ01   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAESGUZ02   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAESGUZ03   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAESGUZ04   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAESGUZ05   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAESGUZ06   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAESGZUSINFOSFDR   PIC  X(0002).                   
             10 PO-DRKI3200-ZMAETFETC   PIC  X(0004).                           
             10 PO-DRKI3200-ZMAFONDSART   PIC  X(0020).                         
             10 PO-DRKI3200-ZMAFONDSKAT   PIC  X(0001).                         
             10 PO-DRKI3200-ZMAFONDSKOSTDAT   PIC  X(010).                      
             10 PO-DRKI3200-ZMAFONDSKOSTEN   PIC S9(4)V9(2) COMP-3.             
             10 PO-DRKI3200-ZMAGENFONDS   PIC  X(0001).                         
             10 PO-DRKI3200-ZMAGESAUSAUF   PIC S9(3)V9(6) COMP-3.               
             10 PO-DRKI3200-ZMAGESAUSAUFKN   PIC  X(0001).                      
             10 PO-DRKI3200-ZMAHEBELKZ   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAISIN   PIC  X(0012).                             
             10 PO-DRKI3200-ZMAISTINVEST   PIC  X(0001).                        
             10 PO-DRKI3200-ZMAMINDESTANL   PIC S9(15)V9(3) COMP-3.             
             10 PO-DRKI3200-ZMAPERFFEE   PIC S9(4)V9(2) COMP-3.                 
             10 PO-DRKI3200-ZMAPERFFEEDAT   PIC  X(010).                        
             10 PO-DRKI3200-ZMARISIKOKL   PIC  X(0001).                         
             10 PO-DRKI3200-ZMARUECKMAX   PIC S9(6)V9(8) COMP-3.                
             10 PO-DRKI3200-ZMARUECKMIN   PIC S9(6)V9(8) COMP-3.                
             10 PO-DRKI3200-ZMARUEKOST   PIC S9(4)V9(2) COMP-3.                 
             10 PO-DRKI3200-ZMARUEKOSTDAT   PIC  X(010).                        
             10 PO-DRKI3200-ZMASCHAETZFOKO   PIC  X(0001).                      
             10 PO-DRKI3200-ZMASCHAETZPEFE   PIC  X(0001).                      
             10 PO-DRKI3200-ZMASCHAETZRUEAB   PIC  X(0001).                     
             10 PO-DRKI3200-ZMASCHAETZRUEKO   PIC  X(0001).                     
             10 PO-DRKI3200-ZMASCHAETZTRKO   PIC  X(0001).                      
             10 PO-DRKI3200-ZMATICKGR   PIC  X(0001).                           
             10 PO-DRKI3200-ZMATRANSKOSTDAT   PIC  X(010).                      
             10 PO-DRKI3200-ZMATRANSKOSTEN   PIC S9(4)V9(2) COMP-3.             
             10 PO-DRKI3200-ZMAVERTRZULAT   PIC  X(0002).                       
             10 PO-DRKI3200-ZMAVERTRZULDE   PIC  X(0002).                       
             10 PO-DRKI3200-ZMAVLFONDS   PIC  X(0001).                          
             10 PO-DRKI3200-ZMAWKN   PIC  X(0006).                              
             10 PO-DRKI3200-ZMAWPGRUPPE   PIC  X(0002).                         
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
