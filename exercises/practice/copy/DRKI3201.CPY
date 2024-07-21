      *****************************************************************         
      *    COPY-MEMBER: DRKI3201                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3201                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         27.09.2022 / 11:34:42 / U131661            
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
      *    FÜR DIE FUNKTION DRKI3201                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3201.                                                         
           05  PI-DRKI3201-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3201-ZMAFONDSID   PIC S9(4) COMP.                        
END   *                                                                         
                                                                                
           05  PI-DRKI3201-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3201-NI-ZMAAIGKZ         PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAANLINFO       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAANLINFOEDH    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAASFBRANCHE    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAASFKAT        PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAASFLAND       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAASFSONDER     PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAASFTYP        PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAASFWAEHRG     PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAAUSGABEMAX    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAAUSGABEMIN    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAAVWICKLG      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMADVAGFONDS     PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMDVGWNFNDS       PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMMRSKPRF         PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGANTOEKO    PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMSGNTKDT         PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGINV        PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGINVDAT     PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGLETZTINV   PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMSGLTZTNVDT      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGLZTATOEKO  PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMSGLZTTKDT       PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMSGPRFRNZ        PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMSGSCHWRPNKT     PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGUZ01       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGUZ02       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGUZ03       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGUZ04       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGUZ05       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAESGUZ06       PIC S9(4) COMP.                 
             15 PI-DRKI3201-N-ZMSGZSNFSFDR      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAETFETC        PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAFONDSART      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAFONDSKAT      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAFONDSKOSTDAT  PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAFONDSKOSTEN   PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAGENFONDS      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAHEBELKZ       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAMINDESTANL    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAPERFFEE       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAPERFFEEDAT    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMARISIKOKL      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMARUECKMAX      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMARUECKMIN      PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMARUEKOST       PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMARUEKOSTDAT    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMASCHAETZFOKO   PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMASCHAETZPEFE   PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMASCHAETZRUEAB  PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMASCHAETZRUEKO  PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMASCHAETZTRKO   PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMATICKGR        PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMATRANSKOSTDAT  PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMATRANSKOSTEN   PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAVERTRZULAT    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAVERTRZULDE    PIC S9(4) COMP.                 
             15 PI-DRKI3201-NI-ZMAVLFONDS       PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3201-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3201-ZMAAIGKZ   PIC  X(0001).                            
             10 PI-DRKI3201-ZMAANLINFO   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAANLINFOEDH   PIC  X(0001).                       
             10 PI-DRKI3201-ZMAASFBRANCHE   PIC  X(0002).                       
             10 PI-DRKI3201-ZMAASFGARANT-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3201-ZMAASFKAT   PIC  X(0001).                           
             10 PI-DRKI3201-ZMAASFLAND   PIC  X(0002).                          
             10 PI-DRKI3201-ZMAASFLAUFZ-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3201-ZMAASFSONDER   PIC  X(0001).                        
             10 PI-DRKI3201-ZMAASFTYP   PIC  X(0001).                           
             10 PI-DRKI3201-ZMAASFWAEHRG   PIC  X(0002).                        
             10 PI-DRKI3201-ZMAAUSGABEMAX   PIC S9(6)V9(8) COMP-3.              
             10 PI-DRKI3201-ZMAAUSGABEMIN   PIC S9(6)V9(8) COMP-3.              
             10 PI-DRKI3201-ZMAAVWICKLG   PIC  X(0001).                         
             10 PI-DRKI3201-ZMADEVABRECHBRIEF   PIC S9(8)V9(4) COMP-3.          
             10 PI-DRKI3201-ZMADEVABRECHGELD   PIC S9(8)V9(4) COMP-3.           
             10 PI-DRKI3201-ZMADEVMITTELKURS   PIC S9(8)V9(5) COMP-3.           
             10 PI-DRKI3201-ZMADISANLHORIZ-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3201-ZMADISANLZIEL-KNR   PIC S9(15)V COMP-3.             
             10 PI-DRKI3201-ZMADISKENNTNIS-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3201-ZMADISKUNKAT-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3201-ZMADISSTRAT-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3201-ZMADISVERLTF-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3201-ZMADVAGFONDS   PIC  X(0001).                        
             10 PI-DRKI3201-ZMADVAGWIENFONDS   PIC  X(0001).                    
             10 PI-DRKI3201-ZMAEMIANLHORIZ-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3201-ZMAEMIANLZIEL-KNR   PIC S9(15)V COMP-3.             
             10 PI-DRKI3201-ZMAEMIKENNTNIS-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3201-ZMAEMIKUNKAT-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3201-ZMAEMIRISIKOPROF   PIC  X(0001).                    
             10 PI-DRKI3201-ZMAEMISPEZANF-KNR   PIC S9(15)V COMP-3.             
             10 PI-DRKI3201-ZMAEMIVERLTF-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3201-ZMAEMIVERSTRAT-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI3201-ZMAESGANTOEKO   PIC S9(5)V9(4) COMP-3.              
             10 PI-DRKI3201-ZMAESGANTOEKODAT   PIC  X(010).                     
             10 PI-DRKI3201-ZMAESGINV   PIC S9(5)V9(4) COMP-3.                  
             10 PI-DRKI3201-ZMAESGINVDAT   PIC  X(010).                         
             10 PI-DRKI3201-ZMAESGLETZTINV   PIC S9(5)V9(4) COMP-3.             
             10 PI-DRKI3201-ZMAESGLETZTINVDAT   PIC  X(010).                    
             10 PI-DRKI3201-ZMAESGLZTATOEKO   PIC S9(5)V9(4) COMP-3.            
             10 PI-DRKI3201-ZMAESGLZTATOEKODAT   PIC  X(010).                   
             10 PI-DRKI3201-ZMAESGPRAEFERENZ   PIC  X(0001).                    
             10 PI-DRKI3201-ZMAESGSCHWERPUNKT   PIC  X(0001).                   
             10 PI-DRKI3201-ZMAESGUZ01   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAESGUZ02   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAESGUZ03   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAESGUZ04   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAESGUZ05   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAESGUZ06   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAESGZUSINFOSFDR   PIC  X(0002).                   
             10 PI-DRKI3201-ZMAETFETC   PIC  X(0004).                           
             10 PI-DRKI3201-ZMAFONDSART   PIC  X(0020).                         
             10 PI-DRKI3201-ZMAFONDSKAT   PIC  X(0001).                         
             10 PI-DRKI3201-ZMAFONDSKOSTDAT   PIC  X(010).                      
             10 PI-DRKI3201-ZMAFONDSKOSTEN   PIC S9(4)V9(2) COMP-3.             
             10 PI-DRKI3201-ZMAGENFONDS   PIC  X(0001).                         
             10 PI-DRKI3201-ZMAGESAUSAUF   PIC S9(3)V9(6) COMP-3.               
             10 PI-DRKI3201-ZMAGESAUSAUFKN   PIC  X(0001).                      
             10 PI-DRKI3201-ZMAHEBELKZ   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAISIN   PIC  X(0012).                             
             10 PI-DRKI3201-ZMAISTINVEST   PIC  X(0001).                        
             10 PI-DRKI3201-ZMAMINDESTANL   PIC S9(15)V9(3) COMP-3.             
             10 PI-DRKI3201-ZMAPERFFEE   PIC S9(4)V9(2) COMP-3.                 
             10 PI-DRKI3201-ZMAPERFFEEDAT   PIC  X(010).                        
             10 PI-DRKI3201-ZMARISIKOKL   PIC  X(0001).                         
             10 PI-DRKI3201-ZMARUECKMAX   PIC S9(6)V9(8) COMP-3.                
             10 PI-DRKI3201-ZMARUECKMIN   PIC S9(6)V9(8) COMP-3.                
             10 PI-DRKI3201-ZMARUEKOST   PIC S9(4)V9(2) COMP-3.                 
             10 PI-DRKI3201-ZMARUEKOSTDAT   PIC  X(010).                        
             10 PI-DRKI3201-ZMASCHAETZFOKO   PIC  X(0001).                      
             10 PI-DRKI3201-ZMASCHAETZPEFE   PIC  X(0001).                      
             10 PI-DRKI3201-ZMASCHAETZRUEAB   PIC  X(0001).                     
             10 PI-DRKI3201-ZMASCHAETZRUEKO   PIC  X(0001).                     
             10 PI-DRKI3201-ZMASCHAETZTRKO   PIC  X(0001).                      
             10 PI-DRKI3201-ZMATICKGR   PIC  X(0001).                           
             10 PI-DRKI3201-ZMATRANSKOSTDAT   PIC  X(010).                      
             10 PI-DRKI3201-ZMATRANSKOSTEN   PIC S9(4)V9(2) COMP-3.             
             10 PI-DRKI3201-ZMAVERTRZULAT   PIC  X(0002).                       
             10 PI-DRKI3201-ZMAVERTRZULDE   PIC  X(0002).                       
             10 PI-DRKI3201-ZMAVLFONDS   PIC  X(0001).                          
             10 PI-DRKI3201-ZMAWKN   PIC  X(0006).                              
             10 PI-DRKI3201-ZMAWPGRUPPE   PIC  X(0002).                         
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3201-NI1.                                                     
           05 PI-DRKI3201-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 58.                                
       01  PI-DRKI3201-NI7.                                                     
           05 PI-DRKI3201-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 58.                                
                                                                                
      *                                                                         
       01  PO-DRKI3201.                                                         
           05  PO-DRKI3201-RC                     PIC  X(003).                  
               88 PO-DRKI3201-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3201-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
