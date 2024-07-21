      *****************************************************************
      *    COPY-MEMBER: DRKI3855                                      * 
      *    --------------------------------------------------------   * 
      *    CURSOR FULL SELECT ZXKI0383 LBNPSVERTRAGTEIL               *
      *    --------------------------------------------------------   * 
      ***************************************************************** 
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-7514   !15.06.21! NEUERSTELLUNG                ! LORSON   *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER              
      *    FÜR DIE FUNKTION DRKI3885                                      
      *    AUSSER: DEM FUNKTIONSNAMEN,                                    
      *            DEM ERR-ERROR-BEREICH UND                              
      *            DEM STANDARD-STATUS-BEREICH                            
      *            (JEWEILS SEPARATE COPYS)                               
      *                                                                   
      *    FUNKTION: LIST                                                 
      *                                                                   
      ****************************************************************    
      *                                                                   
       01  PI-DRKI3885.                                                   
           05 PI-DRKI3885-UFKT-CODE             PIC  9(0002).             
              88 PI-DRKI3885-UFKT-FIRST              VALUE 1.             
              88 PI-DRKI3885-UFKT-NEXT               VALUE 2.             
              88 PI-DRKI3885-UFKT-QUIT               VALUE 3.             
                          
           05  PI-DRKI3885-DATEN.  
              10 PI-DRKI3885-LPTVERTRAGSID     PIC  X(0013).
              
                                                                          
                                                                          
       01  PO-DRKI3885.                                                   
           05 PO-DRKI3885-RC                    PIC  X(0003).
              88 PO-DRKI3885-RC-OK              VALUE 'OK '.             
              88 PO-DRKI3885-RC-LEER            VALUE 'LER'.             
              88 PO-DRKI3885-RC-EOF             VALUE 'EOF'.             


           05 PO-DRKI3885-KEY.                                            
              10 PO-DRKI3885-LPTLFDNR           PIC S9(4) COMP.           
                                                                          
      *                                                                 
           05  PO-DRKI3885-NI.                                          
               10 NI-DRKI3885-TAB.                                      
                  15 DRKI3885-ELE                                       
                     PIC S9(04) COMP OCCURS 7 TIMES.                    
               10 NI-DRKI3885 REDEFINES NI-DRKI3885-TAB.                
                                                                        
      *        %M_PO_DATEN_NI%                                          
             15 PO-DRKI3885-NI-LPTABLAUF   PIC S9(4) COMP.              
             15 PO-DRKI3885-NI-LPTABLBZ   PIC S9(4) COMP.               
             15 PO-DRKI3885-NI-LPTBEGINN   PIC S9(4) COMP.              
             15 PO-DRKI3885-NI-LPTRTBTGVT   PIC S9(4) COMP.             
             15 PO-DRKI3885-NI-LPTTARIFBEITRAG   PIC S9(4) COMP.        
             15 PO-DRKI3885-NI-LPTVZUST-D-DAT   PIC S9(4) COMP.         
             15 PO-DRKI3885-NI-LPTZAHLBTGVT   PIC S9(4) COMP.           
      *                                                                 
                                                                        
           05  PO-DRKI3885-DATEN.                                       
                                                                        
                                                                        
      *        %M_PO_DATEN%                                             
             10 PO-DRKI3885-LETZTEAENDERUNG   PIC  X(0026).             
             10 PO-DRKI3885-LPTABLAUF   PIC  X(010).                    
             10 PO-DRKI3885-LPTABLBZ   PIC  X(010).                     
             10 PO-DRKI3885-LPTBEGINN   PIC  X(010).                    
             10 PO-DRKI3885-LPTBEITRZUST-KNR   PIC S9(15)V COMP-3.      
             10 PO-DRKI3885-LPTBEITRZUST-KTY   PIC S9(4) COMP.          
             10 PO-DRKI3885-LPTBEZUGLFDNR   PIC S9(4) COMP.             
             10 PO-DRKI3885-LPTHVT-KZN   PIC  X(0001).                  
             10 PO-DRKI3885-LPTRTBTGVT   PIC S9(11)V9(2) COMP-3.        
             10 PO-DRKI3885-LPTTARIFBEITRAG   PIC S9(11)V9(2) COMP-3.   
             10 PO-DRKI3885-LPTTARKLASSE-KNR   PIC S9(15)V COMP-3.      
             10 PO-DRKI3885-LPTTARKLASSE-KTY   PIC S9(4) COMP.          
             10 PO-DRKI3885-LPTVERSSUMME   PIC S9(11)V9(2) COMP-3.      
             10 PO-DRKI3885-LPTVZUST-D-DAT   PIC  X(010).               
             10 PO-DRKI3885-LPTVZUST-D-KNR   PIC S9(4) COMP.            
             10 PO-DRKI3885-LPTVZUST-D-KTY   PIC S9(4) COMP.            
             10 PO-DRKI3885-LPTZAHLBTGVT   PIC S9(11)V9(2) COMP-3.      
      *    
      ***************************************************************** 
      *---------------------- ENDE DRKI3885 --------------------------* 
      *****************************************************************