      *****************************************************************         
      *    COPY-MEMBER: DRKI3311                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3311                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         23.12.2021 / 10:51:36 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI3311                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3311.                                                         
           05  PI-DRKI3311-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3311-G70VLFDNR   PIC  X(0001).                           
             10 PI-DRKI3311-G70VVERTRAGSNR   PIC  X(0015).                      
END   *                                                                         
                                                                                
           05  PI-DRKI3311-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3311-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3311-G70VABBVERS   PIC  X(0007).                         
             10 PI-DRKI3311-G70VABLAUF   PIC  X(0008).                          
             10 PI-DRKI3311-G70VABSCHLVBNR   PIC S9(9) COMP.                    
             10 PI-DRKI3311-G70VABTRETBLZ   PIC  X(0008).                       
             10 PI-DRKI3311-G70VAENDKZ   PIC  X(0002).                          
             10 PI-DRKI3311-G70VANFTILG   PIC S9(3)V9(2) COMP-3.                
             10 PI-DRKI3311-G70VANNURATE   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VANSPWOP   PIC S9(7)V9(2) COMP-3.                
             10 PI-DRKI3311-G70VANWZUTEIVO   PIC  X(0008).                      
             10 PI-DRKI3311-G70VBASERHOEH   PIC S9(3)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VBEGINN   PIC  X(0008).                          
             10 PI-DRKI3311-G70VBEITRAG   PIC S9(5)V9(2) COMP-3.                
             10 PI-DRKI3311-G70VBEWERTZAHL   PIC S9(3)V9(5) COMP-3.             
             10 PI-DRKI3311-G70VBLANKODARL   PIC  X(0001).                      
             10 PI-DRKI3311-G70VBONUSAKT   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VBONUSART   PIC  X(0002).                        
             10 PI-DRKI3311-G70VBONUSBEGDATUM   PIC  X(0008).                   
             10 PI-DRKI3311-G70VBONUSFAKT   PIC S9(2)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VBONUSVORJ   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VBWZVJ   PIC S9(3)V9(5) COMP-3.                  
             10 PI-DRKI3311-G70VDARLAKTEKZ   PIC  X(0001).                      
             10 PI-DRKI3311-G70VDARLHOEHE   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VDARLSALDO   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VDARLZIEFF   PIC S9(3)V9(3) COMP-3.              
             10 PI-DRKI3311-G70VDARLZINOM   PIC S9(3)V9(3) COMP-3.              
             10 PI-DRKI3311-G70VDARLZINS   PIC S9(3)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VDATAFBEG   PIC  X(0008).                        
             10 PI-DRKI3311-G70VDATAGEIN   PIC  X(0008).                        
             10 PI-DRKI3311-G70VDATMINANSP   PIC  X(0008).                      
             10 PI-DRKI3311-G70VDATSPERFR   PIC  X(0008).                       
             10 PI-DRKI3311-G70VDATWOBPRAE   PIC  X(0008).                      
             10 PI-DRKI3311-G70VDATZUTAUSSI   PIC  X(0008).                     
             10 PI-DRKI3311-G70VDAUZULKZ   PIC  X(0001).                        
             10 PI-DRKI3311-G70VDISPROZ   PIC S9(3)V9(2) COMP-3.                
             10 PI-DRKI3311-G70VDLETZTAKT   PIC  X(0008).                       
             10 PI-DRKI3311-G70VDYNJAEREINZ   PIC S9(3)V9(2) COMP-3.            
             10 PI-DRKI3311-G70VDZINSFAB   PIC  X(0008).                        
             10 PI-DRKI3311-G70VDZINSFBIS   PIC  X(0008).                       
             10 PI-DRKI3311-G70VEHELKZ   PIC  X(0001).                          
             10 PI-DRKI3311-G70VENDBINDFRI   PIC  X(0010).                      
             10 PI-DRKI3311-G70VENDSTEULZ   PIC  X(0008).                       
             10 PI-DRKI3311-G70VERMITTKZ   PIC  X(0001).                        
             10 PI-DRKI3311-G70VFINANZKZ   PIC  X(0001).                        
             10 PI-DRKI3311-G70VFINPROD   PIC  X(0002).                         
             10 PI-DRKI3311-G70VFMINANSPAR   PIC S9(8)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VFOERDRELEV   PIC S9(7)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VFRONTENDT   PIC  X(0008).                       
             10 PI-DRKI3311-G70VGUTHABBEGR   PIC  X(0008).                      
             10 PI-DRKI3311-G70VIMMOFITKZ   PIC  X(0001).                       
             10 PI-DRKI3311-G70VINKASSO   PIC  X(0001).                         
             10 PI-DRKI3311-G70VJAERLEINZBETR   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3311-G70VJAERLEINZDAT   PIC  X(0008).                    
             10 PI-DRKI3311-G70VJAHRZULAGE.                                     
                49 PI-DRKI3311-G70VJAHRZULAGE-LEN PIC S9(4) COMP.               
                49 PI-DRKI3311-G70VJHRZLG-TEXT PIC X(105).                      
             10 PI-DRKI3311-G70VKTOSTAKZ   PIC  X(0001).                        
             10 PI-DRKI3311-G70VLAUFZEIT   PIC  X(0002).                        
             10 PI-DRKI3311-G70VLETZTAEND   PIC  X(0008).                       
             10 PI-DRKI3311-G70VLETZTKTOAZ   PIC  X(0008).                      
             10 PI-DRKI3311-G70VLETZTZUTEIL   PIC  X(0008).                     
             10 PI-DRKI3311-G70VLZFIN   PIC  X(0003).                           
             10 PI-DRKI3311-G70VMANDANT   PIC S9(9) COMP.                       
             10 PI-DRKI3311-G70VMAXVZINSGH   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VMEHRZUTEIL   PIC S9(7)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VMIETKAUT   PIC  X(0001).                        
             10 PI-DRKI3311-G70VMINANSPPRO   PIC S9(3)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VMINBEWZAHL   PIC S9(4)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VMTLEINZBETR   PIC S9(7)V9(2) COMP-3.            
             10 PI-DRKI3311-G70VMTLEINZDAT   PIC  X(0008).                      
             10 PI-DRKI3311-G70VNRAKTKTOAZ   PIC  X(0007).                      
             10 PI-DRKI3311-G70VNVERANLBE   PIC  X(0008).                       
             10 PI-DRKI3311-G70VONLART   PIC  X(0050).                          
             10 PI-DRKI3311-G70VONLPROD   PIC  X(0050).                         
             10 PI-DRKI3311-G70VPARTINFO   PIC  X(0020).                        
             10 PI-DRKI3311-G70VPASSWORT   PIC  X(0001).                        
             10 PI-DRKI3311-G70VPEAUSZAHLART   PIC  X(0001).                    
             10 PI-DRKI3311-G70VPRODSCHL   PIC  X(0002).                        
             10 PI-DRKI3311-G70VSALDOWFKTO   PIC S9(7)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VSCHLWOHNPRG   PIC  X(0002).                     
             10 PI-DRKI3311-G70VSONDERTILGUNG   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI3311-G70VSONDERZAHLUNG   PIC S9(3)V9(2) COMP-3.          
             10 PI-DRKI3311-G70VSONDTILJ   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VSPARBETRAG   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3311-G70VSPARZULAB   PIC  X(0008).                       
             10 PI-DRKI3311-G70VSTEULFZDAT   PIC  X(0008).                      
             10 PI-DRKI3311-G70VSUMAVWL   PIC S9(7)V9(2) COMP-3.                
             10 PI-DRKI3311-G70VSUMFOERVJ   PIC S9(7)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VSUMME   PIC S9(8)V9(2) COMP-3.                  
             10 PI-DRKI3311-G70VSUMVLEINZ   PIC S9(7)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VTARIFKZ   PIC  X(0001).                         
             10 PI-DRKI3311-G70VTARIFUMST   PIC S9(5)V9(2) COMP-3.              
             10 PI-DRKI3311-G70VTUBAKTION   PIC  X(0001).                       
             10 PI-DRKI3311-G70VVARIWECHS   PIC  X(0001).                       
             10 PI-DRKI3311-G70VVLPERSONAL   PIC  X(0001).                      
             10 PI-DRKI3311-G70VVWLKZ   PIC  X(0001).                           
             10 PI-DRKI3311-G70VWERBVERBOT   PIC  X(0001).                      
             10 PI-DRKI3311-G70VWKZ   PIC  X(0003).                             
             10 PI-DRKI3311-G70VWOPKZ   PIC  X(0001).                           
             10 PI-DRKI3311-G70VWOPVJ   PIC  X(0004).                           
             10 PI-DRKI3311-G70VWOPVVJ   PIC  X(0004).                          
             10 PI-DRKI3311-G70VZAHLW   PIC  X(0002).                           
             10 PI-DRKI3311-G70VZIELTILG   PIC S9(3)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VZINSALLG   PIC S9(3)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VZINSLFDJ   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3311-G70VZINSVOR   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI3311-G70VZPGEN   PIC  X(0002).                           
             10 PI-DRKI3311-G70VZUTEILANW   PIC  X(0008).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3311-NI1.                                                     
           05 PI-DRKI3311-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI3311-NI7.                                                     
           05 PI-DRKI3311-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI3311.                                                         
           05  PO-DRKI3311-RC                     PIC  X(003).                  
               88 PO-DRKI3311-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3311-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
