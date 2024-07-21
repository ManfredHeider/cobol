      *****************************************************************         
      *    COPY-MEMBER: DRKI3506                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3506                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.10.2022 / 15:06:13 / T13EX78            
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
      *    FÜR DIE FUNKTION DRKI3506                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3506.                                                         
           05  PI-DRKI3506-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3506-G11VVERTRAGSNR   PIC  X(0015).                      
END   *                                                                         
                                                                                
           05  PI-DRKI3506-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3506-NI-G11BEGLFAHRTXT   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11FALTERMAX     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11KFZLEISTUNG   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11STORNODAT     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VABLAUF       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VABSCHLVBNR   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VAENDDAT      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VAENDKZ       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VAKZARTKNR    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VALTERGRPKNR  PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VAMTKZ        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBEGINN       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBEGLFAHR     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBEITRAG      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBEITRKFZW    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBEITRKHZW    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBESVEREIN    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBRANCHE      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VBUENDKZ      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VCOCDAT       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VCOCKZ        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VDAKF         PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VDAKH         PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VELEKTROKZ    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VERSTZULDAT   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFAHRER       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFAHRGEBIET   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFAHRZGID     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFIRMENNR     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFSCHEINDAT   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGABSTELL   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGARTKNR    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGAUFBAU    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGHALTER    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGHALTNAME  PIC S9(4) COMP.                 
             15 PI-DRKI3506-N-G11VFZGHLTVNM     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGMODELL    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGNUTZ      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZGVERWKNR   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VFZHGHALTTXT  PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VGEBDATFAH    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VGESGEWICHT   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VHERSTNR      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VHERSTTXT     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VJAHRKMLEIST  PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VKFZKW        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VKFZLEISTKZ   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VKFZTYP       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VKMSTAAKT     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VKMSTAAKTDAT  PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VKSMVERTR     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VLEASING      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VLETZTAEND    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VLSVSCOR      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VMANDANT      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VNAESPEZKNR   PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VNEUWERT      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VNUTZL        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VPAKETTEXT    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VPAKETTYP     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VPIN          PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VPINPRUEF     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VPLATZ        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VRKLKF        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VRKLKH        PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSAISONBEG    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSAISONEND    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSAISONKZ     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSBTK         PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSBVK         PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSCHUTZBR     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSCOREDAT     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSCOREWERT    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSFRTAB       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VSONDERAUS    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VTARAUSW      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VTARIFBEZ     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VTELEMATIK    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VTYPKLKF      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VTYPKLKH      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VVERSBED      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VWAEHRKZ      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VWAGNISKZ     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VWAGNISTXT    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VWERKBIND     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VWOHNEIGEN    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZAHLART      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZAHLWEISE    PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZULDATUM     PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZUNACH       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZUSTAND      PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZWSCHL       PIC S9(4) COMP.                 
             15 PI-DRKI3506-NI-G11VZWSCHLTXT    PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3506-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3506-G11BEGLFAHRTXT   PIC  X(0010).                      
             10 PI-DRKI3506-G11FALTERMAX   PIC  X(0002).                        
             10 PI-DRKI3506-G11KFZLEISTUNG   PIC S9(5)V9(2) COMP-3.             
             10 PI-DRKI3506-G11STORNODAT   PIC  X(0008).                        
             10 PI-DRKI3506-G11VABLAUF   PIC  X(0008).                          
             10 PI-DRKI3506-G11VABSCHLVBNR   PIC S9(9) COMP.                    
             10 PI-DRKI3506-G11VAENDDAT   PIC  X(0008).                         
             10 PI-DRKI3506-G11VAENDKZ   PIC  X(0002).                          
             10 PI-DRKI3506-G11VAKZARTKNR   PIC  X(0001).                       
             10 PI-DRKI3506-G11VALTERGRPKNR   PIC  X(0004).                     
             10 PI-DRKI3506-G11VAMTKZ   PIC  X(0012).                           
             10 PI-DRKI3506-G11VBEGINN   PIC  X(0008).                          
             10 PI-DRKI3506-G11VBEGLFAHR   PIC  X(0002).                        
             10 PI-DRKI3506-G11VBEITRAG   PIC S9(8)V9(2) COMP-3.                
             10 PI-DRKI3506-G11VBEITRKFZW   PIC S9(8)V9(2) COMP-3.              
             10 PI-DRKI3506-G11VBEITRKHZW   PIC S9(8)V9(2) COMP-3.              
             10 PI-DRKI3506-G11VBESVEREIN.                                      
                49 PI-DRKI3506-G11VBESVEREIN-LEN PIC S9(4) COMP.                
                49 PI-DRKI3506-G11VBESVEREIN-TEXT PIC X(750).                   
             10 PI-DRKI3506-G11VBRANCHE   PIC  X(0060).                         
             10 PI-DRKI3506-G11VBUENDKZ   PIC  X(0001).                         
             10 PI-DRKI3506-G11VCOCDAT   PIC  X(0008).                          
             10 PI-DRKI3506-G11VCOCKZ   PIC  X(0001).                           
             10 PI-DRKI3506-G11VDAKF   PIC  X(0002).                            
             10 PI-DRKI3506-G11VDAKH   PIC  X(0002).                            
             10 PI-DRKI3506-G11VELEKTROKZ   PIC  X(0001).                       
             10 PI-DRKI3506-G11VERSTZULDAT   PIC  X(0008).                      
             10 PI-DRKI3506-G11VFAHRER   PIC  X(0010).                          
             10 PI-DRKI3506-G11VFAHRGEBIET   PIC  X(0010).                      
             10 PI-DRKI3506-G11VFAHRZGID   PIC  X(0020).                        
             10 PI-DRKI3506-G11VFIRMENNR   PIC  X(0006).                        
             10 PI-DRKI3506-G11VFSCHEINDAT   PIC  X(0010).                      
             10 PI-DRKI3506-G11VFZGABSTELL   PIC  X(0010).                      
             10 PI-DRKI3506-G11VFZGARTKNR   PIC  X(0003).                       
             10 PI-DRKI3506-G11VFZGAUFBAU   PIC  X(0004).                       
             10 PI-DRKI3506-G11VFZGHALTER   PIC  X(0002).                       
             10 PI-DRKI3506-G11VFZGHALTNAME   PIC  X(0040).                     
             10 PI-DRKI3506-G11VFZGHALTVNAME   PIC  X(0040).                    
             10 PI-DRKI3506-G11VFZGMODELL   PIC  X(0050).                       
             10 PI-DRKI3506-G11VFZGNUTZ   PIC  X(0010).                         
             10 PI-DRKI3506-G11VFZGVERWKNR   PIC  X(0003).                      
             10 PI-DRKI3506-G11VFZHGHALTTXT   PIC  X(0010).                     
             10 PI-DRKI3506-G11VGEBDATFAH   PIC  X(0010).                       
             10 PI-DRKI3506-G11VGESGEWICHT   PIC S9(7)V COMP-3.                 
             10 PI-DRKI3506-G11VHERSTNR   PIC  X(0004).                         
             10 PI-DRKI3506-G11VHERSTTXT   PIC  X(0025).                        
             10 PI-DRKI3506-G11VJAHRKMLEIST   PIC S9(7)V COMP-3.                
             10 PI-DRKI3506-G11VKFZKW   PIC S9(4)V COMP-3.                      
             10 PI-DRKI3506-G11VKFZLEISTKZ   PIC  X(0001).                      
             10 PI-DRKI3506-G11VKFZTYP   PIC  X(0003).                          
             10 PI-DRKI3506-G11VKMSTAAKT   PIC S9(7)V COMP-3.                   
             10 PI-DRKI3506-G11VKMSTAAKTDAT   PIC  X(0008).                     
             10 PI-DRKI3506-G11VKSMVERTR.                                       
                49 PI-DRKI3506-G11VKSMVERTR-LEN PIC S9(4) COMP.                 
                49 PI-DRKI3506-G11VKSMVERTR-TEXT PIC X(464).                    
             10 PI-DRKI3506-G11VLEASING   PIC  X(0001).                         
             10 PI-DRKI3506-G11VLETZTAEND   PIC  X(0008).                       
             10 PI-DRKI3506-G11VLSVSCOR   PIC  X(0001).                         
             10 PI-DRKI3506-G11VMANDANT   PIC S9(9) COMP.                       
             10 PI-DRKI3506-G11VNAESPEZKNR   PIC  X(0003).                      
             10 PI-DRKI3506-G11VNEUWERT   PIC S9(7)V COMP-3.                    
             10 PI-DRKI3506-G11VNUTZL   PIC S9(7)V COMP-3.                      
             10 PI-DRKI3506-G11VPAKETTEXT   PIC  X(0150).                       
             10 PI-DRKI3506-G11VPAKETTYP   PIC  X(0004).                        
             10 PI-DRKI3506-G11VPIN   PIC  X(0009).                             
             10 PI-DRKI3506-G11VPINPRUEF   PIC  X(0002).                        
             10 PI-DRKI3506-G11VPLATZ   PIC  X(0003).                           
             10 PI-DRKI3506-G11VRKLKF   PIC  X(0004).                           
             10 PI-DRKI3506-G11VRKLKH   PIC  X(0004).                           
             10 PI-DRKI3506-G11VSAISONBEG   PIC  X(0002).                       
             10 PI-DRKI3506-G11VSAISONEND   PIC  X(0002).                       
             10 PI-DRKI3506-G11VSAISONKZ   PIC  X(0001).                        
             10 PI-DRKI3506-G11VSBTK   PIC S9(8)V9(2) COMP-3.                   
             10 PI-DRKI3506-G11VSBVK   PIC S9(8)V9(2) COMP-3.                   
             10 PI-DRKI3506-G11VSCHUTZBR   PIC  X(0010).                        
             10 PI-DRKI3506-G11VSCOREDAT   PIC  X(0008).                        
             10 PI-DRKI3506-G11VSCOREWERT   PIC S9(3)V COMP-3.                  
             10 PI-DRKI3506-G11VSFRTAB.                                         
                49 PI-DRKI3506-G11VSFRTAB-LEN PIC S9(4) COMP.                   
                49 PI-DRKI3506-G11VSFRTAB-TEXT PIC X(30).                       
             10 PI-DRKI3506-G11VSONDERAUS.                                      
                49 PI-DRKI3506-G11VSONDERAUS-LEN PIC S9(4) COMP.                
                49 PI-DRKI3506-G11VSONDERAUS-TEXT PIC X(240).                   
             10 PI-DRKI3506-G11VTARAUSW   PIC  X(0001).                         
             10 PI-DRKI3506-G11VTARIFBEZ.                                       
                49 PI-DRKI3506-G11VTARIFBEZ-LEN PIC S9(4) COMP.                 
                49 PI-DRKI3506-G11VTARIFBEZ-TEXT PIC X(21).                     
             10 PI-DRKI3506-G11VTELEMATIK   PIC S9(14)V COMP-3.                 
             10 PI-DRKI3506-G11VTYPKLKF   PIC  X(0002).                         
             10 PI-DRKI3506-G11VTYPKLKH   PIC  X(0002).                         
             10 PI-DRKI3506-G11VVERSBED.                                        
                49 PI-DRKI3506-G11VVERSBED-LEN PIC S9(4) COMP.                  
                49 PI-DRKI3506-G11VVERSBED-TEXT PIC X(63).                      
             10 PI-DRKI3506-G11VWAEHRKZ   PIC  X(0003).                         
             10 PI-DRKI3506-G11VWAGNISKZ   PIC  X(0003).                        
             10 PI-DRKI3506-G11VWAGNISTXT   PIC  X(0025).                       
             10 PI-DRKI3506-G11VWERKBIND   PIC  X(0001).                        
             10 PI-DRKI3506-G11VWOHNEIGEN   PIC  X(0010).                       
             10 PI-DRKI3506-G11VZAHLART   PIC  X(0001).                         
             10 PI-DRKI3506-G11VZAHLWEISE   PIC  X(0001).                       
             10 PI-DRKI3506-G11VZULDATUM   PIC  X(0008).                        
             10 PI-DRKI3506-G11VZUNACH.                                         
                49 PI-DRKI3506-G11VZUNACH-LEN PIC S9(4) COMP.                   
                49 PI-DRKI3506-G11VZUNACH-TEXT PIC X(925).                      
             10 PI-DRKI3506-G11VZUSTAND   PIC  X(0001).                         
             10 PI-DRKI3506-G11VZWSCHL   PIC  X(0002).                          
             10 PI-DRKI3506-G11VZWSCHLTXT   PIC  X(0003).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3506-NI1.                                                     
           05 PI-DRKI3506-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 93.                                
       01  PI-DRKI3506-NI7.                                                     
           05 PI-DRKI3506-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 93.                                
                                                                                
      *                                                                         
       01  PO-DRKI3506.                                                         
           05  PO-DRKI3506-RC                     PIC  X(003).                  
               88 PO-DRKI3506-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3506-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
