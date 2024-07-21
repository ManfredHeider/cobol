      *****************************************************************         
CCC005*    EXEC SQL DECLARE G10KSDSKFZVERTR TABLE                               
CCC004*    ( G10VVERTRAGSNR                 CHAR(15) NOT NULL,                  
CCC004*      G10VMANDANT                    INTEGER NOT NULL,                   
CCC004*      G10VAENDKZ                     CHAR(2) NOT NULL,                   
CCC004*      G10VPIN                        CHAR(9) NOT NULL,                   
CCC004*      G10VPINPRUEF                   CHAR(2) NOT NULL,                   
CCC004*      G10VBEGINN                     CHAR(8) NOT NULL,                   
CCC004*      G10VABLAUF                     CHAR(8) NOT NULL,                   
CCC004*      G10VZUSTAND                    CHAR(1) NOT NULL,                   
CCC004*      G10VZAHLWEISE                  CHAR(1) NOT NULL,                   
CCC004*      G10VBEITRAG                    DECIMAL(10, 2) NOT NULL,            
CCC004*      G10VABSCHLVBNR                 INTEGER NOT NULL,                   
CCC004*      G10VAENDDAT                    CHAR(8) NOT NULL,                   
CCC004*      G10VLETZTAEND                  CHAR(8) NOT NULL,                   
CCC004*      G10VZAHLART                    CHAR(1) NOT NULL,                   
CCC004*      G10STORNODAT                   CHAR(8) NOT NULL,                   
CCC004*      G10VBUENDKZ                    CHAR(1) NOT NULL,                   
CCC004*      G10VWAEHRKZ                    CHAR(3) NOT NULL,                   
CCC004*      G10VZWSCHL                     CHAR(2) NOT NULL,                   
CCC004*      G10VZWSCHLTXT                  CHAR(3) NOT NULL,                   
CCC004*      G10VLSVSCOR                    CHAR(1) NOT NULL,                   
CCC004*      G10VAMTKZ                      CHAR(12) NOT NULL,                  
CCC004*      G10VAKZARTKNR                  CHAR(1) NOT NULL,                   
CCC004*      G10VSAISONKZ                   CHAR(1) NOT NULL,                   
CCC004*      G10VSAISONBEG                  CHAR(2) NOT NULL,                   
CCC004*      G10VSAISONEND                  CHAR(2) NOT NULL,                   
CCC004*      G10VZULDATUM                   CHAR(8) NOT NULL,                   
CCC004*      G10VERSTZULDAT                 CHAR(8) NOT NULL,                   
CCC004*      G10VFZGARTKNR                  CHAR(3) NOT NULL,                   
CCC004*      G10VFZGVERWKNR                 CHAR(3) NOT NULL,                   
CCC004*      G10VNAESPEZKNR                 CHAR(3) NOT NULL,                   
CCC004*      G10VWAGNISKZ                   CHAR(3) NOT NULL,                   
CCC004*      G10VWAGNISTXT                  CHAR(25) NOT NULL,                  
CCC004*      G10VHERSTNR                    CHAR(4) NOT NULL,                   
CCC004*      G10VHERSTTXT                   CHAR(25) NOT NULL,                  
CCC004*      G10VFZGMODELL                  CHAR(50) NOT NULL,                  
CCC004*      G10VKFZTYP                     CHAR(3) NOT NULL,                   
CCC004*      G10VFAHRZGID                   CHAR(20) NOT NULL,                  
CCC004*      G10VKFZLEISTKZ                 CHAR(1) NOT NULL,                   
CCC004*      G10KFZLEISTUNG                 DECIMAL(7, 2) NOT NULL,             
CCC004*      G10VPLATZ                      CHAR(3) NOT NULL,                   
CCC004*      G10VGESGEWICHT                 DECIMAL(7, 0) NOT NULL,             
CCC004*      G10VKFZKW                      DECIMAL(4, 0) NOT NULL,             
CCC004*      G10VNUTZL                      DECIMAL(7, 0) NOT NULL,             
CCC004*      G10VNEUWERT                    DECIMAL(7, 0) NOT NULL,             
CCC004*      G10VJAHRKMLEIST                DECIMAL(7, 0) NOT NULL,             
CCC004*      G10VKMSTAAKT                   DECIMAL(7, 0) NOT NULL,             
CCC004*      G10VKMSTAAKTDAT                CHAR(8) NOT NULL,                   
CCC004*      G10VPAKETTYP                   CHAR(4) NOT NULL,                   
CCC004*      G10VPAKETTEXT                  CHAR(150) NOT NULL,                 
CCC004*      G10VFAHRER                     CHAR(10) NOT NULL,                  
CCC004*      G10VGEBDATFAH                  CHAR(10) NOT NULL,                  
CCC004*      G10FALTERMAX                   CHAR(2) NOT NULL,                   
CCC004*      G10VALTERGRPKNR                CHAR(4) NOT NULL,                   
CCC004*      G10VBEGLFAHR                   CHAR(2) NOT NULL,                   
CCC004*      G10BEGLFAHRTXT                 CHAR(10) NOT NULL,                  
CCC004*      G10VFZGHALTER                  CHAR(2) NOT NULL,                   
CCC004*      G10VFZHGHALTTXT                CHAR(10) NOT NULL,                  
CCC004*      G10VFZGHALTNAME                CHAR(40) NOT NULL,                  
CCC004*      G10VFZGHALTVNAME               CHAR(40) NOT NULL,                  
CCC004*      G10VFZGNUTZ                    CHAR(10) NOT NULL,                  
CCC004*      G10VFZGABSTELL                 CHAR(10) NOT NULL,                  
CCC004*      G10VFZGAUFBAU                  CHAR(4) NOT NULL,                   
CCC004*      G10VFAHRGEBIET                 CHAR(10) NOT NULL,                  
CCC004*      G10VFSCHEINDAT                 CHAR(10) NOT NULL,                  
CCC004*      G10VWOHNEIGEN                  CHAR(10) NOT NULL,                  
CCC004*      G10VBRANCHE                    CHAR(60) NOT NULL,                  
CCC004*      G10VTELEMATIK                  DECIMAL(14, 0) NOT NULL,            
CCC004*      G10VSCOREWERT                  DECIMAL(3, 0) NOT NULL,             
CCC004*      G10VSCOREDAT                   CHAR(8) NOT NULL,                   
CCC004*      G10VTARAUSW                    CHAR(1) NOT NULL,                   
CCC004*      G10VWERKBIND                   CHAR(1) NOT NULL,                   
CCC004*      G10VDAKH                       CHAR(2) NOT NULL,                   
CCC004*      G10VRKLKH                      CHAR(4) NOT NULL,                   
CCC004*      G10VTYPKLKH                    CHAR(2) NOT NULL,                   
CCC004*      G10VBEITRKHZW                  DECIMAL(10, 2) NOT NULL,            
CCC004*      G10VDAKF                       CHAR(2) NOT NULL,                   
CCC004*      G10VRKLKF                      CHAR(4) NOT NULL,                   
CCC004*      G10VTYPKLKF                    CHAR(2) NOT NULL,                   
CCC004*      G10VSBVK                       DECIMAL(10, 2) NOT NULL,            
CCC004*      G10VSBTK                       DECIMAL(10, 2) NOT NULL,            
CCC004*      G10VBEITRKFZW                  DECIMAL(10, 2) NOT NULL,            
CCC004*      G10VLEASING                    CHAR(1) NOT NULL,                   
CCC004*      G10VSCHUTZBR                   CHAR(10) NOT NULL,                  
CCC004*      G10VCOCDAT                     CHAR(8) NOT NULL,                   
CCC004*      G10VCOCKZ                      CHAR(1) NOT NULL,                   
CCC004*      G10VSFRTAB                     VARCHAR(30) NOT NULL,               
CCC004*      G10VVERSBED                    VARCHAR(63) NOT NULL,               
CCC004*      G10VTARIFBEZ                   VARCHAR(21) NOT NULL,               
CCC004*      G10VKSMVERTR                   VARCHAR(464) NOT NULL,              
CCC004*      G10VBESVEREIN                  VARCHAR(750) NOT NULL,              
CCC004*      G10VSONDERAUS                  VARCHAR(240) NOT NULL,              
CCC004*      G10VZUNACH                     VARCHAR(925) NOT NULL,              
CCC004*      G10VELEKTROKZ                  CHAR(1) NOT NULL,                   
CCC004*      G10VFIRMENNR                   CHAR(6),                            
CCC004*      G10VGEBDAT_VN                  CHAR(10) NOT NULL                   
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G10KSDSKFZVERTR             *               
      ******************************************************************        
       01  G10KSDSKFZVERTR-GESAMT.                                              
        05 NI-G10KSDSKFZVERTR-TAB.                                              
           10 NI-G10KSDSKFZVERTR-ELE PIC S9(04) COMP OCCURS 95 TIMES.           
        05 NI-G10KSDSKFZVERTR REDEFINES NI-G10KSDSKFZVERTR-TAB.                 
           10 NI-G10VVERTRAGSNR     PIC S9(4) COMP.                             
           10 NI-G10VMANDANT        PIC S9(4) COMP.                             
           10 NI-G10VAENDKZ         PIC S9(4) COMP.                             
           10 NI-G10VPIN            PIC S9(4) COMP.                             
           10 NI-G10VPINPRUEF       PIC S9(4) COMP.                             
           10 NI-G10VBEGINN         PIC S9(4) COMP.                             
           10 NI-G10VABLAUF         PIC S9(4) COMP.                             
           10 NI-G10VZUSTAND        PIC S9(4) COMP.                             
           10 NI-G10VZAHLWEISE      PIC S9(4) COMP.                             
           10 NI-G10VBEITRAG        PIC S9(4) COMP.                             
           10 NI-G10VABSCHLVBNR     PIC S9(4) COMP.                             
           10 NI-G10VAENDDAT        PIC S9(4) COMP.                             
           10 NI-G10VLETZTAEND      PIC S9(4) COMP.                             
           10 NI-G10VZAHLART        PIC S9(4) COMP.                             
           10 NI-G10STORNODAT       PIC S9(4) COMP.                             
           10 NI-G10VBUENDKZ        PIC S9(4) COMP.                             
           10 NI-G10VWAEHRKZ        PIC S9(4) COMP.                             
           10 NI-G10VZWSCHL         PIC S9(4) COMP.                             
           10 NI-G10VZWSCHLTXT      PIC S9(4) COMP.                             
           10 NI-G10VLSVSCOR        PIC S9(4) COMP.                             
           10 NI-G10VAMTKZ          PIC S9(4) COMP.                             
           10 NI-G10VAKZARTKNR      PIC S9(4) COMP.                             
           10 NI-G10VSAISONKZ       PIC S9(4) COMP.                             
           10 NI-G10VSAISONBEG      PIC S9(4) COMP.                             
           10 NI-G10VSAISONEND      PIC S9(4) COMP.                             
           10 NI-G10VZULDATUM       PIC S9(4) COMP.                             
           10 NI-G10VERSTZULDAT     PIC S9(4) COMP.                             
           10 NI-G10VFZGARTKNR      PIC S9(4) COMP.                             
           10 NI-G10VFZGVERWKNR     PIC S9(4) COMP.                             
           10 NI-G10VNAESPEZKNR     PIC S9(4) COMP.                             
           10 NI-G10VWAGNISKZ       PIC S9(4) COMP.                             
           10 NI-G10VWAGNISTXT      PIC S9(4) COMP.                             
           10 NI-G10VHERSTNR        PIC S9(4) COMP.                             
           10 NI-G10VHERSTTXT       PIC S9(4) COMP.                             
           10 NI-G10VFZGMODELL      PIC S9(4) COMP.                             
           10 NI-G10VKFZTYP         PIC S9(4) COMP.                             
           10 NI-G10VFAHRZGID       PIC S9(4) COMP.                             
           10 NI-G10VKFZLEISTKZ     PIC S9(4) COMP.                             
           10 NI-G10KFZLEISTUNG     PIC S9(4) COMP.                             
           10 NI-G10VPLATZ          PIC S9(4) COMP.                             
           10 NI-G10VGESGEWICHT     PIC S9(4) COMP.                             
           10 NI-G10VKFZKW          PIC S9(4) COMP.                             
           10 NI-G10VNUTZL          PIC S9(4) COMP.                             
           10 NI-G10VNEUWERT        PIC S9(4) COMP.                             
           10 NI-G10VJAHRKMLEIST    PIC S9(4) COMP.                             
           10 NI-G10VKMSTAAKT       PIC S9(4) COMP.                             
           10 NI-G10VKMSTAAKTDAT    PIC S9(4) COMP.                             
           10 NI-G10VPAKETTYP       PIC S9(4) COMP.                             
           10 NI-G10VPAKETTEXT      PIC S9(4) COMP.                             
           10 NI-G10VFAHRER         PIC S9(4) COMP.                             
           10 NI-G10VGEBDATFAH      PIC S9(4) COMP.                             
           10 NI-G10FALTERMAX       PIC S9(4) COMP.                             
           10 NI-G10VALTERGRPKNR    PIC S9(4) COMP.                             
           10 NI-G10VBEGLFAHR       PIC S9(4) COMP.                             
           10 NI-G10BEGLFAHRTXT     PIC S9(4) COMP.                             
           10 NI-G10VFZGHALTER      PIC S9(4) COMP.                             
           10 NI-G10VFZHGHALTTXT    PIC S9(4) COMP.                             
           10 NI-G10VFZGHALTNAME    PIC S9(4) COMP.                             
           10 NI-G10VFZGHALTVNAME   PIC S9(4) COMP.                             
           10 NI-G10VFZGNUTZ        PIC S9(4) COMP.                             
           10 NI-G10VFZGABSTELL     PIC S9(4) COMP.                             
           10 NI-G10VFZGAUFBAU      PIC S9(4) COMP.                             
           10 NI-G10VFAHRGEBIET     PIC S9(4) COMP.                             
           10 NI-G10VFSCHEINDAT     PIC S9(4) COMP.                             
           10 NI-G10VWOHNEIGEN      PIC S9(4) COMP.                             
           10 NI-G10VBRANCHE        PIC S9(4) COMP.                             
           10 NI-G10VTELEMATIK      PIC S9(4) COMP.                             
           10 NI-G10VSCOREWERT      PIC S9(4) COMP.                             
           10 NI-G10VSCOREDAT       PIC S9(4) COMP.                             
           10 NI-G10VTARAUSW        PIC S9(4) COMP.                             
           10 NI-G10VWERKBIND       PIC S9(4) COMP.                             
           10 NI-G10VDAKH           PIC S9(4) COMP.                             
           10 NI-G10VRKLKH          PIC S9(4) COMP.                             
           10 NI-G10VTYPKLKH        PIC S9(4) COMP.                             
           10 NI-G10VBEITRKHZW      PIC S9(4) COMP.                             
           10 NI-G10VDAKF           PIC S9(4) COMP.                             
           10 NI-G10VRKLKF          PIC S9(4) COMP.                             
           10 NI-G10VTYPKLKF        PIC S9(4) COMP.                             
           10 NI-G10VSBVK           PIC S9(4) COMP.                             
           10 NI-G10VSBTK           PIC S9(4) COMP.                             
           10 NI-G10VBEITRKFZW      PIC S9(4) COMP.                             
           10 NI-G10VLEASING        PIC S9(4) COMP.                             
           10 NI-G10VSCHUTZBR       PIC S9(4) COMP.                             
           10 NI-G10VCOCDAT         PIC S9(4) COMP.                             
           10 NI-G10VCOCKZ          PIC S9(4) COMP.                             
           10 NI-G10VSFRTAB         PIC S9(4) COMP.                             
           10 NI-G10VVERSBED        PIC S9(4) COMP.                             
           10 NI-G10VTARIFBEZ       PIC S9(4) COMP.                             
           10 NI-G10VKSMVERTR       PIC S9(4) COMP.                             
           10 NI-G10VBESVEREIN      PIC S9(4) COMP.                             
           10 NI-G10VSONDERAUS      PIC S9(4) COMP.                             
           10 NI-G10VZUNACH         PIC S9(4) COMP.                             
           10 NI-G10VELEKTROKZ      PIC S9(4) COMP.                             
           10 NI-G10VFIRMENNR       PIC S9(4) COMP.                             
           10 NI-G10VGEBDAT-VN      PIC S9(4) COMP.                             
        05 DCL-G10KSDSKFZVERTR.                                                 
           10 G10VVERTRAGSNR       PIC X(15).                                   
           10 G10VMANDANT          PIC S9(9) USAGE COMP-5.                      
           10 G10VAENDKZ           PIC X(2).                                    
           10 G10VPIN              PIC X(9).                                    
           10 G10VPINPRUEF         PIC X(2).                                    
           10 G10VBEGINN           PIC X(8).                                    
           10 G10VABLAUF           PIC X(8).                                    
           10 G10VZUSTAND          PIC X(1).                                    
           10 G10VZAHLWEISE        PIC X(1).                                    
           10 G10VBEITRAG          PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G10VABSCHLVBNR       PIC S9(9) USAGE COMP-5.                      
           10 G10VAENDDAT          PIC X(8).                                    
           10 G10VLETZTAEND        PIC X(8).                                    
           10 G10VZAHLART          PIC X(1).                                    
           10 G10STORNODAT         PIC X(8).                                    
           10 G10VBUENDKZ          PIC X(1).                                    
           10 G10VWAEHRKZ          PIC X(3).                                    
           10 G10VZWSCHL           PIC X(2).                                    
           10 G10VZWSCHLTXT        PIC X(3).                                    
           10 G10VLSVSCOR          PIC X(1).                                    
           10 G10VAMTKZ            PIC X(12).                                   
           10 G10VAKZARTKNR        PIC X(1).                                    
           10 G10VSAISONKZ         PIC X(1).                                    
           10 G10VSAISONBEG        PIC X(2).                                    
           10 G10VSAISONEND        PIC X(2).                                    
           10 G10VZULDATUM         PIC X(8).                                    
           10 G10VERSTZULDAT       PIC X(8).                                    
           10 G10VFZGARTKNR        PIC X(3).                                    
           10 G10VFZGVERWKNR       PIC X(3).                                    
           10 G10VNAESPEZKNR       PIC X(3).                                    
           10 G10VWAGNISKZ         PIC X(3).                                    
           10 G10VWAGNISTXT        PIC X(25).                                   
           10 G10VHERSTNR          PIC X(4).                                    
           10 G10VHERSTTXT         PIC X(25).                                   
           10 G10VFZGMODELL        PIC X(50).                                   
           10 G10VKFZTYP           PIC X(3).                                    
           10 G10VFAHRZGID         PIC X(20).                                   
           10 G10VKFZLEISTKZ       PIC X(1).                                    
           10 G10KFZLEISTUNG       PIC S9(5)V9(2) USAGE COMP-3.                 
           10 G10VPLATZ            PIC X(3).                                    
           10 G10VGESGEWICHT       PIC S9(7)V USAGE COMP-3.                     
           10 G10VKFZKW            PIC S9(4)V USAGE COMP-3.                     
           10 G10VNUTZL            PIC S9(7)V USAGE COMP-3.                     
           10 G10VNEUWERT          PIC S9(7)V USAGE COMP-3.                     
           10 G10VJAHRKMLEIST      PIC S9(7)V USAGE COMP-3.                     
           10 G10VKMSTAAKT         PIC S9(7)V USAGE COMP-3.                     
           10 G10VKMSTAAKTDAT      PIC X(8).                                    
           10 G10VPAKETTYP         PIC X(4).                                    
           10 G10VPAKETTEXT        PIC X(150).                                  
           10 G10VFAHRER           PIC X(10).                                   
           10 G10VGEBDATFAH        PIC X(10).                                   
           10 G10FALTERMAX         PIC X(2).                                    
           10 G10VALTERGRPKNR      PIC X(4).                                    
           10 G10VBEGLFAHR         PIC X(2).                                    
           10 G10BEGLFAHRTXT       PIC X(10).                                   
           10 G10VFZGHALTER        PIC X(2).                                    
           10 G10VFZHGHALTTXT      PIC X(10).                                   
           10 G10VFZGHALTNAME      PIC X(40).                                   
           10 G10VFZGHALTVNAME     PIC X(40).                                   
           10 G10VFZGNUTZ          PIC X(10).                                   
           10 G10VFZGABSTELL       PIC X(10).                                   
           10 G10VFZGAUFBAU        PIC X(4).                                    
           10 G10VFAHRGEBIET       PIC X(10).                                   
           10 G10VFSCHEINDAT       PIC X(10).                                   
           10 G10VWOHNEIGEN        PIC X(10).                                   
           10 G10VBRANCHE          PIC X(60).                                   
           10 G10VTELEMATIK        PIC S9(14)V USAGE COMP-3.                    
           10 G10VSCOREWERT        PIC S9(3)V USAGE COMP-3.                     
           10 G10VSCOREDAT         PIC X(8).                                    
           10 G10VTARAUSW          PIC X(1).                                    
           10 G10VWERKBIND         PIC X(1).                                    
           10 G10VDAKH             PIC X(2).                                    
           10 G10VRKLKH            PIC X(4).                                    
           10 G10VTYPKLKH          PIC X(2).                                    
           10 G10VBEITRKHZW        PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G10VDAKF             PIC X(2).                                    
           10 G10VRKLKF            PIC X(4).                                    
           10 G10VTYPKLKF          PIC X(2).                                    
           10 G10VSBVK             PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G10VSBTK             PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G10VBEITRKFZW        PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G10VLEASING          PIC X(1).                                    
           10 G10VSCHUTZBR         PIC X(10).                                   
           10 G10VCOCDAT           PIC X(8).                                    
           10 G10VCOCKZ            PIC X(1).                                    
           10 G10VSFRTAB.                                                       
              49 G10VSFRTAB-LEN    PIC S9(4) USAGE COMP-5.                      
              49 G10VSFRTAB-TEXT   PIC X(30).                                   
           10 G10VVERSBED.                                                      
              49 G10VVERSBED-LEN   PIC S9(4) USAGE COMP-5.                      
              49 G10VVERSBED-TEXT                                               
                 PIC X(63).                                                     
           10 G10VTARIFBEZ.                                                     
              49 G10VTARIFBEZ-LEN                                               
                 PIC S9(4) USAGE COMP-5.                                        
              49 G10VTARIFBEZ-TEXT                                              
                 PIC X(21).                                                     
           10 G10VKSMVERTR.                                                     
              49 G10VKSMVERTR-LEN                                               
                 PIC S9(4) USAGE COMP-5.                                        
              49 G10VKSMVERTR-TEXT                                              
                 PIC X(464).                                                    
           10 G10VBESVEREIN.                                                    
              49 G10VBESVEREIN-LEN                                              
                 PIC S9(4) USAGE COMP-5.                                        
              49 G10VBESVEREIN-TEXT                                             
                 PIC X(750).                                                    
           10 G10VSONDERAUS.                                                    
              49 G10VSONDERAUS-LEN                                              
                 PIC S9(4) USAGE COMP-5.                                        
              49 G10VSONDERAUS-TEXT                                             
                 PIC X(240).                                                    
           10 G10VZUNACH.                                                       
              49 G10VZUNACH-LEN    PIC S9(4) USAGE COMP-5.                      
              49 G10VZUNACH-TEXT   PIC X(925).                                  
           10 G10VELEKTROKZ        PIC X(1).                                    
           10 G10VFIRMENNR         PIC X(6).                                    
           10 G10VGEBDAT-VN        PIC X(10).                                   
      ************************** ENDE-DCLGEN *************************          
