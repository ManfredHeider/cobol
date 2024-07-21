      *****************************************************************         
CCC005*    EXEC SQL DECLARE G11_KFZVERTR TABLE                                  
CCC004*    ( G11VVERTRAGSNR                 CHAR(15) NOT NULL,                  
CCC004*      G11VMANDANT                    INTEGER NOT NULL,                   
CCC004*      G11VAENDKZ                     CHAR(2) NOT NULL,                   
CCC004*      G11VPIN                        CHAR(9) NOT NULL,                   
CCC004*      G11VPINPRUEF                   CHAR(2) NOT NULL,                   
CCC004*      G11VBEGINN                     CHAR(8) NOT NULL,                   
CCC004*      G11VABLAUF                     CHAR(8) NOT NULL,                   
CCC004*      G11VZUSTAND                    CHAR(1) NOT NULL,                   
CCC004*      G11VZAHLWEISE                  CHAR(1) NOT NULL,                   
CCC004*      G11VBEITRAG                    DECIMAL(10, 2) NOT NULL,            
CCC004*      G11VABSCHLVBNR                 INTEGER NOT NULL,                   
CCC004*      G11VAENDDAT                    CHAR(8) NOT NULL,                   
CCC004*      G11VLETZTAEND                  CHAR(8) NOT NULL,                   
CCC004*      G11VZAHLART                    CHAR(1) NOT NULL,                   
CCC004*      G11STORNODAT                   CHAR(8) NOT NULL,                   
CCC004*      G11VBUENDKZ                    CHAR(1) NOT NULL,                   
CCC004*      G11VWAEHRKZ                    CHAR(3) NOT NULL,                   
CCC004*      G11VZWSCHL                     CHAR(2) NOT NULL,                   
CCC004*      G11VZWSCHLTXT                  CHAR(3) NOT NULL,                   
CCC004*      G11VLSVSCOR                    CHAR(1) NOT NULL,                   
CCC004*      G11VAMTKZ                      CHAR(12) NOT NULL,                  
CCC004*      G11VAKZARTKNR                  CHAR(1) NOT NULL,                   
CCC004*      G11VSAISONKZ                   CHAR(1) NOT NULL,                   
CCC004*      G11VSAISONBEG                  CHAR(2) NOT NULL,                   
CCC004*      G11VSAISONEND                  CHAR(2) NOT NULL,                   
CCC004*      G11VZULDATUM                   CHAR(8) NOT NULL,                   
CCC004*      G11VERSTZULDAT                 CHAR(8) NOT NULL,                   
CCC004*      G11VFZGARTKNR                  CHAR(3) NOT NULL,                   
CCC004*      G11VFZGVERWKNR                 CHAR(3) NOT NULL,                   
CCC004*      G11VNAESPEZKNR                 CHAR(3) NOT NULL,                   
CCC004*      G11VWAGNISKZ                   CHAR(3) NOT NULL,                   
CCC004*      G11VWAGNISTXT                  CHAR(25) NOT NULL,                  
CCC004*      G11VHERSTNR                    CHAR(4) NOT NULL,                   
CCC004*      G11VHERSTTXT                   CHAR(25) NOT NULL,                  
CCC004*      G11VFZGMODELL                  CHAR(50) NOT NULL,                  
CCC004*      G11VKFZTYP                     CHAR(3) NOT NULL,                   
CCC004*      G11VFAHRZGID                   CHAR(20) NOT NULL,                  
CCC004*      G11VKFZLEISTKZ                 CHAR(1) NOT NULL,                   
CCC004*      G11KFZLEISTUNG                 DECIMAL(7, 2) NOT NULL,             
CCC004*      G11VPLATZ                      CHAR(3) NOT NULL,                   
CCC004*      G11VGESGEWICHT                 DECIMAL(7, 0) NOT NULL,             
CCC004*      G11VKFZKW                      DECIMAL(4, 0) NOT NULL,             
CCC004*      G11VNUTZL                      DECIMAL(7, 0) NOT NULL,             
CCC004*      G11VNEUWERT                    DECIMAL(7, 0) NOT NULL,             
CCC004*      G11VJAHRKMLEIST                DECIMAL(7, 0) NOT NULL,             
CCC004*      G11VKMSTAAKT                   DECIMAL(7, 0) NOT NULL,             
CCC004*      G11VKMSTAAKTDAT                CHAR(8) NOT NULL,                   
CCC004*      G11VPAKETTYP                   CHAR(4) NOT NULL,                   
CCC004*      G11VPAKETTEXT                  CHAR(150) NOT NULL,                 
CCC004*      G11VFAHRER                     CHAR(10) NOT NULL,                  
CCC004*      G11VGEBDATFAH                  CHAR(10) NOT NULL,                  
CCC004*      G11FALTERMAX                   CHAR(2) NOT NULL,                   
CCC004*      G11VALTERGRPKNR                CHAR(4) NOT NULL,                   
CCC004*      G11VBEGLFAHR                   CHAR(2) NOT NULL,                   
CCC004*      G11BEGLFAHRTXT                 CHAR(10) NOT NULL,                  
CCC004*      G11VFZGHALTER                  CHAR(2) NOT NULL,                   
CCC004*      G11VFZHGHALTTXT                CHAR(10) NOT NULL,                  
CCC004*      G11VFZGHALTNAME                CHAR(40) NOT NULL,                  
CCC004*      G11VFZGHALTVNAME               CHAR(40) NOT NULL,                  
CCC004*      G11VFZGNUTZ                    CHAR(10) NOT NULL,                  
CCC004*      G11VFZGABSTELL                 CHAR(10) NOT NULL,                  
CCC004*      G11VFZGAUFBAU                  CHAR(4) NOT NULL,                   
CCC004*      G11VFAHRGEBIET                 CHAR(10) NOT NULL,                  
CCC004*      G11VFSCHEINDAT                 CHAR(10) NOT NULL,                  
CCC004*      G11VWOHNEIGEN                  CHAR(10) NOT NULL,                  
CCC004*      G11VBRANCHE                    CHAR(60) NOT NULL,                  
CCC004*      G11VTELEMATIK                  DECIMAL(14, 0) NOT NULL,            
CCC004*      G11VSCOREWERT                  DECIMAL(3, 0) NOT NULL,             
CCC004*      G11VSCOREDAT                   CHAR(8) NOT NULL,                   
CCC004*      G11VTARAUSW                    CHAR(1) NOT NULL,                   
CCC004*      G11VWERKBIND                   CHAR(1) NOT NULL,                   
CCC004*      G11VDAKH                       CHAR(2) NOT NULL,                   
CCC004*      G11VRKLKH                      CHAR(4) NOT NULL,                   
CCC004*      G11VTYPKLKH                    CHAR(2) NOT NULL,                   
CCC004*      G11VBEITRKHZW                  DECIMAL(10, 2) NOT NULL,            
CCC004*      G11VDAKF                       CHAR(2) NOT NULL,                   
CCC004*      G11VRKLKF                      CHAR(4) NOT NULL,                   
CCC004*      G11VTYPKLKF                    CHAR(2) NOT NULL,                   
CCC004*      G11VSBVK                       DECIMAL(10, 2) NOT NULL,            
CCC004*      G11VSBTK                       DECIMAL(10, 2) NOT NULL,            
CCC004*      G11VBEITRKFZW                  DECIMAL(10, 2) NOT NULL,            
CCC004*      G11VLEASING                    CHAR(1) NOT NULL,                   
CCC004*      G11VSCHUTZBR                   CHAR(10) NOT NULL,                  
CCC004*      G11VCOCDAT                     CHAR(8) NOT NULL,                   
CCC004*      G11VCOCKZ                      CHAR(1) NOT NULL,                   
CCC004*      G11VSFRTAB                     VARCHAR(30) NOT NULL,               
CCC004*      G11VVERSBED                    VARCHAR(63) NOT NULL,               
CCC004*      G11VTARIFBEZ                   VARCHAR(21) NOT NULL,               
CCC004*      G11VKSMVERTR                   VARCHAR(464) NOT NULL,              
CCC004*      G11VBESVEREIN                  VARCHAR(750) NOT NULL,              
CCC004*      G11VSONDERAUS                  VARCHAR(240) NOT NULL,              
CCC004*      G11VZUNACH                     VARCHAR(925) NOT NULL,              
CCC004*      G11VELEKTROKZ                  CHAR(1) NOT NULL,                   
CCC004*      G11VFIRMENNR                   CHAR(6)                             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G11_KFZVERTR                *               
      ******************************************************************        
       01  G11-KFZVERTR-GESAMT.                                                 
        05 NI-G11-KFZVERTR-TAB.                                                 
           10 NI-G11-KFZVERTR-ELE PIC S9(04) COMP OCCURS 94 TIMES.              
        05 NI-G11-KFZVERTR REDEFINES NI-G11-KFZVERTR-TAB.                       
           10 NI-G11VVERTRAGSNR     PIC S9(4) COMP.                             
           10 NI-G11VMANDANT        PIC S9(4) COMP.                             
           10 NI-G11VAENDKZ         PIC S9(4) COMP.                             
           10 NI-G11VPIN            PIC S9(4) COMP.                             
           10 NI-G11VPINPRUEF       PIC S9(4) COMP.                             
           10 NI-G11VBEGINN         PIC S9(4) COMP.                             
           10 NI-G11VABLAUF         PIC S9(4) COMP.                             
           10 NI-G11VZUSTAND        PIC S9(4) COMP.                             
           10 NI-G11VZAHLWEISE      PIC S9(4) COMP.                             
           10 NI-G11VBEITRAG        PIC S9(4) COMP.                             
           10 NI-G11VABSCHLVBNR     PIC S9(4) COMP.                             
           10 NI-G11VAENDDAT        PIC S9(4) COMP.                             
           10 NI-G11VLETZTAEND      PIC S9(4) COMP.                             
           10 NI-G11VZAHLART        PIC S9(4) COMP.                             
           10 NI-G11STORNODAT       PIC S9(4) COMP.                             
           10 NI-G11VBUENDKZ        PIC S9(4) COMP.                             
           10 NI-G11VWAEHRKZ        PIC S9(4) COMP.                             
           10 NI-G11VZWSCHL         PIC S9(4) COMP.                             
           10 NI-G11VZWSCHLTXT      PIC S9(4) COMP.                             
           10 NI-G11VLSVSCOR        PIC S9(4) COMP.                             
           10 NI-G11VAMTKZ          PIC S9(4) COMP.                             
           10 NI-G11VAKZARTKNR      PIC S9(4) COMP.                             
           10 NI-G11VSAISONKZ       PIC S9(4) COMP.                             
           10 NI-G11VSAISONBEG      PIC S9(4) COMP.                             
           10 NI-G11VSAISONEND      PIC S9(4) COMP.                             
           10 NI-G11VZULDATUM       PIC S9(4) COMP.                             
           10 NI-G11VERSTZULDAT     PIC S9(4) COMP.                             
           10 NI-G11VFZGARTKNR      PIC S9(4) COMP.                             
           10 NI-G11VFZGVERWKNR     PIC S9(4) COMP.                             
           10 NI-G11VNAESPEZKNR     PIC S9(4) COMP.                             
           10 NI-G11VWAGNISKZ       PIC S9(4) COMP.                             
           10 NI-G11VWAGNISTXT      PIC S9(4) COMP.                             
           10 NI-G11VHERSTNR        PIC S9(4) COMP.                             
           10 NI-G11VHERSTTXT       PIC S9(4) COMP.                             
           10 NI-G11VFZGMODELL      PIC S9(4) COMP.                             
           10 NI-G11VKFZTYP         PIC S9(4) COMP.                             
           10 NI-G11VFAHRZGID       PIC S9(4) COMP.                             
           10 NI-G11VKFZLEISTKZ     PIC S9(4) COMP.                             
           10 NI-G11KFZLEISTUNG     PIC S9(4) COMP.                             
           10 NI-G11VPLATZ          PIC S9(4) COMP.                             
           10 NI-G11VGESGEWICHT     PIC S9(4) COMP.                             
           10 NI-G11VKFZKW          PIC S9(4) COMP.                             
           10 NI-G11VNUTZL          PIC S9(4) COMP.                             
           10 NI-G11VNEUWERT        PIC S9(4) COMP.                             
           10 NI-G11VJAHRKMLEIST    PIC S9(4) COMP.                             
           10 NI-G11VKMSTAAKT       PIC S9(4) COMP.                             
           10 NI-G11VKMSTAAKTDAT    PIC S9(4) COMP.                             
           10 NI-G11VPAKETTYP       PIC S9(4) COMP.                             
           10 NI-G11VPAKETTEXT      PIC S9(4) COMP.                             
           10 NI-G11VFAHRER         PIC S9(4) COMP.                             
           10 NI-G11VGEBDATFAH      PIC S9(4) COMP.                             
           10 NI-G11FALTERMAX       PIC S9(4) COMP.                             
           10 NI-G11VALTERGRPKNR    PIC S9(4) COMP.                             
           10 NI-G11VBEGLFAHR       PIC S9(4) COMP.                             
           10 NI-G11BEGLFAHRTXT     PIC S9(4) COMP.                             
           10 NI-G11VFZGHALTER      PIC S9(4) COMP.                             
           10 NI-G11VFZHGHALTTXT    PIC S9(4) COMP.                             
           10 NI-G11VFZGHALTNAME    PIC S9(4) COMP.                             
           10 NI-G11VFZGHALTVNAME   PIC S9(4) COMP.                             
           10 NI-G11VFZGNUTZ        PIC S9(4) COMP.                             
           10 NI-G11VFZGABSTELL     PIC S9(4) COMP.                             
           10 NI-G11VFZGAUFBAU      PIC S9(4) COMP.                             
           10 NI-G11VFAHRGEBIET     PIC S9(4) COMP.                             
           10 NI-G11VFSCHEINDAT     PIC S9(4) COMP.                             
           10 NI-G11VWOHNEIGEN      PIC S9(4) COMP.                             
           10 NI-G11VBRANCHE        PIC S9(4) COMP.                             
           10 NI-G11VTELEMATIK      PIC S9(4) COMP.                             
           10 NI-G11VSCOREWERT      PIC S9(4) COMP.                             
           10 NI-G11VSCOREDAT       PIC S9(4) COMP.                             
           10 NI-G11VTARAUSW        PIC S9(4) COMP.                             
           10 NI-G11VWERKBIND       PIC S9(4) COMP.                             
           10 NI-G11VDAKH           PIC S9(4) COMP.                             
           10 NI-G11VRKLKH          PIC S9(4) COMP.                             
           10 NI-G11VTYPKLKH        PIC S9(4) COMP.                             
           10 NI-G11VBEITRKHZW      PIC S9(4) COMP.                             
           10 NI-G11VDAKF           PIC S9(4) COMP.                             
           10 NI-G11VRKLKF          PIC S9(4) COMP.                             
           10 NI-G11VTYPKLKF        PIC S9(4) COMP.                             
           10 NI-G11VSBVK           PIC S9(4) COMP.                             
           10 NI-G11VSBTK           PIC S9(4) COMP.                             
           10 NI-G11VBEITRKFZW      PIC S9(4) COMP.                             
           10 NI-G11VLEASING        PIC S9(4) COMP.                             
           10 NI-G11VSCHUTZBR       PIC S9(4) COMP.                             
           10 NI-G11VCOCDAT         PIC S9(4) COMP.                             
           10 NI-G11VCOCKZ          PIC S9(4) COMP.                             
           10 NI-G11VSFRTAB         PIC S9(4) COMP.                             
           10 NI-G11VVERSBED        PIC S9(4) COMP.                             
           10 NI-G11VTARIFBEZ       PIC S9(4) COMP.                             
           10 NI-G11VKSMVERTR       PIC S9(4) COMP.                             
           10 NI-G11VBESVEREIN      PIC S9(4) COMP.                             
           10 NI-G11VSONDERAUS      PIC S9(4) COMP.                             
           10 NI-G11VZUNACH         PIC S9(4) COMP.                             
           10 NI-G11VELEKTROKZ      PIC S9(4) COMP.                             
           10 NI-G11VFIRMENNR       PIC S9(4) COMP.                             
        05 DCL-G11-KFZVERTR.                                                    
           10 G11VVERTRAGSNR       PIC X(15).                                   
           10 G11VMANDANT          PIC S9(9) USAGE COMP-5.                      
           10 G11VAENDKZ           PIC X(2).                                    
           10 G11VPIN              PIC X(9).                                    
           10 G11VPINPRUEF         PIC X(2).                                    
           10 G11VBEGINN           PIC X(8).                                    
           10 G11VABLAUF           PIC X(8).                                    
           10 G11VZUSTAND          PIC X(1).                                    
           10 G11VZAHLWEISE        PIC X(1).                                    
           10 G11VBEITRAG          PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G11VABSCHLVBNR       PIC S9(9) USAGE COMP-5.                      
           10 G11VAENDDAT          PIC X(8).                                    
           10 G11VLETZTAEND        PIC X(8).                                    
           10 G11VZAHLART          PIC X(1).                                    
           10 G11STORNODAT         PIC X(8).                                    
           10 G11VBUENDKZ          PIC X(1).                                    
           10 G11VWAEHRKZ          PIC X(3).                                    
           10 G11VZWSCHL           PIC X(2).                                    
           10 G11VZWSCHLTXT        PIC X(3).                                    
           10 G11VLSVSCOR          PIC X(1).                                    
           10 G11VAMTKZ            PIC X(12).                                   
           10 G11VAKZARTKNR        PIC X(1).                                    
           10 G11VSAISONKZ         PIC X(1).                                    
           10 G11VSAISONBEG        PIC X(2).                                    
           10 G11VSAISONEND        PIC X(2).                                    
           10 G11VZULDATUM         PIC X(8).                                    
           10 G11VERSTZULDAT       PIC X(8).                                    
           10 G11VFZGARTKNR        PIC X(3).                                    
           10 G11VFZGVERWKNR       PIC X(3).                                    
           10 G11VNAESPEZKNR       PIC X(3).                                    
           10 G11VWAGNISKZ         PIC X(3).                                    
           10 G11VWAGNISTXT        PIC X(25).                                   
           10 G11VHERSTNR          PIC X(4).                                    
           10 G11VHERSTTXT         PIC X(25).                                   
           10 G11VFZGMODELL        PIC X(50).                                   
           10 G11VKFZTYP           PIC X(3).                                    
           10 G11VFAHRZGID         PIC X(20).                                   
           10 G11VKFZLEISTKZ       PIC X(1).                                    
           10 G11KFZLEISTUNG       PIC S9(5)V9(2) USAGE COMP-3.                 
           10 G11VPLATZ            PIC X(3).                                    
           10 G11VGESGEWICHT       PIC S9(7)V USAGE COMP-3.                     
           10 G11VKFZKW            PIC S9(4)V USAGE COMP-3.                     
           10 G11VNUTZL            PIC S9(7)V USAGE COMP-3.                     
           10 G11VNEUWERT          PIC S9(7)V USAGE COMP-3.                     
           10 G11VJAHRKMLEIST      PIC S9(7)V USAGE COMP-3.                     
           10 G11VKMSTAAKT         PIC S9(7)V USAGE COMP-3.                     
           10 G11VKMSTAAKTDAT      PIC X(8).                                    
           10 G11VPAKETTYP         PIC X(4).                                    
           10 G11VPAKETTEXT        PIC X(150).                                  
           10 G11VFAHRER           PIC X(10).                                   
           10 G11VGEBDATFAH        PIC X(10).                                   
           10 G11FALTERMAX         PIC X(2).                                    
           10 G11VALTERGRPKNR      PIC X(4).                                    
           10 G11VBEGLFAHR         PIC X(2).                                    
           10 G11BEGLFAHRTXT       PIC X(10).                                   
           10 G11VFZGHALTER        PIC X(2).                                    
           10 G11VFZHGHALTTXT      PIC X(10).                                   
           10 G11VFZGHALTNAME      PIC X(40).                                   
           10 G11VFZGHALTVNAME     PIC X(40).                                   
           10 G11VFZGNUTZ          PIC X(10).                                   
           10 G11VFZGABSTELL       PIC X(10).                                   
           10 G11VFZGAUFBAU        PIC X(4).                                    
           10 G11VFAHRGEBIET       PIC X(10).                                   
           10 G11VFSCHEINDAT       PIC X(10).                                   
           10 G11VWOHNEIGEN        PIC X(10).                                   
           10 G11VBRANCHE          PIC X(60).                                   
           10 G11VTELEMATIK        PIC S9(14)V USAGE COMP-3.                    
           10 G11VSCOREWERT        PIC S9(3)V USAGE COMP-3.                     
           10 G11VSCOREDAT         PIC X(8).                                    
           10 G11VTARAUSW          PIC X(1).                                    
           10 G11VWERKBIND         PIC X(1).                                    
           10 G11VDAKH             PIC X(2).                                    
           10 G11VRKLKH            PIC X(4).                                    
           10 G11VTYPKLKH          PIC X(2).                                    
           10 G11VBEITRKHZW        PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G11VDAKF             PIC X(2).                                    
           10 G11VRKLKF            PIC X(4).                                    
           10 G11VTYPKLKF          PIC X(2).                                    
           10 G11VSBVK             PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G11VSBTK             PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G11VBEITRKFZW        PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G11VLEASING          PIC X(1).                                    
           10 G11VSCHUTZBR         PIC X(10).                                   
           10 G11VCOCDAT           PIC X(8).                                    
           10 G11VCOCKZ            PIC X(1).                                    
           10 G11VSFRTAB.                                                       
              49 G11VSFRTAB-LEN    PIC S9(4) USAGE COMP-5.                      
              49 G11VSFRTAB-TEXT   PIC X(30).                                   
           10 G11VVERSBED.                                                      
              49 G11VVERSBED-LEN   PIC S9(4) USAGE COMP-5.                      
              49 G11VVERSBED-TEXT                                               
                 PIC X(63).                                                     
           10 G11VTARIFBEZ.                                                     
              49 G11VTARIFBEZ-LEN                                               
                 PIC S9(4) USAGE COMP-5.                                        
              49 G11VTARIFBEZ-TEXT                                              
                 PIC X(21).                                                     
           10 G11VKSMVERTR.                                                     
              49 G11VKSMVERTR-LEN                                               
                 PIC S9(4) USAGE COMP-5.                                        
              49 G11VKSMVERTR-TEXT                                              
                 PIC X(464).                                                    
           10 G11VBESVEREIN.                                                    
              49 G11VBESVEREIN-LEN                                              
                 PIC S9(4) USAGE COMP-5.                                        
              49 G11VBESVEREIN-TEXT                                             
                 PIC X(750).                                                    
           10 G11VSONDERAUS.                                                    
              49 G11VSONDERAUS-LEN                                              
                 PIC S9(4) USAGE COMP-5.                                        
              49 G11VSONDERAUS-TEXT                                             
                 PIC X(240).                                                    
           10 G11VZUNACH.                                                       
              49 G11VZUNACH-LEN    PIC S9(4) USAGE COMP-5.                      
              49 G11VZUNACH-TEXT   PIC X(925).                                  
           10 G11VELEKTROKZ        PIC X(1).                                    
           10 G11VFIRMENNR         PIC X(6).                                    
      ************************** ENDE-DCLGEN *************************          
