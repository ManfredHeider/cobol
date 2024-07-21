      *****************************************************************         
        05  WU-WUKI0489.                                                        
           10 G10VVERTRAGSNR                PIC  X(0015).                       
           10 G10VMANDANT                   PIC S9(9) COMP.                     
           10 G10VAENDKZ                    PIC  X(0002).                       
           10 G10VPIN                       PIC  X(0009).                       
           10 G10VPINPRUEF                  PIC  X(0002).                       
           10 G10VBEGINN                    PIC  X(0008).                       
           10 G10VABLAUF                    PIC  X(0008).                       
           10 G10VZUSTAND                   PIC  X(0001).                       
           10 G10VZAHLWEISE                 PIC  X(0001).                       
           10 G10VBEITRAG                   PIC S9(8)V9(2) COMP-3.              
           10 G10VABSCHLVBNR                PIC S9(9) COMP.                     
           10 G10VAENDDAT                   PIC  X(0008).                       
           10 G10VLETZTAEND                 PIC  X(0008).                       
           10 G10VZAHLART                   PIC  X(0001).                       
           10 G10STORNODAT                  PIC  X(0008).                       
           10 G10VBUENDKZ                   PIC  X(0001).                       
           10 G10VWAEHRKZ                   PIC  X(0003).                       
           10 G10VZWSCHL                    PIC  X(0002).                       
           10 G10VZWSCHLTXT                 PIC  X(0003).                       
           10 G10VLSVSCOR                   PIC  X(0001).                       
           10 G10VAMTKZ                     PIC  X(0012).                       
           10 G10VAKZARTKNR                 PIC  X(0001).                       
           10 G10VSAISONKZ                  PIC  X(0001).                       
           10 G10VSAISONBEG                 PIC  X(0002).                       
           10 G10VSAISONEND                 PIC  X(0002).                       
           10 G10VZULDATUM                  PIC  X(0008).                       
           10 G10VERSTZULDAT                PIC  X(0008).                       
           10 G10VFZGARTKNR                 PIC  X(0003).                       
           10 G10VFZGVERWKNR                PIC  X(0003).                       
           10 G10VNAESPEZKNR                PIC  X(0003).                       
           10 G10VWAGNISKZ                  PIC  X(0003).                       
           10 G10VWAGNISTXT                 PIC  X(0025).                       
           10 G10VHERSTNR                   PIC  X(0004).                       
           10 G10VHERSTTXT                  PIC  X(0025).                       
           10 G10VFZGMODELL                 PIC  X(0050).                       
           10 G10VKFZTYP                    PIC  X(0003).                       
           10 G10VFAHRZGID                  PIC  X(0020).                       
           10 G10VKFZLEISTKZ                PIC  X(0001).                       
           10 G10KFZLEISTUNG                PIC S9(5)V9(2) COMP-3.              
           10 G10VPLATZ                     PIC  X(0003).                       
           10 G10VGESGEWICHT                PIC S9(7)V COMP-3.                  
           10 G10VKFZKW                     PIC S9(4)V COMP-3.                  
           10 G10VNUTZL                     PIC S9(7)V COMP-3.                  
           10 G10VNEUWERT                   PIC S9(7)V COMP-3.                  
           10 G10VJAHRKMLEIST               PIC S9(7)V COMP-3.                  
           10 G10VKMSTAAKT                  PIC S9(7)V COMP-3.                  
           10 G10VKMSTAAKTDAT               PIC  X(0008).                       
           10 G10VPAKETTYP                  PIC  X(0004).                       
           10 G10VPAKETTEXT                 PIC  X(0150).                       
           10 G10VFAHRER                    PIC  X(0010).                       
           10 G10VGEBDATFAH                 PIC  X(0010).                       
           10 G10FALTERMAX                  PIC  X(0002).                       
           10 G10VALTERGRPKNR               PIC  X(0004).                       
           10 G10VBEGLFAHR                  PIC  X(0002).                       
           10 G10BEGLFAHRTXT                PIC  X(0010).                       
           10 G10VFZGHALTER                 PIC  X(0002).                       
           10 G10VFZHGHALTTXT               PIC  X(0010).                       
           10 G10VFZGHALTNAME               PIC  X(0040).                       
           10 G10VFZGHALTVNAME              PIC  X(0040).                       
           10 G10VFZGNUTZ                   PIC  X(0010).                       
           10 G10VFZGABSTELL                PIC  X(0010).                       
           10 G10VFZGAUFBAU                 PIC  X(0004).                       
           10 G10VFAHRGEBIET                PIC  X(0010).                       
           10 G10VFSCHEINDAT                PIC  X(0010).                       
           10 G10VWOHNEIGEN                 PIC  X(0010).                       
           10 G10VBRANCHE                   PIC  X(0060).                       
           10 G10VTELEMATIK                 PIC S9(14)V COMP-3.                 
           10 G10VSCOREWERT                 PIC S9(3)V COMP-3.                  
           10 G10VSCOREDAT                  PIC  X(0008).                       
           10 G10VTARAUSW                   PIC  X(0001).                       
           10 G10VWERKBIND                  PIC  X(0001).                       
           10 G10VDAKH                      PIC  X(0002).                       
           10 G10VRKLKH                     PIC  X(0004).                       
           10 G10VTYPKLKH                   PIC  X(0002).                       
           10 G10VBEITRKHZW                 PIC S9(8)V9(2) COMP-3.              
           10 G10VDAKF                      PIC  X(0002).                       
           10 G10VRKLKF                     PIC  X(0004).                       
           10 G10VTYPKLKF                   PIC  X(0002).                       
           10 G10VSBVK                      PIC S9(8)V9(2) COMP-3.              
           10 G10VSBTK                      PIC S9(8)V9(2) COMP-3.              
           10 G10VBEITRKFZW                 PIC S9(8)V9(2) COMP-3.              
           10 G10VLEASING                   PIC  X(0001).                       
           10 G10VSCHUTZBR                  PIC  X(0010).                       
           10 G10VCOCDAT                    PIC  X(0008).                       
           10 G10VCOCKZ                     PIC  X(0001).                       
           10 G10VSFRTAB.                                                       
             49 G10VSFRTAB-LEN              PIC S9(4) COMP.                     
             49 G10VSFRTAB-TEXT             PIC X(30).                          
           10 G10VVERSBED.                                                      
             49 G10VVERSBED-LEN             PIC S9(4) COMP.                     
             49 G10VVERSBED-TEXT            PIC X(63).                          
           10 G10VTARIFBEZ.                                                     
             49 G10VTARIFBEZ-LEN            PIC S9(4) COMP.                     
             49 G10VTARIFBEZ-TEXT           PIC X(21).                          
           10 G10VKSMVERTR.                                                     
             49 G10VKSMVERTR-LEN            PIC S9(4) COMP.                     
             49 G10VKSMVERTR-TEXT           PIC X(464).                         
           10 G10VBESVEREIN.                                                    
             49 G10VBESVEREIN-LEN           PIC S9(4) COMP.                     
             49 G10VBESVEREIN-TEXT          PIC X(750).                         
           10 G10VSONDERAUS.                                                    
             49 G10VSONDERAUS-LEN           PIC S9(4) COMP.                     
             49 G10VSONDERAUS-TEXT          PIC X(240).                         
           10 G10VZUNACH.                                                       
             49 G10VZUNACH-LEN              PIC S9(4) COMP.                     
             49 G10VZUNACH-TEXT             PIC X(925).                         
           10 ZUGANG                        PIC  X(0032).                       
           10 VON                           PIC  X(0032).                       
           10 BIS                           PIC  X(0032).                       
           10 DCOP                          PIC  X(0001).                       
           10 NIL-DCOP                      PIC  X(01).                         
           10 G10VELEKTROKZ                 PIC  X(0001).                       
           10 G10VFIRMENNR                  PIC  X(0006).                       
           10 NIL-G10VFIRMENNR              PIC  X(01).                         
           10 G10VGEBDAT-VN                 PIC  X(0010).                       
