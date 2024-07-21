      *****************************************************************         
CCC005*    EXEC SQL DECLARE G70KSDSBADVERTR TABLE                               
CCC004*    ( G70VVERTRAGSNR                 CHAR(15) NOT NULL,                  
CCC004*      G70VLFDNR                      CHAR(1) NOT NULL,                   
CCC004*      G70VMANDANT                    INTEGER NOT NULL,                   
CCC004*      G70VAENDKZ                     CHAR(2) NOT NULL,                   
CCC004*      G70VPRODSCHL                   CHAR(2) NOT NULL,                   
CCC004*      G70VLAUFZEIT                   CHAR(2) NOT NULL,                   
CCC004*      G70VBEGINN                     CHAR(8) NOT NULL,                   
CCC004*      G70VABLAUF                     CHAR(8) NOT NULL,                   
CCC004*      G70VSUMME                      DECIMAL(10, 2) NOT NULL,            
CCC004*      G70VBEITRAG                    DECIMAL(7, 2) NOT NULL,             
CCC004*      G70VKTOSTAKZ                   CHAR(1) NOT NULL,                   
CCC004*      G70VBEWERTZAHL                 DECIMAL(8, 5) NOT NULL,             
CCC004*      G70VTARIFKZ                    CHAR(1) NOT NULL,                   
CCC004*      G70VZINSALLG                   DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VDARLZINS                   DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VDISPROZ                    DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VFMINANSPAR                 DECIMAL(10, 2) NOT NULL,            
CCC004*      G70VENDSTEULZ                  CHAR(8) NOT NULL,                   
CCC004*      G70VDATWOBPRAE                 CHAR(8) NOT NULL,                   
CCC004*      G70VDATZUTAUSSI                CHAR(8) NOT NULL,                   
CCC004*      G70VDATMINANSP                 CHAR(8) NOT NULL,                   
CCC004*      G70VPARTINFO                   CHAR(20) NOT NULL,                  
CCC004*      G70VABSCHLVBNR                 INTEGER NOT NULL,                   
CCC004*      G70VMINANSPPRO                 DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VZAHLW                      CHAR(2) NOT NULL,                   
CCC004*      G70VSPARBETRAG                 DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VBONUSVORJ                  DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VBWZVJ                      DECIMAL(8, 5) NOT NULL,             
CCC004*      G70VZINSLFDJ                   DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VWKZ                        CHAR(3) NOT NULL,                   
CCC004*      G70VLETZTAEND                  CHAR(8) NOT NULL,                   
CCC004*      G70VSCHLWOHNPRG                CHAR(2) NOT NULL,                   
CCC004*      G70VBASERHOEH                  DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VEHELKZ                     CHAR(1) NOT NULL,                   
CCC004*      G70VBLANKODARL                 CHAR(1) NOT NULL,                   
CCC004*      G70VERMITTKZ                   CHAR(1) NOT NULL,                   
CCC004*      G70VDATSPERFR                  CHAR(8) NOT NULL,                   
CCC004*      G70VZINSVOR                    DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VDATAGEIN                   CHAR(8) NOT NULL,                   
CCC004*      G70VFINPROD                    CHAR(2) NOT NULL,                   
CCC004*      G70VDZINSFAB                   CHAR(8) NOT NULL,                   
CCC004*      G70VDZINSFBIS                  CHAR(8) NOT NULL,                   
CCC004*      G70VDARLZINOM                  DECIMAL(6, 3) NOT NULL,             
CCC004*      G70VDARLZIEFF                  DECIMAL(6, 3) NOT NULL,             
CCC004*      G70VANNURATE                   DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VLZFIN                      CHAR(3) NOT NULL,                   
CCC004*      G70VSONDTILJ                   DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VDARLHOEHE                  DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VDARLSALDO                  DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VDATAFBEG                   CHAR(8) NOT NULL,                   
CCC004*      G70VBONUSFAKT                  DECIMAL(4, 2) NOT NULL,             
CCC004*      G70VBONUSAKT                   DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VZPGEN                      CHAR(2) NOT NULL,                   
CCC004*      G70VMTLEINZBETR                DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VMTLEINZDAT                 CHAR(8) NOT NULL,                   
CCC004*      G70VJAERLEINZBETR              DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VJAERLEINZDAT               CHAR(8) NOT NULL,                   
CCC004*      G70VDYNJAEREINZ                DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VMINBEWZAHL                 DECIMAL(6, 2) NOT NULL,             
CCC004*      G70VINKASSO                    CHAR(1) NOT NULL,                   
CCC004*      G70VSTEULFZDAT                 CHAR(8) NOT NULL,                   
CCC004*      G70VSUMVLEINZ                  DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VWOPKZ                      CHAR(1) NOT NULL,                   
CCC004*      G70VWOPVJ                      CHAR(4) NOT NULL,                   
CCC004*      G70VWOPVVJ                     CHAR(4) NOT NULL,                   
CCC004*      G70VDAUZULKZ                   CHAR(1) NOT NULL,                   
CCC004*      G70VFOERDRELEV                 DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VSALDOWFKTO                 DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VSUMAVWL                    DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VSUMFOERVJ                  DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VVARIWECHS                  CHAR(1) NOT NULL,                   
CCC004*      G70VFINANZKZ                   CHAR(1) NOT NULL,                   
CCC004*      G70VZUTEILANW                  CHAR(8) NOT NULL,                   
CCC004*      G70VDARLAKTEKZ                 CHAR(1) NOT NULL,                   
CCC004*      G70VENDBINDFRI                 CHAR(10) NOT NULL,                  
CCC004*      G70VDLETZTAKT                  CHAR(8) NOT NULL,                   
CCC004*      G70VZIELTILG                   DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VMEHRZUTEIL                 DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VABTRETBLZ                  CHAR(8) NOT NULL,                   
CCC004*      G70VTARIFUMST                  DECIMAL(7, 2) NOT NULL,             
CCC004*      G70VANFTILG                    DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VLETZTZUTEIL                CHAR(8) NOT NULL,                   
CCC004*      G70VANWZUTEIVO                 CHAR(8) NOT NULL,                   
CCC004*      G70VANSPWOP                    DECIMAL(9, 2) NOT NULL,             
CCC004*      G70VSPARZULAB                  CHAR(8) NOT NULL,                   
CCC004*      G70VVWLKZ                      CHAR(1) NOT NULL,                   
CCC004*      G70VIMMOFITKZ                  CHAR(1) NOT NULL,                   
CCC004*      G70VWERBVERBOT                 CHAR(1) NOT NULL,                   
CCC004*      G70VGUTHABBEGR                 CHAR(8) NOT NULL,                   
CCC004*      G70VMAXVZINSGH                 DECIMAL(11, 2) NOT NULL,            
CCC004*      G70VMIETKAUT                   CHAR(1) NOT NULL,                   
CCC004*      G70VPASSWORT                   CHAR(1) NOT NULL,                   
CCC004*      G70VLETZTKTOAZ                 CHAR(8) NOT NULL,                   
CCC004*      G70VNRAKTKTOAZ                 CHAR(7) NOT NULL,                   
CCC004*      G70VNVERANLBE                  CHAR(8) NOT NULL,                   
CCC004*      G70VVLPERSONAL                 CHAR(1) NOT NULL,                   
CCC004*      G70VONLPROD                    CHAR(50) NOT NULL,                  
CCC004*      G70VONLART                     CHAR(50) NOT NULL,                  
CCC004*      G70VFRONTENDT                  CHAR(8) NOT NULL,                   
CCC004*      G70VABBVERS                    CHAR(7) NOT NULL,                   
CCC004*      G70VJAHRZULAGE                 VARCHAR(105) NOT NULL,              
CCC004*      G70VBONUSART                   CHAR(2) NOT NULL,                   
CCC004*      G70VBONUSBEGDATUM              CHAR(8) NOT NULL,                   
CCC004*      G70VTUBAKTION                  CHAR(1) NOT NULL,                   
CCC004*      G70VSONDERZAHLUNG              DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VSONDERTILGUNG              DECIMAL(5, 2) NOT NULL,             
CCC004*      G70VPEAUSZAHLART               CHAR(1) NOT NULL                    
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE G70KSDSBADVERTR             *               
      ******************************************************************        
       01  G70KSDSBADVERTR-GESAMT.                                              
        05 NI-G70KSDSBADVERTR-TAB.                                              
           10 NI-G70KSDSBADVERTR-ELE PIC S9(04) COMP OCCURS 107 TIMES.          
        05 NI-G70KSDSBADVERTR REDEFINES NI-G70KSDSBADVERTR-TAB.                 
           10 NI-G70VVERTRAGSNR     PIC S9(4) COMP.                             
           10 NI-G70VLFDNR          PIC S9(4) COMP.                             
           10 NI-G70VMANDANT        PIC S9(4) COMP.                             
           10 NI-G70VAENDKZ         PIC S9(4) COMP.                             
           10 NI-G70VPRODSCHL       PIC S9(4) COMP.                             
           10 NI-G70VLAUFZEIT       PIC S9(4) COMP.                             
           10 NI-G70VBEGINN         PIC S9(4) COMP.                             
           10 NI-G70VABLAUF         PIC S9(4) COMP.                             
           10 NI-G70VSUMME          PIC S9(4) COMP.                             
           10 NI-G70VBEITRAG        PIC S9(4) COMP.                             
           10 NI-G70VKTOSTAKZ       PIC S9(4) COMP.                             
           10 NI-G70VBEWERTZAHL     PIC S9(4) COMP.                             
           10 NI-G70VTARIFKZ        PIC S9(4) COMP.                             
           10 NI-G70VZINSALLG       PIC S9(4) COMP.                             
           10 NI-G70VDARLZINS       PIC S9(4) COMP.                             
           10 NI-G70VDISPROZ        PIC S9(4) COMP.                             
           10 NI-G70VFMINANSPAR     PIC S9(4) COMP.                             
           10 NI-G70VENDSTEULZ      PIC S9(4) COMP.                             
           10 NI-G70VDATWOBPRAE     PIC S9(4) COMP.                             
           10 NI-G70VDATZUTAUSSI    PIC S9(4) COMP.                             
           10 NI-G70VDATMINANSP     PIC S9(4) COMP.                             
           10 NI-G70VPARTINFO       PIC S9(4) COMP.                             
           10 NI-G70VABSCHLVBNR     PIC S9(4) COMP.                             
           10 NI-G70VMINANSPPRO     PIC S9(4) COMP.                             
           10 NI-G70VZAHLW          PIC S9(4) COMP.                             
           10 NI-G70VSPARBETRAG     PIC S9(4) COMP.                             
           10 NI-G70VBONUSVORJ      PIC S9(4) COMP.                             
           10 NI-G70VBWZVJ          PIC S9(4) COMP.                             
           10 NI-G70VZINSLFDJ       PIC S9(4) COMP.                             
           10 NI-G70VWKZ            PIC S9(4) COMP.                             
           10 NI-G70VLETZTAEND      PIC S9(4) COMP.                             
           10 NI-G70VSCHLWOHNPRG    PIC S9(4) COMP.                             
           10 NI-G70VBASERHOEH      PIC S9(4) COMP.                             
           10 NI-G70VEHELKZ         PIC S9(4) COMP.                             
           10 NI-G70VBLANKODARL     PIC S9(4) COMP.                             
           10 NI-G70VERMITTKZ       PIC S9(4) COMP.                             
           10 NI-G70VDATSPERFR      PIC S9(4) COMP.                             
           10 NI-G70VZINSVOR        PIC S9(4) COMP.                             
           10 NI-G70VDATAGEIN       PIC S9(4) COMP.                             
           10 NI-G70VFINPROD        PIC S9(4) COMP.                             
           10 NI-G70VDZINSFAB       PIC S9(4) COMP.                             
           10 NI-G70VDZINSFBIS      PIC S9(4) COMP.                             
           10 NI-G70VDARLZINOM      PIC S9(4) COMP.                             
           10 NI-G70VDARLZIEFF      PIC S9(4) COMP.                             
           10 NI-G70VANNURATE       PIC S9(4) COMP.                             
           10 NI-G70VLZFIN          PIC S9(4) COMP.                             
           10 NI-G70VSONDTILJ       PIC S9(4) COMP.                             
           10 NI-G70VDARLHOEHE      PIC S9(4) COMP.                             
           10 NI-G70VDARLSALDO      PIC S9(4) COMP.                             
           10 NI-G70VDATAFBEG       PIC S9(4) COMP.                             
           10 NI-G70VBONUSFAKT      PIC S9(4) COMP.                             
           10 NI-G70VBONUSAKT       PIC S9(4) COMP.                             
           10 NI-G70VZPGEN          PIC S9(4) COMP.                             
           10 NI-G70VMTLEINZBETR    PIC S9(4) COMP.                             
           10 NI-G70VMTLEINZDAT     PIC S9(4) COMP.                             
           10 NI-G70VJAERLEINZBETR  PIC S9(4) COMP.                             
           10 NI-G70VJAERLEINZDAT   PIC S9(4) COMP.                             
           10 NI-G70VDYNJAEREINZ    PIC S9(4) COMP.                             
           10 NI-G70VMINBEWZAHL     PIC S9(4) COMP.                             
           10 NI-G70VINKASSO        PIC S9(4) COMP.                             
           10 NI-G70VSTEULFZDAT     PIC S9(4) COMP.                             
           10 NI-G70VSUMVLEINZ      PIC S9(4) COMP.                             
           10 NI-G70VWOPKZ          PIC S9(4) COMP.                             
           10 NI-G70VWOPVJ          PIC S9(4) COMP.                             
           10 NI-G70VWOPVVJ         PIC S9(4) COMP.                             
           10 NI-G70VDAUZULKZ       PIC S9(4) COMP.                             
           10 NI-G70VFOERDRELEV     PIC S9(4) COMP.                             
           10 NI-G70VSALDOWFKTO     PIC S9(4) COMP.                             
           10 NI-G70VSUMAVWL        PIC S9(4) COMP.                             
           10 NI-G70VSUMFOERVJ      PIC S9(4) COMP.                             
           10 NI-G70VVARIWECHS      PIC S9(4) COMP.                             
           10 NI-G70VFINANZKZ       PIC S9(4) COMP.                             
           10 NI-G70VZUTEILANW      PIC S9(4) COMP.                             
           10 NI-G70VDARLAKTEKZ     PIC S9(4) COMP.                             
           10 NI-G70VENDBINDFRI     PIC S9(4) COMP.                             
           10 NI-G70VDLETZTAKT      PIC S9(4) COMP.                             
           10 NI-G70VZIELTILG       PIC S9(4) COMP.                             
           10 NI-G70VMEHRZUTEIL     PIC S9(4) COMP.                             
           10 NI-G70VABTRETBLZ      PIC S9(4) COMP.                             
           10 NI-G70VTARIFUMST      PIC S9(4) COMP.                             
           10 NI-G70VANFTILG        PIC S9(4) COMP.                             
           10 NI-G70VLETZTZUTEIL    PIC S9(4) COMP.                             
           10 NI-G70VANWZUTEIVO     PIC S9(4) COMP.                             
           10 NI-G70VANSPWOP        PIC S9(4) COMP.                             
           10 NI-G70VSPARZULAB      PIC S9(4) COMP.                             
           10 NI-G70VVWLKZ          PIC S9(4) COMP.                             
           10 NI-G70VIMMOFITKZ      PIC S9(4) COMP.                             
           10 NI-G70VWERBVERBOT     PIC S9(4) COMP.                             
           10 NI-G70VGUTHABBEGR     PIC S9(4) COMP.                             
           10 NI-G70VMAXVZINSGH     PIC S9(4) COMP.                             
           10 NI-G70VMIETKAUT       PIC S9(4) COMP.                             
           10 NI-G70VPASSWORT       PIC S9(4) COMP.                             
           10 NI-G70VLETZTKTOAZ     PIC S9(4) COMP.                             
           10 NI-G70VNRAKTKTOAZ     PIC S9(4) COMP.                             
           10 NI-G70VNVERANLBE      PIC S9(4) COMP.                             
           10 NI-G70VVLPERSONAL     PIC S9(4) COMP.                             
           10 NI-G70VONLPROD        PIC S9(4) COMP.                             
           10 NI-G70VONLART         PIC S9(4) COMP.                             
           10 NI-G70VFRONTENDT      PIC S9(4) COMP.                             
           10 NI-G70VABBVERS        PIC S9(4) COMP.                             
           10 NI-G70VJAHRZULAGE     PIC S9(4) COMP.                             
           10 NI-G70VBONUSART       PIC S9(4) COMP.                             
           10 NI-G70VBONUSBEGDATUM  PIC S9(4) COMP.                             
           10 NI-G70VTUBAKTION      PIC S9(4) COMP.                             
           10 NI-G70VSONDERZAHLUNG  PIC S9(4) COMP.                             
           10 NI-G70VSONDERTILGUNG  PIC S9(4) COMP.                             
           10 NI-G70VPEAUSZAHLART   PIC S9(4) COMP.                             
        05 DCL-G70KSDSBADVERTR.                                                 
           10 G70VVERTRAGSNR       PIC X(15).                                   
           10 G70VLFDNR            PIC X(1).                                    
           10 G70VMANDANT          PIC S9(9) USAGE COMP.                        
           10 G70VAENDKZ           PIC X(2).                                    
           10 G70VPRODSCHL         PIC X(2).                                    
           10 G70VLAUFZEIT         PIC X(2).                                    
           10 G70VBEGINN           PIC X(8).                                    
           10 G70VABLAUF           PIC X(8).                                    
           10 G70VSUMME            PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G70VBEITRAG          PIC S9(5)V9(2) USAGE COMP-3.                 
           10 G70VKTOSTAKZ         PIC X(1).                                    
           10 G70VBEWERTZAHL       PIC S9(3)V9(5) USAGE COMP-3.                 
           10 G70VTARIFKZ          PIC X(1).                                    
           10 G70VZINSALLG         PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VDARLZINS         PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VDISPROZ          PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VFMINANSPAR       PIC S9(8)V9(2) USAGE COMP-3.                 
           10 G70VENDSTEULZ        PIC X(8).                                    
           10 G70VDATWOBPRAE       PIC X(8).                                    
           10 G70VDATZUTAUSSI      PIC X(8).                                    
           10 G70VDATMINANSP       PIC X(8).                                    
           10 G70VPARTINFO         PIC X(20).                                   
           10 G70VABSCHLVBNR       PIC S9(9) USAGE COMP.                        
           10 G70VMINANSPPRO       PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VZAHLW            PIC X(2).                                    
           10 G70VSPARBETRAG       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VBONUSVORJ        PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VBWZVJ            PIC S9(3)V9(5) USAGE COMP-3.                 
           10 G70VZINSLFDJ         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VWKZ              PIC X(3).                                    
           10 G70VLETZTAEND        PIC X(8).                                    
           10 G70VSCHLWOHNPRG      PIC X(2).                                    
           10 G70VBASERHOEH        PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VEHELKZ           PIC X(1).                                    
           10 G70VBLANKODARL       PIC X(1).                                    
           10 G70VERMITTKZ         PIC X(1).                                    
           10 G70VDATSPERFR        PIC X(8).                                    
           10 G70VZINSVOR          PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VDATAGEIN         PIC X(8).                                    
           10 G70VFINPROD          PIC X(2).                                    
           10 G70VDZINSFAB         PIC X(8).                                    
           10 G70VDZINSFBIS        PIC X(8).                                    
           10 G70VDARLZINOM        PIC S9(3)V9(3) USAGE COMP-3.                 
           10 G70VDARLZIEFF        PIC S9(3)V9(3) USAGE COMP-3.                 
           10 G70VANNURATE         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VLZFIN            PIC X(3).                                    
           10 G70VSONDTILJ         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VDARLHOEHE        PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VDARLSALDO        PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VDATAFBEG         PIC X(8).                                    
           10 G70VBONUSFAKT        PIC S9(2)V9(2) USAGE COMP-3.                 
           10 G70VBONUSAKT         PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VZPGEN            PIC X(2).                                    
           10 G70VMTLEINZBETR      PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VMTLEINZDAT       PIC X(8).                                    
           10 G70VJAERLEINZBETR    PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VJAERLEINZDAT     PIC X(8).                                    
           10 G70VDYNJAEREINZ      PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VMINBEWZAHL       PIC S9(4)V9(2) USAGE COMP-3.                 
           10 G70VINKASSO          PIC X(1).                                    
           10 G70VSTEULFZDAT       PIC X(8).                                    
           10 G70VSUMVLEINZ        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VWOPKZ            PIC X(1).                                    
           10 G70VWOPVJ            PIC X(4).                                    
           10 G70VWOPVVJ           PIC X(4).                                    
           10 G70VDAUZULKZ         PIC X(1).                                    
           10 G70VFOERDRELEV       PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VSALDOWFKTO       PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VSUMAVWL          PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VSUMFOERVJ        PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VVARIWECHS        PIC X(1).                                    
           10 G70VFINANZKZ         PIC X(1).                                    
           10 G70VZUTEILANW        PIC X(8).                                    
           10 G70VDARLAKTEKZ       PIC X(1).                                    
           10 G70VENDBINDFRI       PIC X(10).                                   
           10 G70VDLETZTAKT        PIC X(8).                                    
           10 G70VZIELTILG         PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VMEHRZUTEIL       PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VABTRETBLZ        PIC X(8).                                    
           10 G70VTARIFUMST        PIC S9(5)V9(2) USAGE COMP-3.                 
           10 G70VANFTILG          PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VLETZTZUTEIL      PIC X(8).                                    
           10 G70VANWZUTEIVO       PIC X(8).                                    
           10 G70VANSPWOP          PIC S9(7)V9(2) USAGE COMP-3.                 
           10 G70VSPARZULAB        PIC X(8).                                    
           10 G70VVWLKZ            PIC X(1).                                    
           10 G70VIMMOFITKZ        PIC X(1).                                    
           10 G70VWERBVERBOT       PIC X(1).                                    
           10 G70VGUTHABBEGR       PIC X(8).                                    
           10 G70VMAXVZINSGH       PIC S9(9)V9(2) USAGE COMP-3.                 
           10 G70VMIETKAUT         PIC X(1).                                    
           10 G70VPASSWORT         PIC X(1).                                    
           10 G70VLETZTKTOAZ       PIC X(8).                                    
           10 G70VNRAKTKTOAZ       PIC X(7).                                    
           10 G70VNVERANLBE        PIC X(8).                                    
           10 G70VVLPERSONAL       PIC X(1).                                    
           10 G70VONLPROD          PIC X(50).                                   
           10 G70VONLART           PIC X(50).                                   
           10 G70VFRONTENDT        PIC X(8).                                    
           10 G70VABBVERS          PIC X(7).                                    
           10 G70VJAHRZULAGE.                                                   
              49 G70VJAHRZULAGE-LEN                                             
                 PIC S9(4) USAGE COMP.                                          
              49 G70VJAHRZULAGE-TEXT                                            
                 PIC X(105).                                                    
           10 G70VBONUSART         PIC X(2).                                    
           10 G70VBONUSBEGDATUM    PIC X(8).                                    
           10 G70VTUBAKTION        PIC X(1).                                    
           10 G70VSONDERZAHLUNG    PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VSONDERTILGUNG    PIC S9(3)V9(2) USAGE COMP-3.                 
           10 G70VPEAUSZAHLART     PIC X(1).                                    
      ************************** ENDE-DCLGEN *************************          
