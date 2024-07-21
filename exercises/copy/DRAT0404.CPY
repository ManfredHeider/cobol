      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0404.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0404.CPv  $
      *
      *   Rev 2.1   Dec 21 2004 12:42:42   t13ex22
      *ASR00001: Elektronische Archivierung; ACIF-Exit zur LKZ und 
      *SIKZ Ermittlung
      *
      *   Rev 2.0   Oct 29 2004 16:26:54   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * LAST UPDATE ON 1 NOV 1993 AT 11:04:11 BY  VEND789 VERSION 01   *
       01  STRUCTURED-FIELDS    GLOBAL.                                 
      *----------------------------------------------------------------*
      * COBOL DEFINITIONS FOR STRUCTURED FIELD IDENTIFIERS AND         *
      * COMPOSED-TEXT CONTROLS. SEE DATA STREAM & OBJECT ARCHITECTURES *
      * MODCA REFERENCE, SC31-6802-02.                                 *
      * THESE DEFINITIONS ARE COMPATIBLE WITH VS COBOL II, RELEASE 3   *
      * OR LATER ONLY.                                                 *
      * * LAST UPDATE ON 26 AUG 1991 AT 11:32:27 BY XPTTT48 VERSION 03**
      * ADDED IDENTIFIERS FOR COMMON TRIPLETS                          *
      * * LAST UPDATE ON  5 MAR 1993 AT 16:38:03 BY VEND789 VERSION 04**
      * INDEXING STRUCTURED FIELDS AND TRIPLETS.                       *
      *----------------------------------------------------------------*
         03  SF-NAMES.                                                  
      * SYMBOLS FOR STRUCTURED FIELD IDENTIFIERS:                       
           05  SF-BAG          PIC XXX  VALUE X'D3A8C9'.                
           05  SFN-BAG         PIC XXX  VALUE 'BAG'.                    
      *               BEGIN ACTIVE ENVIRON .                            
           05  SF-BBC          PIC XXX  VALUE X'D3A8EB'.                
           05  SFN-BBC         PIC XXX  VALUE 'BBC'.                    
      *               BEGIN BAR CODE OBJECT.                            
           05  SF-BCF          PIC XXX  VALUE X'D3A88A'.                
           05  SFN-BCF         PIC XXX  VALUE 'BCF'.                    
      *               BEGIN CODED FONT .                                
           05  SF-BCP          PIC XXX  VALUE X'D3A887'.                
           05  SFN-BCP         PIC XXX  VALUE 'BCP'.                    
      *               BEGIN CODE PAGE .                                 
           05  SF-BCT          PIC XXX  VALUE X'D3A89B'.                
           05  SFN-BCT         PIC XXX  VALUE 'BCT'.                    
      *               BEGIN COMPOSED-TEXT .                             
           05  SF-BDA          PIC XXX  VALUE X'D3EEEB'.                
           05  SFN-BDA         PIC XXX  VALUE 'BDA'.                    
      *               BAR CODE DATA.                                    
           05  SF-BDD          PIC XXX  VALUE X'D3A6EB'.                
           05  SFN-BDD         PIC XXX  VALUE 'BDD'.                    
      *               BAR CODE DATA DESCRIPTOR.                         
           05  SF-BDG          PIC XXX  VALUE X'D3A8C4'.                
           05  SFN-BDG         PIC XXX  VALUE 'BDG'.                    
      *               BEGIN DOC ENVIRON GROUP .                         
           05  SF-BDI          PIC XXX  VALUE X'D3A8A7'.                
           05  SFN-BDI         PIC XXX  VALUE 'BDI'.                    
      *               BEGIN DOC INDEX.                                  
           05  SF-BDM          PIC XXX  VALUE X'D3A8CA'.                
           05  SFN-BDM         PIC XXX  VALUE 'BDM'.                    
      *               BEGIN DATA MAP .                                  
           05  SF-BDT          PIC XXX  VALUE X'D3A8A8'.                
           05  SFN-BDT         PIC XXX  VALUE 'BDT'.                    
      *               BEGIN DOCUMENT .                                  
           05  SF-BDX          PIC XXX  VALUE X'D3A8E3'.                
           05  SFN-BDX         PIC XXX  VALUE 'BDX'.                    
      *               BEGIN DATA MAP XMIT SUBCASE .                     
           05  SF-BFG          PIC XXX  VALUE X'D3A8C5'.                
           05  SFN-BFG         PIC XXX  VALUE 'BFG'.                    
      *               BEGIN FORM ENVIRONMENT GRP .                      
           05  SF-BFM          PIC XXX  VALUE X'D3A8CD'.                
           05  SFN-BFM         PIC XXX  VALUE 'BFM'.                    
      *               BEGIN FORM MAP        .                           
           05  SF-BFN          PIC XXX  VALUE X'D3A889'.                
           05  SFN-BFN         PIC XXX  VALUE 'BFN'.                    
      *               BEGIN FONT            .                           
           05  SF-BGR          PIC XXX  VALUE X'D3A8BB'.                
           05  SFN-BGR         PIC XXX  VALUE 'BGR'.                    
      *               BEGIN GRAPHICS OBJECT.                            
           05  SF-BIM          PIC XXX  VALUE X'D3A87B'.                
           05  SFN-BIM         PIC XXX  VALUE 'BIM'.                    
      *               BEGIN IMAGE OBJECT IM.                            
           05  SF-BIO          PIC XXX  VALUE X'D3A8FB'.                
           05  SFN-BIO         PIC XXX  VALUE 'BI0'.                    
      *               BEGIN IMAGE OBJECT IO.                            
           05  SF-BMM          PIC XXX  VALUE X'D3A8CC'.                
           05  SFN-BMM         PIC XXX  VALUE 'BMM'.                    
      *               BEGIN MEDIUM MAP .                                
           05  SF-BMO          PIC XXX  VALUE X'D3A8DF'.                
           05  SFN-BMO         PIC XXX  VALUE 'BMO'.                    
      *               BEGIN MEDIUM OVERLAY .                            
           05  SF-BNG          PIC XXX  VALUE X'D3A8AD'.                
           05  SFN-BNG         PIC XXX  VALUE 'BNG'.                    
      *               BEGIN NAMED GROUP.                                
           05  SF-BOG          PIC XXX  VALUE X'D3A8C7'.                
           05  SFN-BOG         PIC XXX  VALUE 'BOG'.                    
      *               BEGIN OBJECT ENVIRONMENT GROUP.                   
           05  SF-BPG          PIC XXX  VALUE X'D3A8AF'.                
           05  SFN-BPG         PIC XXX  VALUE 'BPG'.                    
      *               BEGIN PAGE     .                                  
           05  SF-BPM          PIC XXX  VALUE X'D3A8CB'.                
           05  SFN-BPM         PIC XXX  VALUE 'BPM'.                    
      *               BEGIN PAGE MAP .                                  
           05  SF-BPS          PIC XXX  VALUE X'D3A85F'.                
           05  SFN-BPS         PIC XXX  VALUE 'BPS'.                    
      *               BEGIN PAGE SEGMENT .                              
           05  SF-BPT          PIC XXX  VALUE X'D3A89B'.                
           05  SFN-BPT         PIC XXX  VALUE 'BPT'.                    
      *               BEGIN PRESENTATION TEXT .                         
           05  SF-BR           PIC XXX  VALUE X'D3A8CE'.                
           05  SFN-BR          PIC XXX  VALUE 'BR '.                    
      *               BEGIN RESOURCE .                                  
           05  SF-BRG          PIC XXX  VALUE X'D3A8C6'.                
           05  SFN-BRG         PIC XXX  VALUE 'BRG'.                    
      *               BEGIN RESOURCE GROUP .                            
           05  SF-CCP          PIC XXX  VALUE X'D3A7CA'.                
           05  SFN-CCP         PIC XXX  VALUE 'CCP'.                    
      *               CONDITIONAL PROC CNT .                            
           05  SF-CFC          PIC XXX  VALUE X'D3A78A'.                
           05  SFN-CFC         PIC XXX  VALUE 'CFC'.                    
      *               CODED FONT CONTROL .                              
           05  SF-CFI          PIC XXX  VALUE X'D38C8A'.                
           05  SFN-CFI         PIC XXX  VALUE 'CFI'.                    
      *               CODED FONT INDEX .                                
           05  SF-CPC          PIC XXX  VALUE X'D3A787'.                
           05  SFN-CPC         PIC XXX  VALUE 'CPC'.                    
      *               CODED PAGE CONTROL .                              
           05  SF-CPD          PIC XXX  VALUE X'D3A687'.                
           05  SFN-CPD         PIC XXX  VALUE 'CPD'.                    
      *               CODED PAGE DESCRIPTO .                            
           05  SF-CPI          PIC XXX  VALUE X'D3AC87'.                
           05  SFN-CPI         PIC XXX  VALUE 'CPI'.                    
      *               CODED PAGE INDEX .                                
           05  SF-CTC          PIC XXX  VALUE X'D3A79B'.                
           05  SFN-CTC         PIC XXX  VALUE 'CTC'.                    
      *               COMPOSED TEXT CONTRO .                            
           05  SF-CTD          PIC XXX  VALUE X'D3A69B'.                
           05  SFN-CTD         PIC XXX  VALUE 'CTD'.                    
      *               COMPOSED TEXT DESCR. .                            
           05  SF-CTX          PIC XXX  VALUE X'D3EE9B'.                
           05  SFN-CTX         PIC XXX  VALUE 'CTX'.                    
      *               COMPOSED TEXT DATA .                              
           05  SF-DXD          PIC XXX  VALUE X'D3A6E3'.                
           05  SFN-DXD         PIC XXX  VALUE 'DXD'.                    
      *               DATA MAP SUBCASE DES .                            
           05  SF-EAG          PIC XXX  VALUE X'D3A9C9'.                
           05  SFN-EAG         PIC XXX  VALUE 'EAG'.                    
      *               END ACTIVE ENVIR GRP .                            
           05  SF-EBC          PIC XXX  VALUE X'D3A9EB'.                
           05  SFN-EBC         PIC XXX  VALUE 'EBC'.                    
      *               END BAR CODE OBJECT.                              
           05  SF-ECF          PIC XXX  VALUE X'D3A98A'.                
           05  SFN-ECF         PIC XXX  VALUE 'ECF'.                    
      *               END CODED FONT .                                  
           05  SF-ECP          PIC XXX  VALUE X'D3A987'.                
           05  SFN-ECP         PIC XXX  VALUE 'ECP'.                    
      *               END CODED PAGE .                                  
           05  SF-ECT          PIC XXX  VALUE X'D3A99B'.                
           05  SFN-ECT         PIC XXX  VALUE 'ECT'.                    
      *               END COMPOSED TEXT BL .                            
           05  SF-EDG          PIC XXX  VALUE X'D3A9C4'.                
           05  SFN-EDG         PIC XXX  VALUE 'EDG'.                    
      *               END DOC ENVIRN GROUP .                            
           05  SF-EDI          PIC XXX  VALUE X'D3A9A7'.                
           05  SFN-EDI         PIC XXX  VALUE 'EDI'.                    
      *               END DOC INDEX.                                    
           05  SF-EDM          PIC XXX  VALUE X'D3A9CA'.                
           05  SFN-EDM         PIC XXX  VALUE 'EDM'.                    
      *               END DATA MAP   .                                  
           05  SF-EDT          PIC XXX  VALUE X'D3A9A8'.                
           05  SFN-EDT         PIC XXX  VALUE 'EDT'.                    
      *               END OF DOCUMENT .                                 
           05  SF-EDX          PIC XXX  VALUE X'D3A9E3'.                
           05  SFN-EDX         PIC XXX  VALUE 'EDX'.                    
      *               END DATA MAP XMIT SU .                            
           05  SF-EFG          PIC XXX  VALUE X'D3A9C5'.                
           05  SFN-EFG         PIC XXX  VALUE 'EFG'.                    
      *               END FORM ENVIRON GRP .                            
           05  SF-EFM          PIC XXX  VALUE X'D3A9CD'.                
           05  SFN-EFM         PIC XXX  VALUE 'EFM'.                    
      *               END FORM MAP   .                                  
           05  SF-EFN          PIC XXX  VALUE X'D3A989'.                
           05  SFN-EFN         PIC XXX  VALUE 'EFN'.                    
      *               END FONT       .                                  
           05  SF-EGR          PIC XXX  VALUE X'D3A9BB'.                
           05  SFN-EGR         PIC XXX  VALUE 'EGR'.                    
      *               END GRAPHICS OBJECT.                              
           05  SF-EIM          PIC XXX  VALUE X'D3A97B'.                
           05  SFN-EIM         PIC XXX  VALUE 'EIM'.                    
      *               END IMAGE BLOCK IM.                               
           05  SF-EIO          PIC XXX  VALUE X'D3A9FB'.                
           05  SFN-EIO         PIC XXX  VALUE 'EIO'.                    
      *               END IMAGE BLOCK IO.                               
           05  SF-EMM          PIC XXX  VALUE X'D3A9CC'.                
           05  SFN-EMM         PIC XXX  VALUE 'EMM'.                    
      *               END MEDIUM MAP .                                  
           05  SF-EMO          PIC XXX  VALUE X'D3A9DF'.                
           05  SFN-EMO         PIC XXX  VALUE 'EMO'.                    
      *               END MEDIUM OVERLAY .                              
           05  SF-ENG          PIC XXX  VALUE X'D3A9AD'.                
           05  SFN-ENG         PIC XXX  VALUE 'ENG'.                    
      *               END NAMED GROUP.                                  
           05  SF-EOG          PIC XXX  VALUE X'D3A9C7'.                
           05  SFN-EOG         PIC XXX  VALUE 'EOG'.                    
      *               END OBJECT ENVIRONMENT GROUP.                     
           05  SF-EPG          PIC XXX  VALUE X'D3A9AF'.                
           05  SFN-EPG         PIC XXX  VALUE 'EPG'.                    
      *               END PAGE       .                                  
           05  SF-EPM          PIC XXX  VALUE X'D3A9CB'.                
           05  SFN-EPM         PIC XXX  VALUE 'EPM'.                    
      *               END PAGE MAP   .                                  
           05  SF-EPS          PIC XXX  VALUE X'D3A95F'.                
           05  SFN-EPS         PIC XXX  VALUE 'EPS'.                    
      *               END PAGE SEGMENT .                                
           05  SF-EPT          PIC XXX  VALUE X'D3A99B'.                
           05  SFN-EPT         PIC XXX  VALUE 'EPT'.                    
      *               END PRESENTATION TEXT .                           
           05  SF-ER           PIC XXX  VALUE X'D3A9CE'.                
           05  SFN-ER          PIC XXX  VALUE 'ER '.                    
      *               END RESOURCE   .                                  
           05  SF-ERG          PIC XXX  VALUE X'D3A9C6'.                
           05  SFN-ERG         PIC XXX  VALUE 'ERG'.                    
      *               END RESOURCE GROUP .                              
           05  SF-FDS          PIC XXX  VALUE X'D3AAEC'.                
           05  SFN-FDS         PIC XXX  VALUE 'FDS'.                    
      *               FIXED DATA SIZE .                                 
           05  SF-FDX          PIC XXX  VALUE X'D3EEEC'.                
           05  SFN-FDX         PIC XXX  VALUE 'FDX'.                    
      *               FIXED DATA TEXT .                                 
           05  SF-FGD          PIC XXX  VALUE X'D3A6C5'.                
           05  SFN-FGD         PIC XXX  VALUE 'FGD'.                    
      *               FORM ENVIRN GRP DESC .                            
           05  SF-FNC          PIC XXX  VALUE X'D3A789'.                
           05  SFN-FNC         PIC XXX  VALUE 'FNC'.                    
      *               FONT CONTROL   .                                  
           05  SF-FND          PIC XXX  VALUE X'D3A689'.                
           05  SFN-FND         PIC XXX  VALUE 'FND'.                    
      *               FONT DESCRIPTOR .                                 
           05  SF-FNG          PIC XXX  VALUE X'D3EE89'.                
           05  SFN-FNG         PIC XXX  VALUE 'FNG'.                    
      *               FONT PATTERNS  .                                  
           05  SF-FNI          PIC XXX  VALUE X'D38C89'.                
           05  SFN-FNI         PIC XXX  VALUE 'FNI'.                    
      *               FONT INDEX     .                                  
           05  SF-FNM          PIC XXX  VALUE X'D3A289'.                
           05  SFN-FNM         PIC XXX  VALUE 'FNM'.                    
      *               FONT PATTERNS MAP .                               
           05  SF-FNO          PIC XXX  VALUE X'D3AE89'.                
           05  SFN-FNO         PIC XXX  VALUE 'FNO'.                    
      *               FONT ORIENTATION .                                
           05  SF-FNP          PIC XXX  VALUE X'D3AC89'.                
           05  SFN-FNP         PIC XXX  VALUE 'FNP'.                    
      *               FONT POSITION  .                                  
           05  SF-GAD          PIC XXX  VALUE X'D3EEBB'.                
           05  SFN-GAD         PIC XXX  VALUE 'GAD'.                    
      *               GRAPHICS DATA.                                    
           05  SF-GDD          PIC XXX  VALUE X'D3A6BB'.                
           05  SFN-GDD         PIC XXX  VALUE 'GDD'.                    
      *               GRAPHICS DATA DESCRIPTOR.                         
           05  SF-ICP          PIC XXX  VALUE X'D3AC7B'.                
           05  SFN-ICP         PIC XXX  VALUE 'ICP'.                    
      *               IMAGE CELL POSITION .                             
           05  SF-IDD          PIC XXX  VALUE X'D3A6FB'.                
           05  SFN-IDD         PIC XXX  VALUE 'IDD'.                    
      *               IMAGE DATA DESCRIPTOR IO.                         
           05  SF-IDM          PIC XXX  VALUE X'D3ABCA'.                
           05  SFN-IDM         PIC XXX  VALUE 'IDM'.                    
      *               INVOKE DATA MAP .                                 
           05  SF-IEL          PIC XXX  VALUE X'D3B2A7'.                
           05  SFN-IEL         PIC XXX  VALUE 'IEL'.                    
      *               INDEX ELEMENT.                                    
           05  SF-IID          PIC XXX  VALUE X'D3A67B'.                
           05  SFN-IID         PIC XXX  VALUE 'IID'.                    
      *               IMAGE INPUT DESCRIPTOR IM.                        
           05  SF-IMM          PIC XXX  VALUE X'D3ABCC'.                
           05  SFN-IMM         PIC XXX  VALUE 'IMM'.                    
      *               IMAGE MEDIUM MAP .                                
           05  SF-IOC          PIC XXX  VALUE X'D3A77B'.                
           05  SFN-IOC         PIC XXX  VALUE 'IOC'.                    
      *               IMAGE OUTPUT CONTROL .                            
           05  SF-IPD          PIC XXX  VALUE X'D3EEFB'.                
           05  SFN-IPD         PIC XXX  VALUE 'IPD'.                    
      *               IMAGE PICTURE DATA IO.                            
           05  SF-IPO          PIC XXX  VALUE X'D3AFD8'.                
           05  SFN-IPO         PIC XXX  VALUE 'IPO'.                    
      *               INCLUDE PAGE OVERLAY .                            
           05  SF-IPS          PIC XXX  VALUE X'D3AF5F'.                
           05  SFN-IPS         PIC XXX  VALUE 'IPS'.                    
      *               INCLUDE PAGE SEGMENT .                            
           05  SF-IRD          PIC XXX  VALUE X'D3EE7B'.                
           05  SFN-IRD         PIC XXX  VALUE 'IRD'.                    
      *               IMAGE RASTER DATA .                               
           05  SF-LNC          PIC XXX  VALUE X'D3AAE7'.                
           05  SFN-LNC         PIC XXX  VALUE 'LNC'.                    
      *               LINE DESCRIPTOR COUN .                            
           05  SF-LND          PIC XXX  VALUE X'D3A6E7'.                
           05  SFN-LND         PIC XXX  VALUE 'LND'.                    
      *               LINE DESCRIPTOR .                                 
           05  SF-MBC          PIC XXX  VALUE X'D3ABEB'.                
           05  SFN-MBC         PIC XXX  VALUE 'MBC'.                    
      *               MAP BAR CODE.                                     
           05  SF-MCC          PIC XXX  VALUE X'D3A288'.                
           05  SFN-MCC         PIC XXX  VALUE 'MCC'.                    
      *               MEDIUM COPY COUNT .                               
           05  SF-MCF          PIC XXX  VALUE X'D3B18A'.                
           05  SFN-MCF         PIC XXX  VALUE 'MCF'.                    
      *               MAP CODED FONT (FORMAT 1).                        
           05  SF-MCF2         PIC XXX  VALUE X'D3AB8A'.                
           05  SFN-MCF2        PIC XXX  VALUE 'MCF'.                    
      *               MAP CODED FONT (FORMAT 2).                        
           05  SF-MDD          PIC XXX  VALUE X'D3A688'.                
           05  SFN-MDD         PIC XXX  VALUE 'MDD'.                    
      *               MEDIUM DESCRIPTOR .                               
           05  SF-MGO          PIC XXX  VALUE X'D3ABBB'.                
           05  SFN-MGO         PIC XXX  VALUE 'MGO'.                    
      *               MAP GRAPHICS OBJECT.                              
           05  SF-MIO          PIC XXX  VALUE X'D3ABFB'.                
           05  SFN-MIO         PIC XXX  VALUE 'MIO'.                    
      *               MAP IO IMAGE OBJECT.                              
           05  SF-MMC          PIC XXX  VALUE X'D3A788'.                
           05  SFN-MMC         PIC XXX  VALUE 'MMC'.                    
      *               MEDIUM MODIFICATION .                             
           05  SF-MMO          PIC XXX  VALUE X'D3B1DF'.                
           05  SFN-MMO         PIC XXX  VALUE 'MMO'.                    
      *               MAP MEDIUM OVERLAY .                              
           05  SF-MPO          PIC XXX  VALUE X'D3ABD8'.                
           05  SFN-MPO         PIC XXX  VALUE 'MPO'.                    
      *               MAP PAGE OVERLAY .                                
           05  SF-MPS          PIC XXX  VALUE X'D3B15F'.                
           05  SFN-MPS         PIC XXX  VALUE 'MPS'.                    
      *               MAP PAGE SEGMENT .                                
           05  SF-MSU          PIC XXX  VALUE X'D3ABEA'.                
           05  SFN-MSU         PIC XXX  VALUE 'MSU'.                    
      *               MAP SUPPRESSION .                                 
           05  SF-NOP          PIC XXX  VALUE X'D3EEEE'.                
           05  SFN-NOP         PIC XXX  VALUE 'NOP'.                    
      *               NO OPERATION   .                                  
           05  SF-OBD          PIC XXX  VALUE X'D3A66B'.                
           05  SFN-OBD         PIC XXX  VALUE 'OBD'.                    
      *               OBJECT AREA DESCRIPTOR.                           
           05  SF-OBP          PIC XXX  VALUE X'D3AC6B'.                
           05  SFN-OBP         PIC XXX  VALUE 'OBP'.                    
      *               OBJECT AREA POSITION.                             
           05  SF-PGD          PIC XXX  VALUE X'D3A6AF'.                
           05  SFN-PGD         PIC XXX  VALUE 'PGD'.                    
      *               PAGE DESCRIPTOR .                                 
           05  SF-PGP          PIC XXX  VALUE X'D3ACAF'.                
           05  SFN-PGP         PIC XXX  VALUE 'PGP'.                    
      *               PAGE POSITION  .                                  
           05  SF-PGP2         PIC XXX  VALUE X'D3B1AF'.                
           05  SFN-PGP2        PIC XXX  VALUE 'PGP'.                    
      *               PAGE POSITION (FORMAT 2).                         
           05  SF-PTD          PIC XXX  VALUE X'D3A69B'.                
           05  SFN-PTD         PIC XXX  VALUE 'PTD'.                    
      *               PRESENTATION TEXT DESCRIPTOR (FORMAT 1).          
           05  SF-PTD2         PIC XXX  VALUE X'D3B19B'.                
           05  SFN-PTD2        PIC XXX  VALUE 'PTD'.                    
      *               PRESENTATION TEXT DESCRIPTOR (FORMAT 2).          
           05  SF-PTX          PIC XXX  VALUE X'D3EE9B'.                
           05  SFN-PTX         PIC XXX  VALUE 'PTX'.                    
      *               PRESENTATION TEXT DESCRIPTOR (FORMAT 2).          
           05  SF-TLE          PIC XXX  VALUE X'D3A090'.                
           05  SFN-TLE         PIC XXX  VALUE 'TLE'.                    
      *               TAG LOGICAL ELEMENT.                              
         03  SF-TABLE          REDEFINES SF-NAMES,                      
                               OCCURS 111,                              
                               ASCENDING KEY SF-ID,                     
                               INDEXED BY SF-ID-INDX.                   
           05  SF-VALUE        PIC XXX.                                 
           05  SF-ID           PIC XXX.                                 
      *                                                                 
      *               PRESENTATION TEXT DATA .                          
      *--------------------------------------------------------------*  
      * COMPOSED TEXT CONTROL SEQUENCES.                             *  
      * TEXT CONTROLS ARE DESCRIBED AS FOLLOWS:                      *  
      * BYTE 1: LENGTH. ZERO REPRESENTS A VARIABLE LENGTH CODE.      *  
      * BYTE 2: TEXT-CONTROL CODE, UNCHAINED. (EVEN VALUES)          *  
      *         CHAINED CONTROL CODES ARE REPRESENTED BY THE HIGH-OR *  
      *         BIT TURNED ON (ODD VALUES).                          *  
      *--------------------------------------------------------------*  
         03  TC-NAMES.                                                  
           05  TC-AMB          PIC XX   VALUE X'04D2'.                  
           05  TC-AMB-C        PIC XX   VALUE X'04D3'.                  
           05  TCN-AMB         PIC XXX  VALUE 'AMB'.                    
      *               ABSOLUTE MOVE BASELINE.                           
           05  TC-AMI          PIC XX   VALUE X'04C6'.                  
           05  TC-AMI-C        PIC XX   VALUE X'04C7'.                  
           05  TCN-AMI         PIC XXX  VALUE 'AMI'.                    
      *               ABSOLUTE MOVE INLINE .                            
           05  TC-BLN          PIC XX   VALUE X'02D8'.                  
           05  TC-BLN-C        PIC XX   VALUE X'02D9'.                  
           05  TCN-BLN         PIC XXX  VALUE 'BLN'.                    
      *               BEGIN LINE     .                                  
           05  TC-BSU          PIC XX   VALUE X'03F2'.                  
           05  TC-BSU-C        PIC XX   VALUE X'03F3'.                  
           05  TCN-BSU         PIC XXX  VALUE 'BSU'.                    
      *               BEGIN SUPPRESSION .                               
           05  TC-CCTL         PIC XX  VALUE X'0001'.                   
           05  TC-CCTL-C       PIC XX  VALUE X'0001'.                   
           05  TCN-CCT         PIC XXX  VALUE 'CCT'.                    
      *               CHAINED CONTROL FLAG .                            
           05  TC-DBR          PIC XX   VALUE X'07E6'.                  
           05  TC-DBR-C        PIC XX   VALUE X'07E7'.                  
           05  TCN-DBR         PIC XXX  VALUE 'DBR'.                    
      *               DRAW BASELINE RULE .                              
           05  TC-DIR          PIC XX   VALUE X'07E4'.                  
           05  TC-DIR-C        PIC XX   VALUE X'07E5'.                  
           05  TCN-DIR         PIC XXX  VALUE 'DIR'.                    
      *               DRAW INLINE RULE .                                
           05  TC-ESC          PIC XX   VALUE X'2BD3'.                  
           05  TC-ESC-C        PIC XX   VALUE X'2BD3'.                  
           05  TCN-ESC         PIC XXX  VALUE 'ESC'.                    
      *               ESCAPE SEQUENCE .                                 
           05  TC-ESU          PIC XX   VALUE X'03F4'.                  
           05  TC-ESU-C        PIC XX   VALUE X'03F5'.                  
           05  TCN-ESU         PIC XXX  VALUE 'ESU'.                    
      *               END SUPPRESSION .                                 
           05  TC-NOP          PIC XX   VALUE X'00F8'.                  
           05  TC-NOP-C        PIC XX   VALUE X'00F9'.                  
           05  TCN-NOP         PIC XXX  VALUE 'NOP'.                    
      *               NO OPERATION   .                                  
           05  TC-OVS          PIC XX   VALUE X'0572'.                  
           05  TC-OVS-C        PIC XX   VALUE X'0573'.                  
           05  TCN-OVS         PIC XXX  VALUE 'OVS'.                    
      *               OVERSTRIKE.                                       
           05  TC-RMB          PIC XX   VALUE X'04D4'.                  
           05  TC-RMB-C        PIC XX   VALUE X'04D5'.                  
           05  TCN-RMB         PIC XXX  VALUE 'RMB'.                    
      *               RELATIVE MOVE BASELINE.                           
           05  TC-RMI          PIC XX   VALUE X'04C8'.                  
           05  TC-RMI-C        PIC XX   VALUE X'04C9'.                  
           05  TCN-RMI         PIC XXX  VALUE 'RMI'.                    
      *               RELATIVE MOVE INLINE .                            
           05  TC-RPS          PIC XX   VALUE X'00EE'.                  
           05  TC-RPS-C        PIC XX   VALUE X'00EF'.                  
           05  TCN-RPS         PIC XXX  VALUE 'RPS'.                    
      *               REPEAT STRING  .                                  
           05  TC-SBI          PIC XX   VALUE X'04D0'.                  
           05  TC-SBI-C        PIC XX   VALUE X'04D1'.                  
           05  TCN-SBI         PIC XXX  VALUE 'SBI'.                    
      *               SET BASELINE INCREMENT.                           
           05  TC-SCFL          PIC XX  VALUE X'03F0'.                  
           05  TC-SCFL-C        PIC XX  VALUE X'03F1'.                  
           05  TCN-SCFL         PIC XXX VALUE 'SCF'.                    
      *               SET CODED FONT LOCAL .                            
           05  TC-SIA          PIC XX   VALUE X'04C2'.                  
           05  TC-SIA-C        PIC XX   VALUE X'04C3'.                  
           05  TCN-SIA         PIC XXX  VALUE 'SIA'.                    
      *               SET INTERCHARACTER ADJUSTMENT.                    
           05  TC-SIM          PIC XX   VALUE X'04C0'.                  
           05  TC-SIM-C        PIC XX   VALUE X'04C1'.                  
           05  TCN-SIM         PIC XXX  VALUE 'SIM'.                    
      *               SET INLINE MARGIN .                               
           05  TC-STC          PIC XX   VALUE X'05F6'.                  
           05  TC-STC-C        PIC XX   VALUE X'05F7'.                  
           05  TCN-STC         PIC XXX  VALUE 'STC'.                    
      *               SET TEXT COLOR.                                   
           05  TC-STO          PIC XX   VALUE X'06F6'.                  
           05  TC-STO-C        PIC XX   VALUE X'06F7'.                  
           05  TCN-STO         PIC XXX  VALUE 'STO'.                    
      *               SET TEXT ORIENTATION .                            
           05  TC-SVI          PIC XX   VALUE X'04C4'.                  
           05  TC-SVI-C        PIC XX   VALUE X'04C5'.                  
           05  TCN-SVI         PIC XXX  VALUE 'SVI'.                    
      *               SET VAR SPC CHAR INCREMENT.                       
           05  TC-TRN          PIC XX   VALUE X'00DA'.                  
           05  TC-TRN-C        PIC XX   VALUE X'00DB'.                  
           05  TCN-TRN         PIC XXX  VALUE 'TRN'.                    
      *               TRANSPARENT DATA .                                
           05  TC-USC          PIC XX   VALUE X'0376'.                  
           05  TC-USC-C        PIC XX   VALUE X'0377'.                  
           05  TCN-USC         PIC XXX  VALUE 'USC'.                    
      *               UNDERSCORE.                                       
         03  TC-TABLE          REDEFINES TC-NAMES                       
                               OCCURS 23,                               
                               ASCENDING KEY TC-ID,                     
                               INDEXED BY TC-ID-INDX.                   
           05  TC-LENGTH       PIC X.                                   
           05  TC-VALUE        PIC X.                                   
           05  TC-LENGTH-C     PIC X.                                   
           05  TC-VALUE-C      PIC X.                                   
           05  TC-ID           PIC XXX.                                 
      *                                                                 
      *               GRAPHICS DRAWING ORDERS.                          
      *--------------------------------------------------------------*  
      * GOCA GRAPHICS DRAWING ORDERS.                                *  
      * NAMING FOLLOWS CONVENTIONS IN GOCA REFERENCE, SC31-6804.     *  
      * ORDERS ARE FOR DR2 SUBSET, VERSION 0.                        *  
      * BYTE 1: DRAWING ORDER.                                       *  
      * BYTE 2: ORDER LENGTH: 0=VARIABLE LENGTH,                     *  
      *         X'FF'=1 OR 2 BYTE ORDER,                             *  
      *         OTHER VALUES REPRESENT FIXED LENGTH.                 *  
      *--------------------------------------------------------------*  
         03  GR-NAMES.                                                  
           05  GRBSI           PIC XX   VALUE X'700C'.                  
           05  GRN-GRBSI       PIC X(6) VALUE 'GRBSI'.                  
      *               BEGIN SEGMENT INTRODUCER.                         
           05  GBAR            PIC XX   VALUE X'68FF'.                  
           05  GRN-GBAR        PIC X(6) VALUE 'GBAR '.                  
      *               BEGIN AREA.                                       
           05  GBIMG           PIC XX   VALUE X'D10A'.                  
           05  GRN-GBIMG       PIC X(6) VALUE 'GBIMG'.                  
      *               BEGIN IMAGE.                                      
           05  GBCIMG          PIC XX   VALUE X'9106'.                  
           05  GRN-GBCIMG      PIC X(6) VALUE 'GCBIMG'.                 
      *               BEGIN IMAGE AT CURRENT POSITION.                  
           05  GCHST           PIC XX   VALUE X'C300'.                  
           05  GRN-GCHST       PIC X(6) VALUE 'GCHST '.                 
      *               CHARACTER STRING.                                 
           05  GCCHST          PIC XX   VALUE X'8300'.                  
           05  GRN-GCCHST      PIC X(6) VALUE 'GCCHST'.                 
      *               CHARACTER STRING AT CURRENT POSITION.             
           05  GCOMT           PIC XX   VALUE X'0100'.                  
           05  GRN-GCOMT       PIC X(6) VALUE 'GCOMT '.                 
      *               COMMENT.                                          
           05  GEAR            PIC XX   VALUE X'6000'.                  
           05  GRN-GEAR        PIC X(6) VALUE 'GEAR  '.                 
      *               END AREA.                                         
           05  GEIMG           PIC XX   VALUE X'9300'.                  
           05  GRN-GEIMG       PIC X(6) VALUE 'GEIMG '.                 
      *               END IMAGE.                                        
           05  GEPROL          PIC XX   VALUE X'3EFF'.                  
           05  GRN-GEPROL      PIC X(6) VALUE 'GEPROL'.                 
      *               END PROLOG.                                       
           05  GRESI           PIC XX   VALUE X'71FF'.                  
           05  GRN-GRESI       PIC X(6) VALUE 'GRESI '.                 
      *               END SEGMENT.                                      
           05  GFLT            PIC XX   VALUE X'C500'.                  
           05  GRN-GFLT        PIC X(6) VALUE 'GFLT  '.                 
      *               FILLET.                                           
           05  GCFLT           PIC XX   VALUE X'8500'.                  
           05  GRN-GCFLT       PIC X(6) VALUE 'GCFLT '.                 
      *               FILLET AT CURRENT POSITION.                       
           05  GFARC           PIC XX   VALUE X'C706'.                  
           05  GRN-GFARC       PIC X(6) VALUE 'GFARC '.                 
      *               FULL ARC.                                         
           05  GCFLARC         PIC XX   VALUE X'8702'.                  
           05  GRN-GCFARC      PIC X(6) VALUE 'GCFARC'.                 
      *               FILLET AT CURRENT POSITION.                       
           05  GIMD            PIC XX   VALUE X'9200'.                  
           05  GRN-GIMD        PIC X(6) VALUE 'GIMD  '.                 
      *               IMAGE DATA.                                       
           05  GLINE           PIC XX   VALUE X'C100'.                  
           05  GRN-GLINE       PIC X(6) VALUE 'GLINE '.                 
      *               LINE.                                             
           05  GCLINE          PIC XX   VALUE X'8100'.                  
           05  GRN-GCLINE      PIC X(6) VALUE 'GCLINE'.                 
      *               LINE AT CURRENT POSITION.                         
           05  GMRK            PIC XX   VALUE X'C200'.                  
           05  GRN-GMRK        PIC X(6) VALUE 'GMRK  '.                 
      *               MARKER.                                           
           05  GCMRK           PIC XX   VALUE X'8200'.                  
           05  GRN-GCMRK       PIC X(6) VALUE 'GCMRK '.                 
      *               MARKER AT CURRENT POSITION.                       
           05  GNOP1           PIC XX   VALUE X'00FF'.                  
           05  GRN-GNOP1       PIC X(6) VALUE 'GNOP1 '.                 
      *               NO OPERATION.                                     
           05  GRLINE          PIC XX   VALUE X'E100'.                  
           05  GRN-GRLINE      PIC X(6) VALUE 'GRLINE'.                 
      *               RELATIVE LINE.                                    
           05  GCRLINE         PIC XX   VALUE X'A100'.                  
           05  GRN-GCRLINE     PIC X(6) VALUE 'GCRLIN'.                 
      *               RELATIVE LINE AT CURRENT POSITION.                
           05  GSGCH           PIC XX   VALUE X'0400'.                  
           05  GRN-GSGCH       PIC X(6) VALUE 'GSGCH '.                 
      *               SEGMENT CHARACTERISTICS.                          
           05  GSAP            PIC XX   VALUE X'2208'.                  
           05  GRN-GSAP        PIC X(6) VALUE 'GSAP  '.                 
      *               SET ARC PARAMETERS.                               
           05  GSBMX           PIC XX   VALUE X'0DFF'.                  
           05  GRN-GSBMX       PIC X(6) VALUE 'GSBMX '.                 
      *               SET BACKGROUND MIX.                               
           05  GSCA            PIC XX   VALUE X'3404'.                  
           05  GRN-GSCA        PIC X(6) VALUE 'GSCA  '.                 
      *               SET CHARACTER ANGLE.                              
           05  GSCC            PIC XX   VALUE X'3304'.                  
           05  GRN-GSCC        PIC X(6) VALUE 'GSCC  '.                 
      *               SET CHARACTER CELL.                               
           05  GSCD            PIC XX   VALUE X'3AFF'.                  
           05  GRN-GSCD        PIC X(6) VALUE 'GSCD  '.                 
      *               SET CHARACTER DIRECTION.                          
           05  GSCR            PIC XX   VALUE X'39FF'.                  
           05  GRN-GSCR        PIC X(6) VALUE 'GSCR  '.                 
      *               SET CHARACTER PRECISION.                          
           05  GSCS            PIC XX   VALUE X'38FF'.                  
           05  GRN-GSCS        PIC X(6) VALUE 'GSCS  '.                 
      *               SET CHARACTER SET.                                
           05  GSCH            PIC XX   VALUE X'3500'.                  
           05  GRN-GSCH        PIC X(6) VALUE 'GSCH  '.                 
      *               SET CHARACTER SHEAR.                              
           05  GSCOL           PIC XX   VALUE X'A0FF'.                  
           05  GRN-GSCOL       PIC X(6) VALUE 'GSCOL '.                 
      *               SET COLOR.                                        
           05  GSCP            PIC XX   VALUE X'2104'.                  
           05  GRN-GSCP        PIC X(6) VALUE 'GSCP  '.                 
      *               SET CURRENT POSITION.                             
           05  GSECOL          PIC XX   VALUE X'2602'.                  
           05  GRN-GSECOL      PIC X(6) VALUE 'GSECOL'.                 
      *               SET EXTENDED COLOR.                               
           05  GSLT            PIC XX   VALUE X'18FF'.                  
           05  GRN-GSLT        PIC X(6) VALUE 'GSLT  '.                 
      *               SET LINE TYPE.                                    
           05  GSLW            PIC XX   VALUE X'19FF'.                  
           05  GRN-GSLW        PIC X(6) VALUE 'GSLW  '.                 
      *               SET LINE WITDH.                                   
           05  GSMC            PIC XX   VALUE X'37FF'.                  
           05  GRN-GSMC        PIC X(6) VALUE 'GSMC  '.                 
      *               SET MARKER CELL.                                  
           05  GSMP            PIC XX   VALUE X'3BFF'.                  
           05  GRN-GSMP        PIC X(6) VALUE 'GSMP  '.                 
      *               SET MARKER PRECISION.                             
           05  GSMS            PIC XX   VALUE X'3CFF'.                  
           05  GRN-GSMS        PIC X(6) VALUE 'GSMS  '.                 
      *               SET MARKER SET.                                   
           05  GSMT            PIC XX   VALUE X'29FF'.                  
           05  GRN-GSMT        PIC X(6) VALUE 'GSMT  '.                 
      *               SET MARKER SYMBOL.                                
           05  GSMX            PIC XX   VALUE X'0CFF'.                  
           05  GRN-GSMX        PIC X(6) VALUE 'GSMT  '.                 
      *               SET MIX.                                          
           05  GSPS            PIC XX   VALUE X'08FF'.                  
           05  GRN-GSPS        PIC X(6) VALUE 'GSPS  '.                 
      *               SET PATTERN SET.                                  
           05  GSPT            PIC XX   VALUE X'28FF'.                  
           05  GRN-GSPT        PIC X(6) VALUE 'GSPT  '.                 
      *               SET PATTERN SYMBOL.                               
           05  GSPIK           PIC XX   VALUE X'4304'.                  
           05  GRN-GSPIK       PIC X(6) VALUE 'GSPIK '.                 
      *               SET PICK IDENTIFIER.                              
         03  GR-TABLE          REDEFINES GR-NAMES                       
                               OCCURS 45,                               
                               ASCENDING KEY GR-ID,                     
                               INDEXED BY GR-ID-INDX.                   
           05  GR-VALUE        PIC X.                                   
           05  GR-LENGTH       PIC X.                                   
           05  GR-ID           PIC X(6).                                
      *                                                                 
      *               GRAPHICS DRAWING CONSTANTS.                       
      *--------------------------------------------------------------*  
      * GOCA GRAPHICS DRAWING CONSTANT VALUE.                        *  
      * USEFUL VALUES FOR DRAWING ORDER PARAMETERS.                  *  
      *--------------------------------------------------------------*  
         03  GR-AREA-BOUNDARIES.                                        
           05  GR-AREA-BOUNDARY    PIC X    VALUE X'C0'.                
           05  GR-AREA-NOBOUNDARY  PIC X    VALUE X'80'.                
         03  GR-CHAR-DIRECTION.                                         
           05  GR-CDIR-DFLT        PIC X    VALUE X'00'.                
           05  GR-CDIR-L2R         PIC X    VALUE X'01'.                
           05  GR-CDIR-T2B         PIC X    VALUE X'02'.                
           05  GR-CDIR-R2L         PIC X    VALUE X'03'.                
           05  GR-CDIR-B2T         PIC X    VALUE X'04'.                
         03  GR-COLOR.                                                  
           05  GR-COLOR-DFLT       PIC X    VALUE X'00'.                
           05  GR-COLOR-BLUE       PIC X    VALUE X'01'.                
           05  GR-COLOR-RED        PIC X    VALUE X'02'.                
           05  GR-COLOR-MAGENTA    PIC X    VALUE X'03'.                
           05  GR-COLOR-GREEN      PIC X    VALUE X'04'.                
           05  GR-COLOR-CYAN       PIC X    VALUE X'05'.                
           05  GR-COLOR-YELLOW     PIC X    VALUE X'06'.                
           05  GR-COLOR-PDFLT      PIC X    VALUE X'07'.                
           05  GR-COLOR-MEDIUM     PIC X    VALUE X'08'.                
         03  GR-EXTENDED-COLOR.                                         
           05  GR-ECOLOR-DFLT      PIC XX   VALUE X'0000'.              
           05  GR-ECOLOR-BLUE      PIC XX   VALUE X'0001'.              
           05  GR-ECOLOR-RED       PIC XX   VALUE X'0002'.              
           05  GR-ECOLOR-MAGENTA   PIC XX   VALUE X'0003'.              
           05  GR-ECOLOR-GREEN     PIC XX   VALUE X'0004'.              
           05  GR-ECOLOR-CYAN      PIC XX   VALUE X'0005'.              
           05  GR-ECOLOR-YELLOW    PIC XX   VALUE X'0006'.              
           05  GR-ECOLOR-BLACK     PIC XX   VALUE X'0008'.              
           05  GR-ECOLOR-BROWN     PIC XX   VALUE X'0010'.              
           05  GR-ECOLOR-PDFLT     PIC XX   VALUE X'FF07'.              
           05  GR-ECOLOR-MEDIUM    PIC XX   VALUE X'FF08'.              
         03  GR-LINE-TYPE.                                              
           05  GR-LINE-DFLT        PIC X    VALUE X'00'.                
           05  GR-LINE-DOTTED      PIC X    VALUE X'01'.                
           05  GR-LINE-SDASH       PIC X    VALUE X'02'.                
           05  GR-LINE-DASHDOT     PIC X    VALUE X'03'.                
           05  GR-LINE-DBLDOT      PIC X    VALUE X'04'.                
           05  GR-LINE-LDASH       PIC X    VALUE X'05'.                
           05  GR-LINE-DASHDDOT    PIC X    VALUE X'06'.                
           05  GR-LINE-SOLID       PIC X    VALUE X'07'.                
           05  GR-LINE-INVISIBLE   PIC X    VALUE X'08'.                
         03  GR-LINE-THICKNESS.                                         
           05  GR-THIK-DFLT        PIC X    VALUE X'00'.                
           05  GR-THIK-NORMAL      PIC X    VALUE X'01'.                
           05  GR-THIK-THICK       PIC X    VALUE X'02'.                
         03  GR-MARKER-SYMBOL.                                          
           05  GR-MARK-DFLT        PIC X    VALUE X'00'.                
           05  GR-MARK-CROSS       PIC X    VALUE X'01'.                
           05  GR-MARK-PLUS        PIC X    VALUE X'02'.                
           05  GR-MARK-DIAMOND     PIC X    VALUE X'03'.                
           05  GR-MARK-SQUARE      PIC X    VALUE X'04'.                
           05  GR-MARK-6PT-STAR    PIC X    VALUE X'05'.                
           05  GR-MARK-9PT-STAR    PIC X    VALUE X'06'.                
           05  GR-MARK-FDIAMOND    PIC X    VALUE X'07'.                
           05  GR-MARK-FSQUARE     PIC X    VALUE X'08'.                
           05  GR-MARK-DOT         PIC X    VALUE X'09'.                
           05  GR-MARK-SMCIRCLE    PIC X    VALUE X'0A'.                
           05  GR-MARK-BLANK       PIC X    VALUE X'40'.                
         03  GR-PATTERN-SYMBOL.                                         
           05  GR-PATT-DFLT        PIC X    VALUE X'00'.                
           05  GR-PATT-GREY-1      PIC X    VALUE X'01'.                
           05  GR-PATT-GREY-2      PIC X    VALUE X'02'.                
           05  GR-PATT-GREY-3      PIC X    VALUE X'03'.                
           05  GR-PATT-GREY-4      PIC X    VALUE X'04'.                
           05  GR-PATT-GREY-5      PIC X    VALUE X'05'.                
           05  GR-PATT-GREY-6      PIC X    VALUE X'06'.                
           05  GR-PATT-GREY-7      PIC X    VALUE X'07'.                
           05  GR-PATT-GREY-8      PIC X    VALUE X'08'.                
           05  GR-PATT-VLINES      PIC X    VALUE X'09'.                
           05  GR-PATT-HLINES      PIC X    VALUE X'0A'.                
           05  GR-PATT-L2R-DIAG1   PIC X    VALUE X'0B'.                
           05  GR-PATT-L2R-DIAG2   PIC X    VALUE X'0C'.                
           05  GR-PATT-T2B-DIAG1   PIC X    VALUE X'0D'.                
           05  GR-PATT-T2B-DIAG2   PIC X    VALUE X'0E'.                
           05  GR-PATT-NONE        PIC X    VALUE X'0F'.                
           05  GR-PATT-SOLID       PIC X    VALUE X'10'.                
           05  GR-PATT-BLANK       PIC X    VALUE X'40'.                
         03  GR-IMAGE-MAPPING.                                          
           05  GR-MAP-TRIM         PIC X    VALUE X'10'.                
           05  GR-MAP-SCALE        PIC X    VALUE X'20'.                
           05  GR-MAP-CENTER       PIC X    VALUE X'30'.                
           05  GR-MAP-PEL          PIC X    VALUE X'41'.                
           05  GR-MAP-DOUBLE-DOT   PIC X    VALUE X'42'.                
         03  GR-IMAGE-ROTATION.                                         
           05  GR-ROT-00           PIC X(4) VALUE X'00002D00'.          
           05  GR-ROT-90           PIC X(4) VALUE X'2D005A00'.          
           05  GR-ROT-180          PIC X(4) VALUE X'5A008700'.          
           05  GR-ROT-270          PIC X(4) VALUE X'87000000'.          
      *                                                                 
      *               TRIPLET IDENTIFIERS.   .                          
      *--------------------------------------------------------------*  
      * COMMON TRIPLET IDENTIFIERS.                                  *  
      * BYTE 1: LENGTH. ZERO REPRESENTS A VARIABLE LENGTH TRIPLET.   *  
      * BYTE 2: TRIPLET IDENTIFIER.                                  *  
      *--------------------------------------------------------------*  
         03  TR-NAMES.                                                  
           05  TR-GCSGID       PIC XX   VALUE X'0601'.                  
           05  TRN-GCSGID      PIC X(6) VALUE 'GCSGID'.                 
      *               GCSGID/CPGID CODE PAGE/CHAR SET ID.               
           05  TR-FQN          PIC XX   VALUE X'0002'.                  
           05  TRN-FQN         PIC X(6) VALUE 'FQN   '.                 
      *               FULLY QUALIFIED NAME (SEE FQN IDS, BELOW).        
           05  TR-INTCHG       PIC XX   VALUE X'0518'.                  
           05  TRN-INTCHG      PIC X(6) VALUE 'INTCHG'.                 
      *               MO:DCA INTERCHANGE SET.                           
           05  TR-OFS          PIC XX   VALUE X'0821'.                  
           05  TRN-OFS         PIC X(6) VALUE 'OFS   '.                 
      *               OBJECT FUNCTION SET SPECIFICATION.                
           05  TR-BYTEOF       PIC XX   VALUE X'062D'.                  
           05  TRN-BYTEX       PIC X(6) VALUE 'BYTEOF'.                 
      *               BYTE OFFSET.          .                           
           05  TR-ATVAL        PIC XX   VALUE X'0036'.                  
           05  TRN-ATVAL       PIC X(6) VALUE 'ATTVAL'.                 
      *               ATTRIBUTE VALUE.                                  
           05  TR-PGMAP        PIC XX   VALUE X'0656'.                  
           05  TRN-PGMAP       PIC X(6) VALUE 'PGMAP '.                 
      *               MEDIUM MAP PAGE NUMBER.                           
           05  TR-BYTEX        PIC XX   VALUE X'0657'.                  
           05  TRN-BYTEX       PIC X(6) VALUE 'BYTEX '.                 
      *               BYTE EXTENT.          .                           
           05  TR-SFOF         PIC XX   VALUE X'0658'.                  
           05  TRN-SFOF        PIC X(6) VALUE 'SFOF  '.                 
      *               STRUCTURED FIELD OFFSET.                          
           05  TR-SFEX         PIC XX   VALUE X'0659'.                  
           05  TRN-SFEX        PIC X(6) VALUE 'SFEX  '.                 
      *               STRUCTURED FIELD EXTENT.                          
           05  TR-DATETIME     PIC XX   VALUE X'1162'.                  
           05  TRN-DATETIME    PIC X(6) VALUE 'TMDT '.                  
      *               OBJECT DATE AND TIMESTAMP.                        
           05  TR-CRC          PIC XX   VALUE X'0663'.                  
           05  TRN-CRC         PIC X(6) VALUE 'CRC  '.                  
      *               CRC RESOURCE MANAGEMENT.                          
           05  TR-OBJORIG      PIC XX   VALUE X'3D64'.                  
           05  TRN-OBJORIG     PIC X(6) VALUE 'OORIG'.                  
      *               OBJECT ORIGIN IDENTIFIER                          
           05  TR-UCOMMENT     PIC XX   VALUE X'0065'.                  
           05  TRN-UCOMMENT    PIC X(6) VALUE 'UCOMM'.                  
      *               USER COMMENT.                                     
           05  TR-ATTQUAL      PIC XX   VALUE X'1080'.                  
           05  TRN-ATTQUAL     PIC X(6) VALUE 'AQUAL'.                  
      *               ATTIBUTE QUALIFIER.                               
         03  TR-TABLE          REDEFINES TR-NAMES                       
                               OCCURS 15,                               
                               ASCENDING KEY TR-VALUE                   
                               INDEXED BY TR-VALUE-INDX.                
           05  TR-LENGTH       PIC X.                                   
           05  TR-VALUE        PIC X.                                   
           05  TR-ID           PIC X(6).                                
      *                                                                 
      *               FULLY QUALIFIED NAME IDENTIFIERS.                 
      *--------------------------------------------------------------*  
      * FQN IDENTIFIERS                                              *  
      * BYTE 1: ID TYPE, 3RD BYTE OF FQN TRIPLET.                    *  
      *--------------------------------------------------------------*  
         03  FQN-PREFIX        PIC 9(4) BINARY VALUE 4.                 
      *               OFFSET TO NAME FIELD OF FQN.                      
         03  FQN-NAMES.                                                 
           05  FQN-GID         PIC X   VALUE X'01'.                     
      *               REPLACE FIRST GID NAME.                           
           05  FQN-BRG         PIC X   VALUE X'0A'.                     
      *               BEGIN RESOURCE GROUP.                             
           05  FQN-ATTR        PIC X   VALUE X'0B'.                     
      *               DOCUMENT ATTRIBUTE.                               
           05  FQN-PREL        PIC X   VALUE X'0C'.                     
      *               PROCESS ELEMENT.                                  
           05  FQN-PGRP        PIC X   VALUE X'0D'.                     
      *               BEGIN PAGE GROUP.                                 
           05  FQN-BDT         PIC X   VALUE X'83'.                     
      *               BEGIN DOCUMENT REFERENCE.                         
           05  FQN-BPG         PIC X   VALUE X'87'.                     
      *               BEGIN PAGE REFERENCE.                             
           05  FQN-BMM         PIC X   VALUE X'8D'.                     
      *               BEGIN MEDIUM MAP.                                 
           05  FQN-BDI         PIC X   VALUE X'98'.                     
      *               BEGIN DOCUMENT INDEX.                             
         03  FQN-TABLE         REDEFINES FQN-NAMES                      
                               OCCURS 09,                               
                               ASCENDING KEY FQN-ID                     
                               INDEXED BY FQN-INDX.                     
           05  FQN-ID          PIC X.                                   
