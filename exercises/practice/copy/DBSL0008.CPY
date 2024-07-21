      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  23.06.2008                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DBSL0008.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DBSL0008.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Oct 30 2013 15:42:36   t13ex57                              
      *ASL00498: Versorgungswerk 2013: Anp. SL06 (UBO)                          
      *                                                                         
      *   Rev 2.3   Oct 13 2009 14:16:02   t13ex43                              
      *ASL00341: PGM - Änderung SL06-Dialog                                     
      *                                                                         
      *   Rev 2.2   Aug 31 2009 09:33:34   t13ex43                              
      *ASL00335: PGM - VSW Release 1.1 (Dialog)                                 
      *                                                                         
      *   Rev 2.1   Sep 09 2008 12:04:34   t13ex43                              
      *ASL00309: PGM - Redesign Versorgungswerk (Dialog 1)                      
      *                                                                         
      *   Rev 2.0   Jun 23 2008 16:15:06   t13ex43                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75                                 
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-58)                   
      *   MAPSET NAME   BSL0008                                                 
      *   DATE CREATED  08/05/2009                                              
      *   TIME CREATED  16:11:44                                                
      *                                                                         
      * AUFTRAG ! DATUM  !  AENDERUNG                     ! NAME                
      * --------+--------+--------------------------------+----------           
      * ASL00341!01.10.09! EQF MIT 1 NK, TEXTE KÜRZER     ! KLEEMEIER      
231013* ASL00498!23.10.13! EINGABE BESTANDSSCHUTZ         ! Boll           
291121* VLA-3689!29.11.21! EQF vereinheitlicht            ! Bruder      
      *         !        !                                !
      *
      *  INPUT DATA FOR MAP BSL0008                                             
         01 BSL0008I.                                                           
            03 FILLER                         PIC X(12).                        
            03 EGESL                          PIC S9(4) COMP.                   
            03 EGESF                          PIC X.                            
            03 FILLER REDEFINES EGESF.                                          
               05 EGESA                          PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EGESI                          PIC X(1).                         
            03 ELEISTL                        PIC S9(4) COMP.                   
            03 ELEISTF                        PIC X.                            
            03 FILLER REDEFINES ELEISTF.                                        
               05 ELEISTA                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ELEISTI                        PIC X(1).                         
            03 EBSTATL                        PIC S9(4) COMP.                   
            03 EBSTATF                        PIC X.                            
            03 FILLER REDEFINES EBSTATF.                                        
               05 EBSTATA                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EBSTATI                        PIC X(7).                         
            03 EARTL                          PIC S9(4) COMP.                   
            03 EARTF                          PIC X.                            
            03 FILLER REDEFINES EARTF.                                          
               05 EARTA                          PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EARTI                          PIC X(1).                         
            03 ATEXT1L                        PIC S9(4) COMP.                   
            03 ATEXT1F                        PIC X.                            
            03 FILLER REDEFINES ATEXT1F.                                        
               05 ATEXT1A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
291121      03 ATEXT1I                        PIC X(9).                         
            03 EEQF1L                         PIC S9(4) COMP.                   
            03 EEQF1F                         PIC X.                            
            03 FILLER REDEFINES EEQF1F.                                         
               05 EEQF1A                         PIC X.                         
            03 FILLER                         PIC X(1).                         
011009****  03 EEQF1I                         PIC X(2).                         
011009      03 EEQF1I                         PIC X(4).                         
            03 ATEXT2L                        PIC S9(4) COMP.                   
            03 ATEXT2F                        PIC X.                            
            03 FILLER REDEFINES ATEXT2F.                                        
               05 ATEXT2A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
011009****  03 ATEXT2I                        PIC X(46).                        
291121      03 ATEXT2I                        PIC X(13).                        
            03 ATEXT3L                        PIC S9(4) COMP.                   
            03 ATEXT3F                        PIC X.                            
            03 FILLER REDEFINES ATEXT3F.                                        
               05 ATEXT3A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
291121      03 ATEXT3I                        PIC X(9).                         
            03 EEQF2L                         PIC S9(4) COMP.                   
            03 EEQF2F                         PIC X.                            
            03 FILLER REDEFINES EEQF2F.                                         
               05 EEQF2A                         PIC X.                         
            03 FILLER                         PIC X(1).                         
011009****  03 EEQF2I                         PIC X(2).                         
011009      03 EEQF2I                         PIC X(4).                         
            03 ATEXT4L                        PIC S9(4) COMP.                   
            03 ATEXT4F                        PIC X.                            
            03 FILLER REDEFINES ATEXT4F.                                        
               05 ATEXT4A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
011009****  03 ATEXT4I                        PIC X(46).                        
291121      03 ATEXT4I                        PIC X(32).                        
            03 ATEXT5L                        PIC S9(4) COMP.                   
            03 ATEXT5F                        PIC X.                            
            03 FILLER REDEFINES ATEXT5F.                                        
               05 ATEXT5A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT5I                        PIC X(7).                         
            03 EEQF3L                         PIC S9(4) COMP.                   
            03 EEQF3F                         PIC X.                            
            03 FILLER REDEFINES EEQF3F.                                         
               05 EEQF3A                         PIC X.                         
            03 FILLER                         PIC X(1).                         
011009****  03 EEQF3I                         PIC X(2).                         
011009      03 EEQF3I                         PIC X(4).                         
            03 ATEXT6L                        PIC S9(4) COMP.                   
            03 ATEXT6F                        PIC X.                            
            03 FILLER REDEFINES ATEXT6F.                                        
               05 ATEXT6A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
011009****  03 ATEXT6I                        PIC X(46).                        
291121      03 ATEXT6I                        PIC X(16).                        
            03 ATEXT7L                        PIC S9(4) COMP.                   
            03 ATEXT7F                        PIC X.                            
            03 FILLER REDEFINES ATEXT7F.                                        
               05 ATEXT7A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT7I                        PIC X(15).                        
            03 EEH1L                          PIC S9(4) COMP.                   
            03 EEH1F                          PIC X.                            
            03 FILLER REDEFINES EEH1F.                                          
               05 EEH1A                          PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EEH1I                          PIC X(5).                         
            03 ATEXT8L                        PIC S9(4) COMP.                   
            03 ATEXT8F                        PIC X.                            
            03 FILLER REDEFINES ATEXT8F.                                        
               05 ATEXT8A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT8I                        PIC X(14).                        
            03 EPRO1L                         PIC S9(4) COMP.                   
            03 EPRO1F                         PIC X.                            
            03 FILLER REDEFINES EPRO1F.                                         
               05 EPRO1A                         PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EPRO1I                         PIC X(3).                         
            03 ATEXT21L                       PIC S9(4) COMP.                   
            03 ATEXT21F                       PIC X.                            
            03 FILLER REDEFINES ATEXT21F.                                       
               05 ATEXT21A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT21I                       PIC X(1).                         
            03 ATEXT9L                        PIC S9(4) COMP.                   
            03 ATEXT9F                        PIC X.                            
            03 FILLER REDEFINES ATEXT9F.                                        
               05 ATEXT9A                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT9I                        PIC X(6).                         
            03 EEH2L                          PIC S9(4) COMP.                   
            03 EEH2F                          PIC X.                            
            03 FILLER REDEFINES EEH2F.                                          
               05 EEH2A                          PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EEH2I                          PIC X(5).                         
            03 ATEXT10L                       PIC S9(4) COMP.                   
            03 ATEXT10F                       PIC X.                            
            03 FILLER REDEFINES ATEXT10F.                                       
               05 ATEXT10A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT10I                       PIC X(14).                        
            03 EPRO2L                         PIC S9(4) COMP.                   
            03 EPRO2F                         PIC X.                            
            03 FILLER REDEFINES EPRO2F.                                         
               05 EPRO2A                         PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EPRO2I                         PIC X(3).                         
            03 ATEXT22L                       PIC S9(4) COMP.                   
            03 ATEXT22F                       PIC X.                            
            03 FILLER REDEFINES ATEXT22F.                                       
               05 ATEXT22A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT22I                       PIC X(1).                         
            03 ATEXT11L                       PIC S9(4) COMP.                   
            03 ATEXT11F                       PIC X.                            
            03 FILLER REDEFINES ATEXT11F.                                       
               05 ATEXT11A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT11I                       PIC X(6).                         
            03 EEH3L                          PIC S9(4) COMP.                   
            03 EEH3F                          PIC X.                            
            03 FILLER REDEFINES EEH3F.                                          
               05 EEH3A                          PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EEH3I                          PIC X(5).                         
            03 ATEXT12L                       PIC S9(4) COMP.                   
            03 ATEXT12F                       PIC X.                            
            03 FILLER REDEFINES ATEXT12F.                                       
               05 ATEXT12A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT12I                       PIC X(14).                        
            03 EPRO3L                         PIC S9(4) COMP.                   
            03 EPRO3F                         PIC X.                            
            03 FILLER REDEFINES EPRO3F.                                         
               05 EPRO3A                         PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EPRO3I                         PIC X(3).                         
            03 ATEXT23L                       PIC S9(4) COMP.                   
            03 ATEXT23F                       PIC X.                            
            03 FILLER REDEFINES ATEXT23F.                                       
               05 ATEXT23A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT23I                       PIC X(1).                         
            03 ATEXT13L                       PIC S9(4) COMP.                   
            03 ATEXT13F                       PIC X.                            
            03 FILLER REDEFINES ATEXT13F.                                       
               05 ATEXT13A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT13I                       PIC X(13).                        
            03 EAUFBEDL                       PIC S9(4) COMP.                   
            03 EAUFBEDF                       PIC X.                            
            03 FILLER REDEFINES EAUFBEDF.                                       
               05 EAUFBEDA                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EAUFBEDI                       PIC X(4).                         
            03 ATEXT14L                       PIC S9(4) COMP.                   
            03 ATEXT14F                       PIC X.                            
            03 FILLER REDEFINES ATEXT14F.                                       
               05 ATEXT14A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT14I                       PIC X(6).                         
            03 ATEXT15L                       PIC S9(4) COMP.                   
            03 ATEXT15F                       PIC X.                            
            03 FILLER REDEFINES ATEXT15F.                                       
               05 ATEXT15A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT15I                       PIC X(15).                        
            03 EVB2007L                       PIC S9(4) COMP.                   
            03 EVB2007F                       PIC X.                            
            03 FILLER REDEFINES EVB2007F.                                       
               05 EVB2007A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EVB2007I                       PIC X(1).                         
            03 ATEXT16L                       PIC S9(4) COMP.                   
            03 ATEXT16F                       PIC X.                            
            03 FILLER REDEFINES ATEXT16F.                                       
               05 ATEXT16A                       PIC X.                         
231013      03 FILLER                         PIC X(1).                         
231013      03 ATEXT16I                       PIC X(5).                         
231013      03 ATEXT24L                       PIC S9(4) COMP.                   
231013      03 ATEXT24F                       PIC X.                            
231013      03 FILLER REDEFINES ATEXT24F.                                       
231013         05 ATEXT24A                       PIC X.                         
231013      03 FILLER                         PIC X(1).                         
231013      03 ATEXT24I                       PIC X(2).                         
231013      03 EBESTL                         PIC S9(4) COMP.                   
231013      03 EBESTF                         PIC X.                            
231013      03 FILLER REDEFINES EBESTF.                                         
231013         05 EBESTA                         PIC X.                         
231013      03 FILLER                         PIC X(1).                         
231013      03 EBESTI                         PIC X(1).                         
231013      03 ATEXT25L                       PIC S9(4) COMP.                   
231013      03 ATEXT25F                       PIC X.                            
231013      03 FILLER REDEFINES ATEXT25F.                                       
231013         05 ATEXT25A                       PIC X.                         
231013      03 FILLER                         PIC X(1).                         
231013      03 ATEXT25I                       PIC X(5).                         
            03 ATEXT17L                       PIC S9(4) COMP.                   
            03 ATEXT17F                       PIC X.                            
            03 FILLER REDEFINES ATEXT17F.                                       
               05 ATEXT17A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
231013*     03 ATEXT17I                       PIC X(26).                        
231013      03 ATEXT17I                       PIC X(3).                         
            03 EBZLL                          PIC S9(4) COMP.                   
            03 EBZLF                          PIC X.                            
            03 FILLER REDEFINES EBZLF.                                          
               05 EBZLA                          PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EBZLI                          PIC X(1).                         
            03 ATEXT18L                       PIC S9(4) COMP.                   
            03 ATEXT18F                       PIC X.                            
            03 FILLER REDEFINES ATEXT18F.                                       
               05 ATEXT18A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT18I                       PIC X(5).                         
            03 ATEXT19L                       PIC S9(4) COMP.                   
            03 ATEXT19F                       PIC X.                            
            03 FILLER REDEFINES ATEXT19F.                                       
               05 ATEXT19A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT19I                       PIC X(9).                         
            03 E40PROZL                       PIC S9(4) COMP.                   
            03 E40PROZF                       PIC X.                            
            03 FILLER REDEFINES E40PROZF.                                       
               05 E40PROZA                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 E40PROZI                       PIC X(1).                         
            03 ATEXT20L                       PIC S9(4) COMP.                   
            03 ATEXT20F                       PIC X.                            
            03 FILLER REDEFINES ATEXT20F.                                       
               05 ATEXT20A                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ATEXT20I                       PIC X(5).                         
            03 ESTUFEL                        PIC S9(4) COMP.                   
            03 ESTUFEF                        PIC X.                            
            03 FILLER REDEFINES ESTUFEF.                                        
               05 ESTUFEA                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 ESTUFEI                        PIC X(4).                         
            03 EPRAXISL                       PIC S9(4) COMP.                   
            03 EPRAXISF                       PIC X.                            
            03 FILLER REDEFINES EPRAXISF.                                       
               05 EPRAXISA                       PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 EPRAXISI                       PIC X(1).                         
            03 AFCANZL                        PIC S9(4) COMP.                   
            03 AFCANZF                        PIC X.                            
            03 FILLER REDEFINES AFCANZF.                                        
               05 AFCANZA                        PIC X.                         
            03 FILLER                         PIC X(1).                         
            03 AFCANZI                        PIC X(76).                        
                                                                                
      *  OUTPUT DATA FOR MAP BSL0008                                            
         01 BSL0008O REDEFINES BSL0008I.                                        
            03 FILLER                         PIC X(12).                        
            03 FILLER                         PIC X(2).                         
            03 EGESA                          PIC X.                            
            03 EGESH                          PIC X.                            
            03 EGESO                          PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ELEISTA                        PIC X.                            
            03 ELEISTH                        PIC X.                            
            03 ELEISTO                        PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 EBSTATA                        PIC X.                            
            03 EBSTATH                        PIC X.                            
            03 EBSTATO                        PIC X(7).                         
            03 FILLER                         PIC X(2).                         
            03 EARTA                          PIC X.                            
            03 EARTH                          PIC X.                            
            03 EARTO                          PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT1A                        PIC X.                            
            03 ATEXT1H                        PIC X.                            
291121      03 ATEXT1O                        PIC X(9).                         
            03 FILLER                         PIC X(2).                         
            03 EEQF1A                         PIC X.                            
            03 EEQF1H                         PIC X.                            
011009****  03 EEQF1O                         PIC X(2).                         
011009      03 EEQF1O                         PIC X(4).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT2A                        PIC X.                            
            03 ATEXT2H                        PIC X.                            
011009****  03 ATEXT2O                        PIC X(46).                        
291121      03 ATEXT2O                        PIC X(13).                       
            03 FILLER                         PIC X(2).                         
            03 ATEXT3A                        PIC X.                            
            03 ATEXT3H                        PIC X.                            
291121      03 ATEXT3O                        PIC X(9).                         
            03 FILLER                         PIC X(2).                         
            03 EEQF2A                         PIC X.                            
            03 EEQF2H                         PIC X.                            
011009****  03 EEQF2O                         PIC X(2).                         
011009      03 EEQF2O                         PIC X(4).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT4A                        PIC X.                            
            03 ATEXT4H                        PIC X.                            
011009****  03 ATEXT4O                        PIC X(46).                        
291121      03 ATEXT4O                        PIC X(32).                        
            03 FILLER                         PIC X(2).                         
            03 ATEXT5A                        PIC X.                            
            03 ATEXT5H                        PIC X.                            
            03 ATEXT5O                        PIC X(7).                         
            03 FILLER                         PIC X(2).                         
            03 EEQF3A                         PIC X.                            
            03 EEQF3H                         PIC X.                            
011009****  03 EEQF3O                         PIC X(2).                         
011009      03 EEQF3O                         PIC X(4).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT6A                        PIC X.                            
            03 ATEXT6H                        PIC X.                            
011009****  03 ATEXT6O                        PIC X(46).                        
291121      03 ATEXT6O                        PIC X(16).                        
            03 FILLER                         PIC X(2).                         
            03 ATEXT7A                        PIC X.                            
            03 ATEXT7H                        PIC X.                            
            03 ATEXT7O                        PIC X(15).                        
            03 FILLER                         PIC X(2).                         
            03 EEH1A                          PIC X.                            
            03 EEH1H                          PIC X.                            
            03 EEH1O                          PIC X(5).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT8A                        PIC X.                            
            03 ATEXT8H                        PIC X.                            
            03 ATEXT8O                        PIC X(14).                        
            03 FILLER                         PIC X(2).                         
            03 EPRO1A                         PIC X.                            
            03 EPRO1H                         PIC X.                            
            03 EPRO1O                         PIC X(3).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT21A                       PIC X.                            
            03 ATEXT21H                       PIC X.                            
            03 ATEXT21O                       PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT9A                        PIC X.                            
            03 ATEXT9H                        PIC X.                            
            03 ATEXT9O                        PIC X(6).                         
            03 FILLER                         PIC X(2).                         
            03 EEH2A                          PIC X.                            
            03 EEH2H                          PIC X.                            
            03 EEH2O                          PIC X(5).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT10A                       PIC X.                            
            03 ATEXT10H                       PIC X.                            
            03 ATEXT10O                       PIC X(14).                        
            03 FILLER                         PIC X(2).                         
            03 EPRO2A                         PIC X.                            
            03 EPRO2H                         PIC X.                            
            03 EPRO2O                         PIC X(3).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT22A                       PIC X.                            
            03 ATEXT22H                       PIC X.                            
            03 ATEXT22O                       PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT11A                       PIC X.                            
            03 ATEXT11H                       PIC X.                            
            03 ATEXT11O                       PIC X(6).                         
            03 FILLER                         PIC X(2).                         
            03 EEH3A                          PIC X.                            
            03 EEH3H                          PIC X.                            
            03 EEH3O                          PIC X(5).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT12A                       PIC X.                            
            03 ATEXT12H                       PIC X.                            
            03 ATEXT12O                       PIC X(14).                        
            03 FILLER                         PIC X(2).                         
            03 EPRO3A                         PIC X.                            
            03 EPRO3H                         PIC X.                            
            03 EPRO3O                         PIC X(3).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT23A                       PIC X.                            
            03 ATEXT23H                       PIC X.                            
            03 ATEXT23O                       PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT13A                       PIC X.                            
            03 ATEXT13H                       PIC X.                            
            03 ATEXT13O                       PIC X(13).                        
            03 FILLER                         PIC X(2).                         
            03 EAUFBEDA                       PIC X.                            
            03 EAUFBEDH                       PIC X.                            
            03 EAUFBEDO                       PIC X(4).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT14A                       PIC X.                            
            03 ATEXT14H                       PIC X.                            
            03 ATEXT14O                       PIC X(6).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT15A                       PIC X.                            
            03 ATEXT15H                       PIC X.                            
            03 ATEXT15O                       PIC X(15).                        
            03 FILLER                         PIC X(2).                         
            03 EVB2007A                       PIC X.                            
            03 EVB2007H                       PIC X.                            
            03 EVB2007O                       PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT16A                       PIC X.                            
            03 ATEXT16H                       PIC X.                            
            03 ATEXT16O                       PIC X(5).                         
            03 FILLER                         PIC X(2).                         
231013      03 ATEXT24A                       PIC X.                            
231013      03 ATEXT24H                       PIC X.                            
231013      03 ATEXT24O                       PIC X(2).                         
231013      03 FILLER                         PIC X(2).                         
231013      03 EBESTA                         PIC X.                            
231013      03 EBESTH                         PIC X.                            
231013      03 EBESTO                         PIC X(1).                         
231013      03 FILLER                         PIC X(2).                         
231013      03 ATEXT25A                       PIC X.                            
231013      03 ATEXT25H                       PIC X.                            
231013      03 ATEXT25O                       PIC X(5).                         
231013      03 FILLER                         PIC X(2).                         
            03 ATEXT17A                       PIC X.                            
            03 ATEXT17H                       PIC X.                            
231013*     03 ATEXT17O                       PIC X(26).                        
231013      03 ATEXT17O                       PIC X(3).                         
            03 FILLER                         PIC X(2).                         
            03 EBZLA                          PIC X.                            
            03 EBZLH                          PIC X.                            
            03 EBZLO                          PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT18A                       PIC X.                            
            03 ATEXT18H                       PIC X.                            
            03 ATEXT18O                       PIC X(5).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT19A                       PIC X.                            
            03 ATEXT19H                       PIC X.                            
            03 ATEXT19O                       PIC X(9).                         
            03 FILLER                         PIC X(2).                         
            03 E40PROZA                       PIC X.                            
            03 E40PROZH                       PIC X.                            
            03 E40PROZO                       PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 ATEXT20A                       PIC X.                            
            03 ATEXT20H                       PIC X.                            
            03 ATEXT20O                       PIC X(5).                         
            03 FILLER                         PIC X(2).                         
            03 ESTUFEA                        PIC X.                            
            03 ESTUFEH                        PIC X.                            
            03 ESTUFEO                        PIC X(4).                         
            03 FILLER                         PIC X(2).                         
            03 EPRAXISA                       PIC X.                            
            03 EPRAXISH                       PIC X.                            
            03 EPRAXISO                       PIC X(1).                         
            03 FILLER                         PIC X(2).                         
            03 AFCANZA                        PIC X.                            
            03 AFCANZH                        PIC X.                            
            03 AFCANZO                        PIC X(76).                        
