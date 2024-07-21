      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
110322* JIRA     ! DATUM  ! AENDERUNG                     ! NAME     *
110322* -------------------------------------------------------------*
110322* VLA-4411 !11.03.22! EMAIL-VORSCHLAG HINZU UND     !          *
110322*          !        ! PVCS-KOPF ENTFERNT            ! KLEEMEIER*
      *          !        !                               !          *
      *                                                              *
      ****************************************************************
      *---------------------------------------------------------------*
      *   MICRO FOCUS ENTERPRISE DEVELOPER FOR ECLIPSE  5.0.00256
      *   MICRO FOCUS BMS SCREEN PAINTER (VER BMSIDE_5-0-01)
      *   MAPSET NAME   BVB0054
      *   DATE CREATED  03/14/2022
      *   TIME CREATED  09:08:23

      *  INPUT DATA FOR MAP BVB0054
         01 BVB0054I.
            03 FILLER                         PIC X(12).
            03 EFUNKL                         PIC S9(4) COMP.
            03 EFUNKF                         PIC X.
            03 FILLER REDEFINES EFUNKF.
               05 EFUNKA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EFUNKI                         PIC X(1).
            03 ATXT1L                         PIC S9(4) COMP.
            03 ATXT1F                         PIC X.
            03 FILLER REDEFINES ATXT1F.
               05 ATXT1A                         PIC X.
            03 FILLER                         PIC X(1).
            03 ATXT1I                         PIC X(45).
            03 ESEIAKTL                       PIC S9(4) COMP.
            03 ESEIAKTF                       PIC X.
            03 FILLER REDEFINES ESEIAKTF.
               05 ESEIAKTA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ESEIAKTI                       PIC X(3).
            03 ASEIGESL                       PIC S9(4) COMP.
            03 ASEIGESF                       PIC X.
            03 FILLER REDEFINES ASEIGESF.
               05 ASEIGESA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ASEIGESI                       PIC X(3).
            03 ATXT2L                         PIC S9(4) COMP.
            03 ATXT2F                         PIC X.
            03 FILLER REDEFINES ATXT2F.
               05 ATXT2A                         PIC X.
            03 FILLER                         PIC X(1).
            03 ATXT2I                         PIC X(3).
            03 DFHG001I                       OCCURS 12 TIMES.                  
               05 EFCL                            PIC S9(4) COMP.               
               05 EFCF                            PIC X.                        
               05 FILLER REDEFINES EFCF.                                        
                  08 EFCA                         PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 EFCI                            PIC X(1).                     
               05 ADGL                            PIC S9(4) COMP.               
               05 ADGF                            PIC X.                        
               05 FILLER REDEFINES ADGF.                                        
                  08 ADGA                         PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 ADGI                            PIC X(10).                    
               05 ADIRL                           PIC S9(4) COMP.               
               05 ADIRF                           PIC X.                        
               05 FILLER REDEFINES ADIRF.                                       
                  08 ADIRA                        PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 ADIRI                           PIC X(3).                     
               05 EANRL                           PIC S9(4) COMP.               
               05 EANRF                           PIC X.                        
               05 FILLER REDEFINES EANRF.                                       
                  08 EANRA                        PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 EANRI                           PIC X(4).                     
               05 ETITL                           PIC S9(4) COMP.               
               05 ETITF                           PIC X.                        
               05 FILLER REDEFINES ETITF.                                       
                  08 ETITA                        PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 ETITI                           PIC X(5).                     
               05 EVNAML                          PIC S9(4) COMP.               
               05 EVNAMF                          PIC X.                        
               05 FILLER REDEFINES EVNAMF.                                      
                  08 EVNAMA                       PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 EVNAMI                          PIC X(15).                    
               05 ENNAML                          PIC S9(4) COMP.               
               05 ENNAMF                          PIC X.                        
               05 FILLER REDEFINES ENNAMF.                                      
                  08 ENNAMA                       PIC X.                        
               05 FILLER                          PIC X(1).                     
               05 ENNAMI                          PIC X(20).                    
            03 AMAILTL                        PIC S9(4) COMP.
            03 AMAILTF                        PIC X.
            03 FILLER REDEFINES AMAILTF.
               05 AMAILTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AMAILTI                        PIC X(30).
            03 EMAILL                         PIC S9(4) COMP.
            03 EMAILF                         PIC X.
            03 FILLER REDEFINES EMAILF.
               05 EMAILA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EMAILI                         PIC X(46).
            03 AFCANZL                        PIC S9(4) COMP.
            03 AFCANZF                        PIC X.
            03 FILLER REDEFINES AFCANZF.
               05 AFCANZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AFCANZI                        PIC X(76).

      *  OUTPUT DATA FOR MAP BVB0054
         01 BVB0054O REDEFINES BVB0054I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 EFUNKH                         PIC X.
            03 EFUNKO                         PIC X(1).
            03 FILLER                         PIC X(3).
            03 ATXT1H                         PIC X.
            03 ATXT1O                         PIC X(45).
            03 FILLER                         PIC X(3).
            03 ESEIAKTH                       PIC X.
            03 ESEIAKTO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ASEIGESH                       PIC X.
            03 ASEIGESO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 ATXT2H                         PIC X.
            03 ATXT2O                         PIC X(3).
            03 DFHG001O OCCURS 12.                                              
               05 FILLER                         PIC X(3).                      
               05 EFCH                           PIC X.                         
               05 EFCO                           PIC X(1).                      
               05 FILLER                         PIC X(3).                      
               05 ADGH                           PIC X.                         
               05 ADGO                           PIC X(10).                     
               05 FILLER                         PIC X(3).                      
               05 ADIRH                          PIC X.                         
               05 ADIRO                          PIC X(3).                      
               05 FILLER                         PIC X(3).                      
               05 EANRH                          PIC X.                         
               05 EANRO                          PIC X(4).                      
               05 FILLER                         PIC X(3).                      
               05 ETITH                          PIC X.                         
               05 ETITO                          PIC X(5).                      
               05 FILLER                         PIC X(3).                      
               05 EVNAMH                         PIC X.                         
               05 EVNAMO                         PIC X(15).                     
               05 FILLER                         PIC X(3).                      
               05 ENNAMH                         PIC X.                         
               05 ENNAMO                         PIC X(20).                     
            03 FILLER                         PIC X(3).
            03 AMAILTH                        PIC X.
            03 AMAILTO                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 EMAILH                         PIC X.
            03 EMAILO                         PIC X(46).
            03 FILLER                         PIC X(3).
            03 AFCANZH                        PIC X.
            03 AFCANZO                        PIC X(76).

