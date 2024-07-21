      **************************************************************     
      *                                                                  
      *     Aufbereitung Storno AM-Leben-Verträge                        
      *                                                                  
      *     SOURCE-NAME      :  DRMA3142                                 
      *                                                                  
      *     GENERIERUNG      :  08.04.2021/10:52:42                      
      *     DURCH            :  PMMA5004/01.00.01                        
      *                                                                  
      * LOGBUCH                                                          
      * AENDERUNGEN:                                                     
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME        
      * --------------------------------------------------------------   
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM   
      * AMA00360!20.09.17!2    ! ÄNDERUNG DEMA0128         ! WARZ        
      *         !        !     ! und DEMA0035 PROVEINBEHALT!             
      * MA-908  !07.04.21!3    ! EINFÜGEN                  ! D.WILHELM   
      *         !        !     !  STORNO-ANZ-NICHTEINL     !             
      *         !        !     !  STORNO-ANZ-ERSTJAHR      !             
      *         !        !     !  STORNO-ANZ-FOLGE         !             
      *         !        !     !  STORNO-ANZ-AUFH-WIDER    !             
      *         !        !     !                           !             
      *                                                                  
      * ENDELOGBUCH                                                      
      **************************************************************     
      *                                                                  
      *                                                                  
           05 DRMA3142.                                                  
              07 BUCHDAT                pic X(10).                       
              07 GESKZ                  pic 99.                          
              07 PRODSCHL               pic 999.                         
              07 KENNZEICHEN            pic X.                           
              07 PROVAUSZAHLUNG         pic S9(11)V99  packed-decimal.   
              07 PROVEINBEHALT          pic S9(11)V99  packed-decimal.   
              07 VTF                    pic S9(11)V99  packed-decimal.   
              07 VWF                    pic S9(11)V99  packed-decimal.   
              07 PROVGESAMT             pic S9(11)V99  packed-decimal.   
              07 AE                     pic S9(11)V99  packed-decimal.   
              07 PRODART                pic 99.                          
              07 JHRG-KZN               pic 99.                          
              07 STORNO-ANZ-NICHTEINL   pic 9(7).                        
              07 STORNO-ANZ-ERSTJAHR    pic 9(7).                        
              07 STORNO-ANZ-FOLGE       pic 9(7).                        
              07 STORNO-ANZ-AUFH-WIDER  pic 9(7).                        
      *                                                                  
      **************************************************************     