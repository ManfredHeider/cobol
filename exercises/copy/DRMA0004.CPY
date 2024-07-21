      **************************************************************    
      *                                                                 
      *     SST Storno restl.                                           
      *                                                                 
      *     SOURCE-NAME      :  DRMA0004                                
      *                                                                 
      *     GENERIERUNG      :  22.10.2021/16:13:27                     
      *     DURCH            :  PMMA5004/01.00.01                       
      *                                                                 
      * LOGBUCH                                                         
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME       
      * --------------------------------------------------------------  
      * MA-653  !22.10.21! 1   ! NEUERSTELLUNG             !D.WILHELM   
      *         !        !     !                           !            
      *                                                                 
      * ENDELOGBUCH                                                     
      **************************************************************    
      *                                                                 
      *                                                                 
           05 DRMA0004.                                                 
              07 PG                     pic 99.                         
              07 PRODSCHL               pic 999.                        
              07 PRODART                pic 99.                         
              07 PROVAUS                pic S9(9)V99   packed-decimal.  
              07 PROVEINB               pic S9(9)V99   packed-decimal.  
              07 PROVEINH               pic S9(10)V9   packed-decimal.  
              07 PE-MANR                pic 9(9).                       
              07 ERWERB-FOLGE-KZN       pic X.                          
              07 BUCHDAT                pic X(10).                      
              07 HZRBEG-JAHR            pic 9(4).                       
              07 HZRBEG-MONAT           pic 99.                         
              07 DGV                    pic 9(5).                       
      *                                                                 
      **************************************************************    