      ******************************************************************        
      *                                                                *        
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand           *      
      *                Kontokorrentdaten von der Deutschen Bank        *        
      *                                                                *        
      *    SATZLÄNGE 255                                               *       
      *                                                                *        
      *    LOGBUCH                                                     *        
      *--------!-------------------------------------------!-----------*        
      *****************************************************************         
      *****************************************************************         
      * LOGBUCH:                                                      *         
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *         
      * --------------------------------------------------------------*         
      * DBO-8784   !22.07.21! NEU                          ! WHO      *         
      *            !        !                              !          *         
      *            !        !                              !          *         
      *                                                               *         
      * ENDE-LOGBUCH                                                  *         
      ******************************************************************        
      *                                                                         
       01  DRKI3894.                                                            
      *                                                                         
      ******************************************************************        
      *                                                                *        
      *                 VORSATZ                                        *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
           10  'XX'-VORSATZ.                                                    
               15  'XX'-V-SA                      PIC X(001).                   
      *            SATZART = 'A'                                                
               15  'XX'-V-ABSENDER                PIC X(008).                   
      *             ABSENDER = 'AGI,HOF'
               15  'XX'-V-DATUM                   PIC 9(008).                   
      *            Buchungsdatum = 'JJJJMMTT'                                   
               15  'XX'-V-EMPFAENGER              PIC X(004).                   
      *            EMPFAENGER = 'DVAG'                                          
               15  'XX'-V-TEXT                    PIC X(010).                 
      *            INHALTSANGABE:                                               
      *            Tägliche Lieferung 'UMSATZFB'                               
      *            Komplettbestand    'UMSATZFBK'                               
               15  'XX'-V-WKZ                     PIC X(003).                   
      *            WÄHRUNGSKENNZEICHEN = 'EUR'                                 
               15  FILLER                         PIC X(221).                   
      *            RESERVE                                                      
      ******************************************************************        
      *                                                                *        
      *         HAUPTSATZ                                              *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
           10  'XX'-SATZ   REDEFINES      'XX'-VORSATZ.                         
      *                                                                         
               15  'XX'-SA                        PIC  X(001).                 
      *            SATZART        = 'C'                                         
               15  'XX'-GES                       PIC  9(002).                  
      *            GESELLSCHAFT   = '40'                                       
               15  'XX'-KEY.                                                    
                   20  'XX'-VNR                   PIC  9(015).               
      *                                                                         
               15  'XX'-DATUM-EROEFFNUNG.                                       
                   20 'XX'-JAHR-EROEFFNUNG        PIC  9(004).                  
                   20 'XX'-MONAT-EROEFFNUNG       PIC  9(002).                  
                   20 'XX'-TAG-EROEFFNUNG         PIC  9(002).                  
                                                                                
               15  'XX'-DATUM-SCHLIESSUNG.                                      
                   20 'XX'-JAHR-SCHLIESSUNG       PIC  9(004).                  
                   20 'XX'-MONAT-SCHLIESSUNG      PIC  9(002).                  
                   20 'XX'-TAG-SCHLIESSUNG        PIC  9(002).                  
               15  'XX'-BILANZSCHLUESSEL          PIC  X(004).                 
               15  'XX'-EXTERNES-LIMIT            PIC  9(012)V99.               
               15  'XX'-VORZEICHEN-SALDO          PIC  X(001).                 
               15  'XX'-SALDO                     PIC  S9(012)V99.              
      *                                                                         
               15  'XX'-KONDITIONSSCHLUESSEL      PIC  X(002).                  
               15  'XX'-DEL-KZ                    PIC  X(001).                  
               15  'XX'-KUMHABENAM                PIC  S9(012)V99.              
               15  'XX'-KUMSOLLAM                 PIC  S9(012)V99.              
               15  'XX'-KUMHABENVM                PIC  S9(012)V99.              
               15  'XX'-KUMSOLLVM                 PIC  S9(012)V99.              
               15  'XX'-ZRAM                      PIC  X(005).                  
               15  'XX'-ZRVM                      PIC  X(005).                  
               15  'XX'-SOLLZINS                  PIC  S9(02)V9999.             
               15  'XX'-UEBERZZINS                PIC  S9(02)V9999.             
               15  'XX'-HABENZINS                 PIC  S9(02)V9999.             
               15  'XX'-RESERVE                   PIC  X(101).               
      *            RESERVE                                                      
      *                                                                         
      ******************************************************************        
      *                                                                *        
      *          NACHSATZ                                              *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
           10  'XX'-NACHSATZ  REDEFINES     'XX'-VORSATZ.                       
               15  'XX'-N-SA                      PIC  X(001).                  
      *            SATZART 'E'                                                  
               15  'XX'-N-ANZ                     PIC  9(007).                  
      *            ANZAHL SAETZE INCL. VORSATZ                                  
               15  FILLER                         PIC  X(247).               
      *****************************************************************         
      *---------------------- ENDE XXXX0000 --------------------------*         
      *****************************************************************         
